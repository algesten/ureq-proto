use http::Response;

use crate::client::test::TestSliceExt;
use crate::client::CloseReason;

use super::scenario::Scenario;

#[test]
fn recv_body_close_delimited() {
    let scenario = Scenario::builder().get("https://q.test").build();

    let mut flow = scenario.to_recv_body();

    let mut output = vec![0; 1024];

    assert!(flow.can_proceed());

    let (input_used, output_used) = flow.read(b"hello", &mut output).unwrap();
    assert_eq!(input_used, 5);
    assert_eq!(output_used, 5);

    let inner = flow.inner();
    let reason = *inner.close_reason.first().unwrap();

    assert_eq!(reason, CloseReason::CloseDelimitedBody);
    assert_eq!(output[..output_used].as_str(), "hello");
    assert!(flow.can_proceed());
}

#[test]
fn recv_body_chunked_partial() {
    let scenario = Scenario::builder()
        .get("https://q.test")
        .response(
            Response::builder()
                .header("transfer-encoding", "chunked")
                .body(())
                .unwrap(),
        )
        .build();

    let mut flow = scenario.to_recv_body();

    let mut output = vec![0; 1024];

    let (input_used, output_used) = flow.read(b"5\r", &mut output).unwrap();
    assert_eq!(input_used, 0);
    assert_eq!(output_used, 0);
    assert!(!flow.can_proceed());

    let (input_used, output_used) = flow.read(b"5\r\nhel", &mut output).unwrap();
    assert_eq!(input_used, 6);
    assert_eq!(output_used, 3);
    assert!(!flow.can_proceed());

    let (input_used, output_used) = flow.read(b"lo", &mut output).unwrap();
    assert_eq!(input_used, 2);
    assert_eq!(output_used, 2);
    assert!(!flow.can_proceed());

    let (input_used, output_used) = flow.read(b"\r\n", &mut output).unwrap();
    assert_eq!(input_used, 2);
    assert_eq!(output_used, 0);
    assert!(!flow.can_proceed());

    let (input_used, output_used) = flow.read(b"0\r\n\r\n", &mut output).unwrap();
    assert_eq!(input_used, 5);
    assert_eq!(output_used, 0);
    assert!(flow.can_proceed());
}

#[test]
fn recv_body_chunked_full() {
    let scenario = Scenario::builder()
        .get("https://q.test")
        .response(
            Response::builder()
                .header("transfer-encoding", "chunked")
                .body(())
                .unwrap(),
        )
        .build();

    let mut flow = scenario.to_recv_body();

    let mut output = vec![0; 1024];

    // this is the default
    // flow.stop_on_chunk_boundary(false);

    let (input_used, output_used) = flow.read(b"5\r\nhello\r\n0\r\n\r\n", &mut output).unwrap();
    assert_eq!(input_used, 15);
    assert_eq!(output_used, 5);
    assert_eq!(output[..output_used].as_str(), "hello");
    assert!(flow.can_proceed());
}

#[test]
fn recv_body_chunked_stop_boundary() {
    let scenario = Scenario::builder()
        .get("https://q.test")
        .response(
            Response::builder()
                .header("transfer-encoding", "chunked")
                .body(())
                .unwrap(),
        )
        .build();

    let mut flow = scenario.to_recv_body();

    let mut output = vec![0; 1024];

    flow.stop_on_chunk_boundary(true);

    // chunk reading starts on boundary.
    assert!(flow.is_on_chunk_boundary());

    let (input_used, output_used) = flow.read(b"5\r\nhello\r\n0\r\n\r\n", &mut output).unwrap();
    assert_eq!(input_used, 10);
    assert_eq!(output_used, 5);
    assert_eq!(output[..output_used].as_str(), "hello");

    // chunk reading stops on chunk boundary.
    assert!(flow.is_on_chunk_boundary());

    let (input_used, output_used) = flow.read(b"0\r\n\r\n", &mut output).unwrap();
    assert_eq!(input_used, 5);
    assert_eq!(output_used, 0);
    assert!(flow.can_proceed());
}

#[test]
fn recv_body_content_length() {
    let scenario = Scenario::builder()
        .get("https://q.test")
        .response(
            Response::builder()
                .header("content-length", "5")
                .body(())
                .unwrap(),
        )
        .build();

    let mut flow = scenario.to_recv_body();

    let mut output = vec![0; 1024];

    let (input_used, output_used) = flow.read(b"hel", &mut output).unwrap();
    assert_eq!(input_used, 3);
    assert_eq!(output_used, 3);
    assert_eq!(output[..output_used].as_str(), "hel");
    assert!(!flow.can_proceed());

    let (input_used, output_used) = flow.read(b"lo", &mut output).unwrap();
    assert_eq!(input_used, 2);
    assert_eq!(output_used, 2);
    assert_eq!(output[..output_used].as_str(), "lo");
    assert!(flow.can_proceed());
}
