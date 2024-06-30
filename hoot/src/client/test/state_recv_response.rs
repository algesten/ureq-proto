use http::{StatusCode, Version};

use crate::analyze::HeaderIterExt;
use crate::client::test::scenario::Scenario;

// This is a complete response.
const RESPONSE: &[u8] = b"\
        HTTP/1.1 200 OK\r\n\
        Content-Length: 123\r\n\
        Content-Type: text/plain\r\n\
        \r\n";

#[test]
fn receive_incomplete_response() {
    // -1 to never reach the end
    for i in 0..RESPONSE.len() - 1 {
        let scenario = Scenario::builder().get("https://q.test").build();
        let mut flow = scenario.to_recv_response();

        let (input_used, maybe_response) = flow.try_response(&RESPONSE[..i]).unwrap();
        assert_eq!(input_used, 0);
        assert!(maybe_response.is_none());
        assert!(!flow.can_proceed());
    }
}

#[test]
fn receive_complete_response() {
    let scenario = Scenario::builder().get("https://q.test").build();
    let mut flow = scenario.to_recv_response();

    let (input_used, maybe_response) = flow.try_response(&RESPONSE).unwrap();
    assert_eq!(input_used, 66);
    assert!(maybe_response.is_some());

    let response = maybe_response.unwrap();

    assert_eq!(response.version(), Version::HTTP_11);
    assert_eq!(response.status(), StatusCode::OK);
    assert_eq!(response.headers().get("content-length").unwrap(), "123");
    assert!(response.headers().iter().has("content-type", "text/plain"));

    assert!(flow.can_proceed());
}

#[test]
fn prepended_100_continue() {
    // In the case of expect-100-continue, there's a chance the 100-continue
    // arrives after we started sending the request body, in which case
    // we receive it before the actual response.
    let scenario = Scenario::builder()
        .post("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut flow = scenario.to_recv_response();

    // incomplete 100-continue should be ignored.
    let (input_used, maybe_response) = flow.try_response(b"HTTP/1.1 100 Continue\r\n").unwrap();
    assert_eq!(input_used, 0);
    assert!(maybe_response.is_none());
    assert!(!flow.can_proceed());

    // complete 100-continue should be consumed without producing a request
    let (input_used, maybe_response) = flow.try_response(b"HTTP/1.1 100 Continue\r\n\r\n").unwrap();
    assert_eq!(input_used, 25);
    assert!(maybe_response.is_none());
    assert!(!flow.can_proceed());

    // full response after prepended 100-continue
    let (input_used, maybe_response) = flow.try_response(&RESPONSE).unwrap();
    assert_eq!(input_used, 66);
    assert!(maybe_response.is_some());
    assert!(flow.can_proceed());
}

#[test]
fn expect_100_without_100_continue() {
    // In the case of expect-100-continue
    let scenario = Scenario::builder()
        .post("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut flow = scenario.to_recv_response();

    // full response and no 100-continue
    let (input_used, maybe_response) = flow.try_response(&RESPONSE).unwrap();
    assert_eq!(input_used, 66);
    assert!(maybe_response.is_some());
    assert!(flow.can_proceed());
}
