//! Responses to HTTP/1.0 requests.
//!
//! Chunked coding is HTTP/1.1 only (RFC 9112 section 7.1), so a body without
//! a Content-Length is close delimited. An HTTP/1.0 client assumes the server
//! closes unless the response says `Connection: keep-alive`
//! (RFC 9112 Appendix C.2.2).

use http::{Request, Response, Version};

use crate::server::state::Cleanup;
use crate::server::{Reply, SendResponseResult};

use super::scenario::Scenario;

struct Exchange {
    head: String,
    body: Vec<u8>,
    reply: Reply<Cleanup>,
}

/// Run a request/response pair through SendResponse and SendBody, collecting
/// the bytes written for the head and the body.
fn exchange(request: Request<()>, response: Response<()>, body: &[u8]) -> Exchange {
    let scenario = Scenario::builder()
        .request(request)
        .response(response)
        .build();

    let mut reply = scenario.to_send_response();
    let mut output = vec![0; 1024];
    let n = reply.write(&mut output).unwrap();
    let head = String::from_utf8(output[..n].to_vec()).unwrap();
    assert!(reply.is_finished());

    let mut reply = match reply.proceed() {
        SendResponseResult::SendBody(reply) => reply,
        SendResponseResult::Cleanup(reply) => {
            assert!(body.is_empty(), "no SendBody state to send the body in");
            return Exchange {
                head,
                body: vec![],
                reply,
            };
        }
    };

    let mut written = Vec::new();
    if !body.is_empty() {
        let (input_used, output_used) = reply.write(body, &mut output).unwrap();
        assert_eq!(input_used, body.len());
        written.extend_from_slice(&output[..output_used]);
    }
    let (input_used, output_used) = reply.write(&[], &mut output).unwrap();
    assert_eq!(input_used, 0);
    written.extend_from_slice(&output[..output_used]);
    assert!(reply.is_finished());

    Exchange {
        head,
        body: written,
        reply: reply.proceed(),
    }
}

fn get10(connection: Option<&str>) -> Request<()> {
    let mut builder = Request::get("/path").version(Version::HTTP_10);
    if let Some(v) = connection {
        builder = builder.header("connection", v);
    }
    builder.body(()).unwrap()
}

#[test]
fn without_length_is_close_delimited() {
    let response = Response::builder().status(200).body(()).unwrap();
    let ex = exchange(get10(None), response, b"hello");

    assert_eq!(ex.head, "HTTP/1.0 200 OK\r\n\r\n");
    assert_eq!(ex.body, b"hello");
    assert!(ex.reply.must_close_connection());
    assert_eq!(
        ex.reply.close_reason(),
        Some("response body is close delimited")
    );
}

#[test]
fn keep_alive_without_length_closes() {
    let response = Response::builder().status(200).body(()).unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert_eq!(ex.head, "HTTP/1.0 200 OK\r\n\r\n");
    assert_eq!(ex.body, b"hello");
    assert!(ex.reply.must_close_connection());
    assert_eq!(
        ex.reply.close_reason(),
        Some("response body is close delimited")
    );
}

#[test]
fn keep_alive_with_content_length_stays_open() {
    let response = Response::builder()
        .status(200)
        .header("content-length", "5")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert_eq!(
        ex.head,
        "HTTP/1.0 200 OK\r\n\
        connection: keep-alive\r\n\
        content-length: 5\r\n\
        \r\n"
    );
    assert_eq!(ex.body, b"hello");
    assert!(!ex.reply.must_close_connection());
}

#[test]
fn keep_alive_with_server_close() {
    let response = Response::builder()
        .status(200)
        .header("connection", "close")
        .header("content-length", "5")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert_eq!(
        ex.head,
        "HTTP/1.0 200 OK\r\n\
        connection: close\r\n\
        content-length: 5\r\n\
        \r\n"
    );
    assert!(ex.reply.must_close_connection());
}

#[test]
fn keep_alive_header_already_present_is_not_repeated() {
    let response = Response::builder()
        .status(200)
        .header("connection", "Keep-Alive")
        .header("content-length", "5")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert_eq!(
        ex.head,
        "HTTP/1.0 200 OK\r\n\
        connection: Keep-Alive\r\n\
        content-length: 5\r\n\
        \r\n"
    );
    assert!(!ex.reply.must_close_connection());
}

#[test]
fn keep_alive_head_without_length_stays_open() {
    let request = Request::head("/path")
        .version(Version::HTTP_10)
        .header("connection", "keep-alive")
        .body(())
        .unwrap();
    let response = Response::builder().status(200).body(()).unwrap();
    let ex = exchange(request, response, b"");

    assert_eq!(
        ex.head,
        "HTTP/1.0 200 OK\r\n\
        connection: keep-alive\r\n\
        \r\n"
    );
    assert!(ex.body.is_empty());
    assert!(!ex.reply.must_close_connection());
}

#[test]
fn keep_alive_204_stays_open() {
    let response = Response::builder().status(204).body(()).unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"");

    assert_eq!(
        ex.head,
        "HTTP/1.0 204 No Content\r\n\
        connection: keep-alive\r\n\
        \r\n"
    );
    assert!(ex.body.is_empty());
    assert!(!ex.reply.must_close_connection());
}

#[test]
fn caller_transfer_encoding_is_dropped() {
    let response = Response::builder()
        .status(200)
        .header("transfer-encoding", "chunked")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert_eq!(ex.head, "HTTP/1.0 200 OK\r\n\r\n");
    assert_eq!(ex.body, b"hello");
    assert!(ex.reply.must_close_connection());
}

#[test]
fn close_delimited_direct_write() {
    let scenario = Scenario::builder()
        .request(get10(None))
        .response(Response::builder().status(200).body(()).unwrap())
        .build();
    let mut reply = scenario.to_send_body();

    assert!(!reply.is_chunked());
    assert_eq!(reply.calculate_max_input(123), 123);

    reply.consume_direct_write(1_000_000).unwrap();
    assert!(!reply.is_finished());

    let mut output = vec![0; 16];
    assert_eq!(reply.write(&[], &mut output).unwrap(), (0, 0));
    assert!(reply.is_finished());

    let reply = reply.proceed();
    assert!(reply.must_close_connection());
}

#[test]
fn http11_without_length_is_still_chunked() {
    let request = Request::get("/path").body(()).unwrap();
    let response = Response::builder().status(200).body(()).unwrap();
    let ex = exchange(request, response, b"hello");

    assert_eq!(
        ex.head,
        "HTTP/1.1 200 OK\r\n\
        transfer-encoding: chunked\r\n\
        \r\n"
    );
    assert_eq!(ex.body, b"5\r\nhello\r\n0\r\n\r\n");
    assert!(!ex.reply.must_close_connection());
}

#[test]
fn keep_alive_response_connection_close_token_closes() {
    let response = Response::builder()
        .status(200)
        .header("connection", "upgrade, Close")
        .header("content-length", "5")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert!(ex.reply.must_close_connection());
    assert!(!ex.head.contains("connection: keep-alive\r\n"));
}

#[test]
fn transfer_encoding_with_content_length_is_close_delimited() {
    let response = Response::builder()
        .status(200)
        .header("transfer-encoding", "gzip")
        .header("content-length", "5")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");

    assert!(!ex.head.contains("transfer-encoding:"));
    assert!(!ex.head.contains("content-length:"));
    assert!(ex.reply.must_close_connection());
}

#[test]
fn close_delimited_response_does_not_advertise_keep_alive() {
    let response = Response::builder()
        .header("connection", "keep-alive")
        .body(())
        .unwrap();
    let ex = exchange(get10(Some("keep-alive")), response, b"hello");
    assert!(ex.reply.must_close_connection());
    assert!(!ex.head.contains("connection: keep-alive"));
}

#[test]
fn no_content_with_non_chunked_transfer_encoding_has_no_body() {
    let response = Response::builder()
        .status(204)
        .header("transfer-encoding", "gzip")
        .body(())
        .unwrap();
    let scenario = Scenario::builder()
        .request(get10(Some("keep-alive")))
        .response(response)
        .build();
    let mut reply = scenario.to_send_response();
    reply.write(&mut [0; 1024]).unwrap();
    assert!(matches!(reply.proceed(), SendResponseResult::Cleanup(_)));
}
