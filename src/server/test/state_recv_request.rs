use crate::server::{RecvRequestResult, Reply};

#[test]
fn parse_request() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    let input = b"GET /path HTTP/1.1\r\nhost: example.com\r\n\r\n";
    let (input_used, request) = reply.try_request(input).unwrap();

    assert_eq!(input_used, 41);
    let request = request.unwrap();
    assert_eq!(request.uri().path(), "/path");
    assert_eq!(request.method(), "GET");
    assert_eq!(request.headers().get("host").unwrap(), "example.com");
}

#[test]
fn incomplete_request() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    // Incomplete request (missing final \r\n)
    let input = b"GET /path HTTP/1.1\r\nhost: example.com\r\n";
    let (input_used, request) = reply.try_request(input).unwrap();

    // Should not consume any input and return None
    assert_eq!(input_used, 0);
    assert!(request.is_none());
}

#[test]
fn malformed_request() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    // Malformed request (invalid HTTP version)
    let input = b"GET /path INVALID/1.1\r\nhost: example.com\r\n\r\n";
    let result = reply.try_request(input);

    assert!(result.is_err());
}

#[test]
fn proceed_to_send_100() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    // Request with Expect: 100-continue
    let input = b"POST /path HTTP/1.1\r\nhost: example.com\r\nexpect: 100-continue\r\n\r\n";
    let (_, request) = reply.try_request(input).unwrap();
    assert!(request.is_some());

    // Should proceed to Send100
    match reply.proceed().unwrap() {
        RecvRequestResult::Send100(_) => {}
        _ => panic!("Expected Send100 state"),
    }
}

#[test]
fn proceed_to_recv_body() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    // POST request without Expect: 100-continue
    let input = b"POST /path HTTP/1.1\r\nhost: example.com\r\n\r\n";
    let (_, request) = reply.try_request(input).unwrap();
    assert!(request.is_some());

    // Should proceed to RecvBody
    match reply.proceed().unwrap() {
        RecvRequestResult::RecvBody(_) => {}
        _ => panic!("Expected RecvBody state"),
    }
}

#[test]
fn proceed_to_provide_response() {
    // Create a new Reply directly
    let mut reply = Reply::new().unwrap();

    // GET request (no body)
    let input = b"GET /path HTTP/1.1\r\nhost: example.com\r\n\r\n";
    let (_, request) = reply.try_request(input).unwrap();
    assert!(request.is_some());

    // Should proceed to ProvideResponse
    match reply.proceed().unwrap() {
        RecvRequestResult::ProvideResponse(_) => {}
        _ => panic!("Expected ProvideResponse state"),
    }
}

#[test]
fn cannot_proceed_without_request() {
    // Create a new Reply directly
    let reply = Reply::new().unwrap();

    // No request received yet
    assert!(!reply.can_proceed());
    assert!(reply.proceed().is_none());
}
