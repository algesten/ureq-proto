use crate::client::results::Await100Result;

use super::scenario::Scenario;

#[test]
fn proceed_without_100_continue() {
    let scenario = Scenario::builder()
        .put("https://q.test")
        .header("expect", "100-continue")
        .build();

    let state = scenario.to_await_100();

    assert!(state.can_keep_await_100());

    let inner = state.inner();
    assert!(inner.should_send_body);
    assert!(inner.close_reason.is_empty());

    match state.proceed() {
        Await100Result::SendBody(_) => {}
        _ => panic!("proceed without 100-continue should go to SendBody"),
    }
}

#[test]
fn proceed_after_100_continue() {
    let scenario = Scenario::builder()
        .put("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut state = scenario.to_await_100();

    let input = b"HTTP/1.1 100 Continue\r\n\r\n";
    let n = state.try_read_100(input).unwrap();
    assert_eq!(n, 25);

    assert!(!state.can_keep_await_100());

    let inner = state.inner();
    assert!(inner.should_send_body);
    assert!(inner.close_reason.is_empty());

    match state.proceed() {
        Await100Result::SendBody(_) => {}
        _ => panic!("proceed after 100-continue should go to SendBody"),
    }
}

#[test]
fn proceed_after_403() {
    let scenario = Scenario::builder()
        .put("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut state = scenario.to_await_100();

    let input = b"HTTP/1.1 403 Forbidden\r\n\r\n";
    let n = state.try_read_100(input).unwrap();
    assert_eq!(n, 0);

    assert!(!state.can_keep_await_100());

    let inner = state.inner();
    assert!(!inner.should_send_body);
    assert!(!inner.close_reason.is_empty());

    match state.proceed() {
        Await100Result::RecvResponse(_) => {}
        _ => panic!("proceed after 403 should go to RecvResponse"),
    }
}

#[test]
fn proceed_after_200() {
    let scenario = Scenario::builder()
        .put("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut state = scenario.to_await_100();

    let input = b"HTTP/1.1 200 Ok\r\n\r\n";
    let n = state.try_read_100(input).unwrap();
    assert_eq!(n, 0);

    assert!(!state.can_keep_await_100());

    let inner = state.inner();
    assert!(!inner.should_send_body);
    assert!(!inner.close_reason.is_empty());

    match state.proceed() {
        Await100Result::RecvResponse(_) => {}
        _ => panic!("proceed after 200 should go to RecvResponse"),
    }
}

#[test]
fn proceed_after_403_with_headers() {
    let scenario = Scenario::builder()
        .put("https://q.test")
        .header("expect", "100-continue")
        .build();

    let mut state = scenario.to_await_100();

    let input = b"HTTP/1.1 403 Forbidden\r\nContent-Length: 100\r\n";
    let n = state.try_read_100(input).unwrap();
    assert_eq!(n, 0);

    assert!(!state.can_keep_await_100());

    let inner = state.inner();
    assert!(!inner.should_send_body);
    assert!(!inner.close_reason.is_empty());

    match state.proceed() {
        Await100Result::RecvResponse(_) => {}
        _ => panic!("proceed after 403 should go to RecvResponse"),
    }
}