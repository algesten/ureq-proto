use super::scenario::Scenario;

#[test]
fn proceed_without_amended_headers() {
    let scenario = Scenario::builder().get("https://q.test").build();

    let flow = scenario.to_prepare();

    let inner = flow.inner();
    let request = &inner.request;

    assert_eq!(request.headers_vec(), []);

    flow.proceed();
}

#[test]
fn proceed_with_amended_headers() {
    let scenario = Scenario::builder().get("https://q.test").build();

    let mut flow = scenario.to_prepare();

    flow.header("Cookie", "name=bar").unwrap();
    flow.header("Cookie", "name2=baz").unwrap();

    let inner = flow.inner();
    let request = &inner.request;

    assert_eq!(
        request.headers_vec(),
        [
            //
            ("cookie", "name=bar"),
            ("cookie", "name2=baz"),
        ]
    );

    flow.proceed();
}
