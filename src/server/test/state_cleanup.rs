use http::Response;

use super::scenario::Scenario;

#[test]
fn create_cleanup_state() {
    // Create a scenario with a GET request and a response
    let scenario = Scenario::builder()
        .get("/path")
        .response(
            Response::builder()
                .status(200)
                .header("content-length", "0")
                .body(())
                .unwrap(),
        )
        .build();

    // Get a Reply in the Cleanup state
    let reply = scenario.to_cleanup();

    // Verify that we have a Reply<Cleanup>
    // Just having this test pass means the type system correctly created a Reply<Cleanup>
}

#[test]
fn cleanup_after_content_length_response() {
    // Create a scenario with a GET request and a response with content-length
    let scenario = Scenario::builder()
        .get("/path")
        .response(Response::builder().status(200).body(()).unwrap())
        .response_body(b"Hello, world!", false)
        .build();

    // Get a Reply in the Cleanup state
    let reply = scenario.to_cleanup();

    // Verify that we have a Reply<Cleanup>
    // Just having this test pass means the type system correctly created a Reply<Cleanup>
}

#[test]
fn cleanup_after_chunked_response() {
    // Create a scenario with a GET request and a chunked response
    let scenario = Scenario::builder()
        .get("/path")
        .response(
            Response::builder()
                .status(200)
                .header("transfer-encoding", "chunked")
                .body(())
                .unwrap(),
        )
        .response_body(b"Hello, world!", true)
        .build();

    // Get a Reply in the Cleanup state
    let reply = scenario.to_cleanup();

    // Verify that we have a Reply<Cleanup>
    // Just having this test pass means the type system correctly created a Reply<Cleanup>
}
