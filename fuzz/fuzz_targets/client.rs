#![no_main]

use libfuzzer_sys::fuzz_target;
use ureq_proto::client::{
    Await100Result, Call, RecvBodyResult, RecvResponseResult, SendRequestResult,
};
use ureq_proto::http::{Method, Request, Version};

// List of HTTP methods to randomly choose from
const METHODS: &[&str] = &["GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS", "PATCH"];

// List of relevant request headers that drive the Call API logic
const RELEVANT_REQUEST_HEADERS: &[(&str, &[&str])] = &[
    // Header name, possible values
    ("content-length", &["10", "100", "1000"]),
    ("transfer-encoding", &["chunked"]),
    ("expect", &["100-continue"]),
    ("connection", &["close", "keep-alive"]),
    ("host", &["example.com", "test.org", "localhost"]),
    ("authorization", &["Basic dXNlcjpwYXNz", "Bearer token123"]),
];

// List of relevant response headers that drive the Call API logic
const RELEVANT_RESPONSE_HEADERS: &[(&str, &[&str])] = &[
    // Header name, possible values
    ("content-length", &["5", "10", "100"]),
    ("transfer-encoding", &["chunked"]),
    ("connection", &["close", "keep-alive"]),
    (
        "location",
        &[
            "http://example.com/redirect",
            "/relative/path",
            "../parent/path",
        ],
    ),
    ("set-cookie", &["session=123", "user=test"]),
    (
        "content-type",
        &["text/plain", "application/json", "text/html"],
    ),
];

// List of status codes to randomly choose from
const STATUS_CODES: &[u16] = &[
    200, 201, 204, // Success
    301, 302, 303, 307, 308, // Redirects
    400, 401, 403, 404, // Client errors
    500, 502, 503, // Server errors
];

fuzz_target!(|data: &[u8]| {
    // Ensure we have enough data to work with
    if data.len() < 8 {
        return;
    }

    // Use the first byte to select a method
    let method_idx = (data[0] as usize) % METHODS.len();
    let method_str = METHODS[method_idx];
    let method = Method::from_bytes(method_str.as_bytes()).unwrap();

    // Create a basic request
    let mut request_builder = Request::builder()
        .method(method)
        .uri("http://example.com/test")
        .version(Version::HTTP_11);

    // Use the second byte to determine how many headers to add
    let header_count = ((data[1] as usize) % 5) + 1; // 1 to 5 headers

    // Add random headers from the relevant headers list
    for i in 0..header_count {
        if i + 2 >= data.len() {
            break;
        }

        let header_idx = (data[i + 2] as usize) % RELEVANT_REQUEST_HEADERS.len();
        let (header_name, header_values) = RELEVANT_REQUEST_HEADERS[header_idx];

        let value_idx = (data[i + 3] as usize) % header_values.len();
        let header_value = header_values[value_idx];

        request_builder = request_builder.header(header_name, header_value);
    }

    // Build the request
    let request = match request_builder.body(()) {
        Ok(req) => req,
        Err(_) => return, // Skip invalid requests
    };

    // Create a Call from the request
    let call = match Call::new(request) {
        Ok(call) => call,
        Err(_) => return, // Skip if Call creation fails
    };

    // Proceed to SendRequest state
    let mut call = call.proceed();

    // Create a buffer for writing the request
    let mut output = vec![0u8; 4096];

    // Write the request headers
    match call.write(&mut output) {
        Ok(_) => {}
        Err(_) => return, // Skip if writing fails
    }

    // Check if we can proceed
    if !call.can_proceed() {
        return; // Skip if we can't proceed
    }

    // Proceed to the next state
    let next_call = match call.proceed() {
        Ok(Some(next)) => next,
        _ => return, // Skip if proceeding fails
    };

    // Handle the different possible next states
    match next_call {
        SendRequestResult::Await100(call) => {
            // Simulate a 100 Continue response
            handle_await_100(call, &mut output, data);
        }
        SendRequestResult::SendBody(call) => {
            // Send a body
            handle_send_body(call, &mut output, data);
        }
        SendRequestResult::RecvResponse(call) => {
            // Simulate a response
            handle_recv_response(call, &mut output, data);
        }
    }
});

// Helper function to handle the Await100 state with randomized responses
fn handle_await_100<B>(
    mut call: Call<B, ureq_proto::client::state::Await100>,
    output: &mut [u8],
    data: &[u8],
) {
    // Use a byte from the fuzz data to decide whether to send a 100 Continue response
    let send_100 = data.len() > 6 && (data[6] % 2 == 0);

    if send_100 {
        // Create a randomized 100 Continue response
        let mut response = String::from("HTTP/1.1 100 Continue\r\n");

        // Randomize whether to include headers (which is technically invalid but good for fuzzing)
        let include_headers = data.len() > 7 && (data[7] % 5 == 0); // 20% chance

        if include_headers {
            // Add a random header (100 Continue shouldn't have headers, but we're fuzzing)
            let header_idx = (data[7] as usize) % RELEVANT_RESPONSE_HEADERS.len();
            let (header_name, header_values) = RELEVANT_RESPONSE_HEADERS[header_idx];
            let value_idx = (data[7] as usize) % header_values.len();
            let header_value = header_values[value_idx];

            response.push_str(&format!("{}: {}\r\n", header_name, header_value));
        }

        response.push_str("\r\n");

        // Try to read the 100 Continue response
        match call.try_read_100(response.as_bytes()) {
            Ok(_) => {}
            Err(_) => return, // Skip if reading fails
        }
    }

    // Proceed to the next state
    match call.proceed() {
        Ok(Await100Result::SendBody(call)) => {
            // Handle the send body state
            handle_send_body(call, output, data);
        }
        Ok(Await100Result::RecvResponse(call)) => {
            // Handle the response directly
            handle_recv_response(call, output, data);
        }
        Err(_) => return, // Skip if proceeding fails
    }
}

// Helper function to handle the SendBody state
fn handle_send_body<B>(
    mut call: Call<B, ureq_proto::client::state::SendBody>,
    output: &mut [u8],
    data: &[u8],
) {
    // Use some of the fuzz data as the body
    let body_data = if data.len() > 10 {
        &data[5..10]
    } else {
        b"test"
    };

    // Write the body
    match call.write(body_data, output) {
        Ok(_) => {}
        Err(_) => return, // Skip if writing fails
    }

    // Write an empty chunk to signal the end of the body
    match call.write(&[], &mut output[body_data.len()..]) {
        Ok(_) => {}
        Err(_) => return, // Skip if writing fails
    }

    // Check if we can proceed
    if !call.can_proceed() {
        return; // Skip if we can't proceed
    }

    // Proceed to RecvResponse
    match call.proceed() {
        Some(call) => {
            // Handle the response
            handle_recv_response(call, output, data);
        }
        None => return, // Skip if proceeding fails
    }
}

// Helper function to handle the RecvResponse state with randomized responses
fn handle_recv_response<B>(
    mut call: Call<B, ureq_proto::client::state::RecvResponse>,
    output: &mut [u8],
    data: &[u8],
) {
    // Randomize the status code
    let status_code = if data.len() > 11 {
        let idx = (data[11] as usize) % STATUS_CODES.len();
        STATUS_CODES[idx]
    } else {
        200
    };

    // Start building the response
    let mut response = format!("HTTP/1.1 {} OK\r\n", status_code);

    // Randomize the number of headers
    let header_count = if data.len() > 12 {
        (data[12] as usize) % 5 // 0 to 4 headers
    } else {
        1 // Default to 1 header
    };

    // Add random headers
    for i in 0..header_count {
        if data.len() <= 13 + i {
            break;
        }

        let header_idx = (data[13 + i] as usize) % RELEVANT_RESPONSE_HEADERS.len();
        let (header_name, header_values) = RELEVANT_RESPONSE_HEADERS[header_idx];

        let value_idx = if data.len() > 14 + i {
            (data[14 + i] as usize) % header_values.len()
        } else {
            0
        };

        let header_value = header_values[value_idx];

        response.push_str(&format!("{}: {}\r\n", header_name, header_value));
    }

    // End headers section
    response.push_str("\r\n");

    // Try to parse the response
    match call.try_response(response.as_bytes(), false) {
        Ok(_) => {}
        Err(_) => return, // Skip if parsing fails
    }

    // Proceed to the next state
    let next_call = match call.proceed() {
        Some(next) => next,
        None => return, // Skip if proceeding fails
    };

    // Handle the different possible next states
    match next_call {
        RecvResponseResult::RecvBody(mut call) => {
            // Create a response body as a Vec<u8> to avoid type mismatches
            let body = if response.contains("content-length: 5")
                || response.contains("Content-Length: 5")
            {
                b"hello".to_vec()
            } else {
                // For other content lengths or chunked encoding, use a generic body
                b"hello world this is a test body".to_vec()
            };

            // Read the response body
            match call.read(&body, output) {
                Ok(_) => {}
                Err(_) => return, // Skip if reading fails
            }

            // Proceed to the next state
            let next_call = match call.proceed() {
                Some(RecvBodyResult::Cleanup(call)) => call,
                Some(RecvBodyResult::Redirect(_)) => {
                    // We don't want to follow redirects, so we're done
                    return;
                }
                None => return, // Skip if proceeding fails
            };

            // Check if we need to close the connection
            let _must_close = next_call.must_close_connection();
            // In a real client, we would close the connection if must_close is true
        }
        RecvResponseResult::Redirect(_) => {
            // We don't want to follow redirects, so we're done
            return;
        }
        RecvResponseResult::Cleanup(call) => {
            // Check if we need to close the connection
            let _must_close = call.must_close_connection();
            // In a real client, we would close the connection if must_close is true
        }
    }
}
