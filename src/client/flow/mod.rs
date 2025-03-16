//! HTTP/1.1 client protocol state machine implementation.
//!
//! This module implements a state machine for handling HTTP/1.1 requests and responses.
//! It provides a type-safe way to ensure that operations are performed in the correct
//! sequence according to the HTTP/1.1 protocol.
//!
//! ## State Machine
//!
//! The Call type represents a state machine that transitions through various states
//! during the lifecycle of an HTTP request/response. Each state is represented by a
//! different type parameter to `Call<B, State>`, ensuring at compile time that
//! operations are performed in the correct order.
//!
//! For a visual representation of the state transitions, see the
//! [state diagram][crate::client] in the client module documentation.
//!
//! ## States
//!
//! The state machine includes the following states:
//!
//! * **Prepare** - Initial state for preparing a request (adding headers, etc.)
//! * **SendRequest** - Sending the request line and headers
//! * **Await100** - Waiting for a 100 Continue response (if Expect: 100-continue was sent)
//! * **SendBody** - Sending the request body
//! * **RecvResponse** - Receiving the response status and headers
//! * **RecvBody** - Receiving the response body
//! * **Redirect** - Handling redirects
//! * **Cleanup** - Final state for connection cleanup
//!
//! ## Usage
//!
//! See the [example in the client module][crate::client] for a comprehensive
//! demonstration of how to use the Call API to handle an HTTP request/response cycle.
//!
//! ## Design
//!
//! The Call module is designed as a "sans-IO" implementation, meaning it doesn't
//! directly perform I/O operations. Instead, it works with buffers that the caller
//! is responsible for reading from or writing to the network. This design allows
//! for greater flexibility in how I/O is handled and makes the code easier to test.
