//! HTTP/1.1 server protocol
//!
//! Sans-IO protocol impl, which means "writing" and "reading" are made via buffers
//! rather than the Write/Read std traits.
//!
//! The [`Reply`] object attempts to encode correct HTTP/1.1 handling using
//! state variables, for example `Reply<RecvRequest>` to represent the
//! lifecycle stage where we are to receive a request.
//!
//! The states are:
//!
//! * **RecvRequest** - Receive the request, which is the method, path,
//!   version and the request headers
//! * **Send100** - If there is an `Expect: 100-continue` header, the
//!   server should send a 100 Continue response before receiving the body
//! * **RecvBody** - Receive the request body
//! * **ProvideResponse** - Prepare a response to the request
//! * **SendResponse** - Send the response status and headers
//! * **SendBody** - Send the response body
//! * **Cleanup** - Close the connection or prepare for the next request
//!
//! ```text
//!        ┌──────────────────┐
//!     ┌──│   RecvRequest    │───────────────┐
//!     │  └──────────────────┘               │
//!     │            │                        │
//!     │            │                        │
//!     │            ▼                        ▼
//!     │  ┌──────────────────┐     ┌──────────────────┐
//!     │  │     RecvBody     │◀────│     Send100      │
//!     │  └──────────────────┘     └──────────────────┘
//!     │            │                        │
//!     │            │                        │
//!     │            ▼                        │
//!     └─▶┌──────────────────┐               │
//!        │  ProvideResponse │             reject
//!        └──────────────────┘               │
//!                  │                        │
//!                  │                        │
//!                  ▼                        │
//!        ┌──────────────────┐◀──────────────┘
//!        │   SendResponse   │──┐
//!        └──────────────────┘  │
//!                  │           │
//!                  │           │
//!                  ▼           │
//!        ┌──────────────────┐  │
//!        │     SendBody     │  │
//!        └──────────────────┘  │
//!                  │           │
//!                  │           │
//!                  ▼           │
//!        ┌──────────────────┐  │
//!        │     Cleanup      │◀─┘
//!        └──────────────────┘
//! ```
//!
//! # Example
//!
//! ```
//! use ureq_proto::server::*;
//! use http::{Response, StatusCode, Version};
//!
//! // ********************************** RecvRequest
//!
//! // Create a new Reply in the RecvRequest state
//! let mut reply = Reply::new().unwrap();
//!
//! // Receive a request from the client
//! let input = b"GET /my-path HTTP/1.1\r\nhost: example.test\r\nexpect: 100-continue\r\n\r\n";
//! let (input_used, request) = reply.try_request(input).unwrap();
//!
//! assert_eq!(input_used, 67);
//! let request = request.unwrap();
//! assert_eq!(request.uri().path(), "/my-path");
//! assert_eq!(request.method(), "GET");
//!
//! // Check if we can proceed to the next state
//! // In a real server, you would implement this method
//! // let can_proceed = reply.can_proceed();
//!
//! // Proceed to the next state
//! let reply = reply.proceed().unwrap();
//!
//! // ********************************** Send100
//!
//! // In this example, we know the next state is Send100 because
//! // the request included an "Expect: 100-continue" header.
//! // A real server needs to match on the variants.
//! let reply = match reply {
//!     RecvRequestResult::Send100(v) => v,
//!     _ => panic!(),
//! };
//!
//! // We can either accept or reject the 100-continue request
//! // Here we accept it and proceed to receiving the body
//! let mut output = vec![0_u8; 1024];
//! let (output_used, reply) = reply.accept(&mut output).unwrap();
//!
//! assert_eq!(output_used, 25);
//! assert_eq!(&output[..output_used], b"HTTP/1.1 100 Continue\r\n\r\n");
//!
//! // ********************************** RecvBody
//!
//! // Now we can receive the request body
//! let mut reply = reply;
//!
//! // Receive the body in chunks
//! let input = b"hello";
//! let mut body_buffer = vec![0_u8; 1024];
//! let (input_used, output_used) = reply.read(input, &mut body_buffer).unwrap();
//!
//! assert_eq!(input_used, 5);
//! assert_eq!(output_used, 5);
//! assert_eq!(&body_buffer[..output_used], b"hello");
//!
//! // Check if the body is fully received
//! // In this example, we'll assume it is
//! assert!(reply.is_ended());
//!
//! // Proceed to providing a response
//! let reply = reply.proceed().unwrap();
//!
//! // ********************************** ProvideResponse
//!
//! // Create a response
//! let response = Response::builder()
//!     .status(StatusCode::OK)
//!     .header("content-type", "text/plain")
//!     .body(())
//!     .unwrap();
//!
//! // Provide the response and proceed to sending it
//! let mut reply = reply.provide(response).unwrap();
//!
//! // ********************************** SendResponse
//!
//! // Send the response headers
//! let output_used = reply.write(&mut output).unwrap();
//!
//! assert_eq!(&output[..output_used], b"\
//!     HTTP/1.1 200 OK\r\n\
//!     content-type: text/plain\r\n\
//!     transfer-encoding: chunked\r\n\
//!     \r\n");
//!
//! // Check if the response headers are fully sent
//! assert!(reply.is_finished());
//!
//! // Proceed to sending the response body
//! let mut reply = reply.proceed();
//!
//! // ********************************** SendBody
//!
//! // Send the response body
//! let (input_used, output_used) = reply.write(b"hello world", &mut output).unwrap();
//!
//! assert_eq!(input_used, 11);
//! assert_eq!(&output[..output_used], b"b\r\nhello world\r\n");
//!
//! // Indicate the end of the body with an empty input
//! let (input_used, output_used) = reply.write(&[], &mut output).unwrap();
//!
//! assert_eq!(input_used, 0);
//! assert_eq!(&output[..output_used], b"0\r\n\r\n");
//!
//! // Check if the body is fully sent
//! assert!(reply.is_finished());
//!
//! // ********************************** Cleanup
//!
//! // Proceed to cleanup
//! let reply = reply.proceed();
//!
//! // Check if we need to close the connection
//! if reply.must_close_connection() {
//!     // connection.close();
//! } else {
//!     // Prepare for the next request
//!     // let new_reply = Reply::new().unwrap();
//! }
//! ```
//!
//! # In scope:
//!
//! * First class HTTP/1.1 protocol implementation
//! * Indication of connection states (such as when a connection must be closed)
//! * transfer-encoding: chunked
//! * 100-continue handling
//!
//! # Out of scope:
//!
//! * Opening/closing sockets
//! * TLS (https)
//! * Request routing
//! * Body data transformations (charset, compression etc)
//!
//! # The http crate
//!
//! Based on the [http crate](https://crates.io/crates/http) - a unified HTTP API for Rust.
use std::fmt;
use std::io::Write;
use std::marker::PhantomData;

use amended::AmendedResponse;
use http::{Method, Response, StatusCode, Version};

use crate::body::{BodyReader, BodyWriter};
use crate::ext::StatusCodeExt;
use crate::util::Writer;
use crate::{ArrayVec, CloseReason};

mod amended;

/// Maximum number of headers to parse from an HTTP request.
///
/// This constant defines the upper limit on the number of headers that can be
/// parsed from an incoming HTTP request. Requests with more headers than this
/// will be rejected.
pub const MAX_REQUEST_HEADERS: usize = 128;

/// A state machine for an HTTP request/response cycle.
///
/// This type represents a state machine that transitions through various
/// states during the lifecycle of an HTTP request/response.
///
/// The type parameters are:
/// - `State`: The current state of the state machine (e.g., `RecvRequest`, `SendResponse`, etc.)
/// - `B`: The type of the response body (defaults to `()`)
///
/// See the [state graph][crate::server] in the server module documentation for a
/// visual representation of the state transitions.
pub struct Reply<State> {
    inner: Inner,
    _ph: PhantomData<State>,
}

// pub(crate) for tests to inspect state
#[derive(Debug)]
pub(crate) struct Inner {
    pub phase: ResponsePhase,
    pub state: BodyState,
    pub response: Option<AmendedResponse>,
    pub close_reason: ArrayVec<CloseReason, 4>,
    pub method: Option<Method>,
    pub expect_100: bool,
    pub expect_100_reject: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResponsePhase {
    Status,
    Headers(usize),
    Body,
}

impl ResponsePhase {
    fn is_prelude(&self) -> bool {
        matches!(self, ResponsePhase::Status | ResponsePhase::Headers(_))
    }

    fn is_body(&self) -> bool {
        matches!(self, ResponsePhase::Body)
    }
}

#[derive(Debug, Default)]
pub(crate) struct BodyState {
    reader: Option<BodyReader>,
    writer: Option<BodyWriter>,
    stop_on_chunk_boundary: bool,
}
#[doc(hidden)]
pub mod state {
    pub(crate) trait Named {
        fn name() -> &'static str;
    }

    macro_rules! reply_state {
        ($n:tt) => {
            #[doc(hidden)]
            pub struct $n(());
            impl Named for $n {
                fn name() -> &'static str {
                    stringify!($n)
                }
            }
        };
    }

    reply_state!(RecvRequest);
    reply_state!(Send100);
    reply_state!(RecvBody);
    reply_state!(ProvideResponse);
    reply_state!(SendResponse);
    reply_state!(SendBody);
    reply_state!(Cleanup);
}
use self::state::*;

impl<S> Reply<S> {
    fn wrap(inner: Inner) -> Reply<S>
    where
        S: Named,
    {
        let wrapped = Reply {
            inner,
            _ph: PhantomData,
        };

        debug!("{:?}", wrapped);

        wrapped
    }

    #[cfg(test)]
    pub(crate) fn inner(&self) -> &Inner {
        &self.inner
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// RECV REQUEST

mod recvreq;

/// The possible states after receiving a request.
///
/// See [state graph][crate::server]
pub enum RecvRequestResult {
    /// Client is expecting a 100-continue response.
    Send100(Reply<Send100>),
    /// Receive a request body.
    RecvBody(Reply<RecvBody>),
    /// Client did not send a body.
    ProvideResponse(Reply<ProvideResponse>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// SEND 100

mod send100;

/// Internal function to append a response to an existing inner state.
///
/// This function is used when transitioning from a state that has received a request
/// to a state that will send a response.
fn append_request(inner: Inner, response: Response<()>) -> Inner {
    let default_body_mode = if response.status().body_allowed() {
        BodyWriter::new_chunked()
    } else {
        BodyWriter::new_none()
    };

    Inner {
        phase: inner.phase,
        state: BodyState {
            writer: Some(default_body_mode),
            ..inner.state
        },
        response: Some(AmendedResponse::new(response)),
        close_reason: inner.close_reason,
        method: inner.method,
        expect_100: inner.expect_100,
        expect_100_reject: inner.expect_100_reject,
    }
}

/// Internal function to write a status line to a writer.
///
/// This function is used when sending a response status line.
fn do_write_send_line(line: (Version, StatusCode), w: &mut Writer, end_head: bool) -> bool {
    w.try_write(|w| {
        write!(
            w,
            "{:?} {} {}\r\n{}",
            line.0,
            line.1.as_str(),
            line.1.canonical_reason().unwrap_or("Unknown"),
            if end_head { "\r\n" } else { "" }
        )
    })
}
// //////////////////////////////////////////////////////////////////////////////////////////// RECV BODY

mod provres;

// //////////////////////////////////////////////////////////////////////////////////////////// RECV BODY

mod recvbody;

// //////////////////////////////////////////////////////////////////////////////////////////// SEND RESPONSE

mod sendres;

// //////////////////////////////////////////////////////////////////////////////////////////// SEND RESPONSE

mod sendbody;

// //////////////////////////////////////////////////////////////////////////////////////////// CLEANUP

impl Reply<Cleanup> {
    /// Tell if we must close the connection.
    pub fn must_close_connection(&self) -> bool {
        self.close_reason().is_some()
    }

    /// If we are closing the connection, give a reason.
    pub fn close_reason(&self) -> Option<&'static str> {
        self.inner.close_reason.first().map(|s| s.explain())
    }
}

// ////////////////////////////////////////////////////////////////////////////////////////////

impl<State: Named> fmt::Debug for Reply<State> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Reply<{}>", State::name())
    }
}

impl fmt::Debug for ResponsePhase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResponsePhase::Status => write!(f, "SendStatus"),
            ResponsePhase::Headers(_) => write!(f, "SendHeaders"),
            ResponsePhase::Body => write!(f, "SendBody"),
        }
    }
}
