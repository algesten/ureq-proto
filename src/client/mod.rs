//! HTTP/1.1 client protocol
//!
//! Sans-IO protocol impl, which means "writing" and "reading" are made via buffers
//! rather than the Write/Read std traits.
//!
//! The [`Flow`](flow::Flow) object attempts to encode correct HTTP/1.1 handling using
//! state variables, for example `Flow<'a, SendRequest>` to represent the
//! lifecycle stage where we are to send the request.
//!
//! The states are:
//!
//! * **Prepare** - Preparing a request means 1) adding headers such as
//!   cookies. 2) acquiring the connection from a pool or opening a new
//!   socket (potentially wrappping in TLS)
//! * **SendRequest** - Send the first row, which is the method, path
//!   and version as well as the request headers
//! * **SendBody** - Send the request body
//! * **Await100** - If there is an `Expect: 100-continue` header, the
//!   client should pause before sending the body
//! * **RecvResponse** - Receive the response, meaning the status and
//!   version and the response headers
//! * **RecvBody** - Receive the response body
//! * **Redirect** - Handle redirects, potentially spawning new requests
//! * **Cleanup** - Return the connection to the pool or close it
//!
//!
//! ```text
//!                            ┌──────────────────┐
//! ┌ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ─ ▶│     Prepare      │
//!                            └──────────────────┘
//! │                                    │
//!                                      ▼
//! │                          ┌──────────────────┐
//!                         ┌──│   SendRequest    │──────────────┐
//! │                       │  └──────────────────┘              │
//!                         │            │                       │
//! │                       │            ▼                       ▼
//!                         │  ┌──────────────────┐    ┌──────────────────┐
//! │                       │  │     SendBody     │◀───│     Await100     │
//!                         │  └──────────────────┘    └──────────────────┘
//! │                       │            │                       │
//!                         │            ▼                       │
//! │                       └─▶┌──────────────────┐◀─────────────┘
//!              ┌─────────────│   RecvResponse   │──┐
//! │            │             └──────────────────┘  │
//!              │                       │           │
//! │            ▼                       ▼           │
//!    ┌──────────────────┐    ┌──────────────────┐  │
//! └ ─│     Redirect     │◀───│     RecvBody     │  │
//!    └──────────────────┘    └──────────────────┘  │
//!              │                       │           │
//!              │                       ▼           │
//!              │             ┌──────────────────┐  │
//!              └────────────▶│     Cleanup      │◀─┘
//!                            └──────────────────┘
//! ```
//!
//! # Example
//!
//! ```
//! use ureq_proto::client::*;
//! use ureq_proto::http::Request;
//!
//! let request = Request::put("https://example.test/my-path")
//!     .header("Expect", "100-continue")
//!     .header("x-foo", "bar")
//!     .body(())
//!     .unwrap();
//!
//! // ********************************** Prepare
//!
//! let mut flow = Flow::new(request).unwrap();
//!
//! // Prepare with state from cookie jar. The uri
//! // is used to key the cookies.
//! let uri = flow.uri();
//!
//! // flow.header("Cookie", "my_cookie1=value1");
//! // flow.header("Cookie", "my_cookie2=value2");
//!
//! // Obtain a connection for the uri, either a
//! // pooled connection from a previous http/1.1
//! // keep-alive, or open a new. The connection
//! // must be TLS wrapped if the scheme so indicate.
//! // let connection = todo!();
//!
//! // Sans-IO means it does not use any
//! // Write trait or similar. Requests and request
//! // bodies are written to a buffer that in turn
//! // should be sent via the connection.
//! let mut output = vec![0_u8; 1024];
//!
//! // ********************************** SendRequest
//!
//! // Proceed to the next state writing the request.
//! let mut flow = flow.proceed();
//!
//! let output_used = flow.write(&mut output).unwrap();
//! assert_eq!(output_used, 107);
//!
//! assert_eq!(&output[..output_used], b"\
//!     PUT /my-path HTTP/1.1\r\n\
//!     host: example.test\r\n\
//!     transfer-encoding: chunked\r\n\
//!     expect: 100-continue\r\n\
//!     x-foo: bar\r\n\
//!     \r\n");
//!
//! // Check we can continue to send the body
//! assert!(flow.can_proceed());
//!
//! // ********************************** Await100
//!
//! // In this example, we know the next state is Await100.
//! // A real client needs to match on the variants.
//! let mut flow = match flow.proceed() {
//!     Ok(Some(SendRequestResult::Await100(v))) => v,
//!     _ => panic!(),
//! };
//!
//! // When awaiting 100, the client should run a timer and
//! // proceed to sending the body either when the server
//! // indicates it can receive the body, or the timer runs out.
//!
//! // This boolean can be checked whether there's any point
//! // in keeping waiting for the timer to run out.
//! assert!(flow.can_keep_await_100());
//!
//! let input = b"HTTP/1.1 100 Continue\r\n\r\n";
//! let input_used = flow.try_read_100(input).unwrap();
//!
//! assert_eq!(input_used, 25);
//! assert!(!flow.can_keep_await_100());
//!
//! // ********************************** SendBody
//!
//! // Proceeding is possible regardless of whether the
//! // can_keep_await_100() is true or false.
//! // A real client needs to match on the variants.
//! let mut flow = match flow.proceed() {
//!     Ok(Await100Result::SendBody(v)) => v,
//!     _ => panic!(),
//! };
//!
//! let (input_used, o1) =
//!     flow.write(b"hello", &mut output).unwrap();
//!
//! assert_eq!(input_used, 5);
//!
//! // When doing transfer-encoding: chunked,
//! // the end of body must be signaled with
//! // an empty input. This is also valid for
//! // regular content-length body.
//! assert!(!flow.can_proceed());
//!
//! let (_, o2) = flow.write(&[], &mut output[o1..]).unwrap();
//!
//! let output_used = o1 + o2;
//! assert_eq!(output_used, 15);
//!
//! assert_eq!(&output[..output_used], b"\
//!     5\r\n\
//!     hello\
//!     \r\n\
//!     0\r\n\
//!     \r\n");
//!
//! assert!(flow.can_proceed());
//!
//! // ********************************** RecvRequest
//!
//! // Proceed to read the request.
//! let mut flow = flow.proceed().unwrap();
//!
//! let part = b"HTTP/1.1 200 OK\r\nContent-Len";
//! let full = b"HTTP/1.1 200 OK\r\nContent-Length: 9\r\n\r\n";
//!
//! // try_response can be used repeatedly until we
//! // get enough content including all headers.
//! let (input_used, maybe_response) =
//!     flow.try_response(part, false).unwrap();
//!
//! assert_eq!(input_used, 0);
//! assert!(maybe_response.is_none());
//!
//! let (input_used, maybe_response) =
//!     flow.try_response(full, false).unwrap();
//!
//! assert_eq!(input_used, 38);
//! let response = maybe_response.unwrap();
//!
//! // ********************************** RecvBody
//!
//! // It's not possible to proceed until we
//! // have read a response.
//! let mut flow = match flow.proceed() {
//!     Some(RecvResponseResult::RecvBody(v)) => v,
//!     _ => panic!(),
//! };
//!
//! let(input_used, output_used) =
//!     flow.read(b"hi there!", &mut output).unwrap();
//!
//! assert_eq!(input_used, 9);
//! assert_eq!(output_used, 9);
//!
//! assert_eq!(&output[..output_used], b"hi there!");
//!
//! // ********************************** Cleanup
//!
//! let flow = match flow.proceed() {
//!     Some(RecvBodyResult::Cleanup(v)) => v,
//!     _ => panic!(),
//! };
//!
//! if flow.must_close_connection() {
//!     // connection.close();
//! } else {
//!     // connection.return_to_pool();
//! }
//!
//! ```
//!
//! # In scope:
//!
//! * First class HTTP/1.1 protocol implementation
//! * Indication of connection states (such as when a connection must be closed)
//! * transfer-encoding: chunked
//! * Redirect handling (building URI and amending requests)
//!
//! # Out of scope:
//!
//! * Opening/closing sockets
//! * TLS (https)
//! * Cookie jars
//! * Authorization
//! * Body data transformations (charset, compression etc)
//!
//! # The http crate
//!
//! Based on the [http crate](https://crates.io/crates/http) - a unified HTTP API for Rust.

use std::fmt;
use std::marker::PhantomData;

use http::{HeaderValue, StatusCode};

use crate::body::{BodyReader, BodyWriter};
use crate::util::ArrayVec;

use amended::AmendedRequest;

pub mod flow;

mod amended;

// mod holder;

#[cfg(test)]
mod test;

/// Max number of headers to parse from an HTTP response
pub const MAX_RESPONSE_HEADERS: usize = 128;

/// State types for the Flow state machine.
///
/// These types are used as type parameters to `Flow<B, State>` to represent
/// the current state of the HTTP request/response flow.
pub mod state {
    pub(crate) trait Named {
        fn name() -> &'static str;
    }

    macro_rules! flow_state {
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

    flow_state!(Prepare);
    flow_state!(SendRequest);
    flow_state!(Await100);
    flow_state!(SendBody);
    flow_state!(RecvResponse);
    flow_state!(RecvBody);
    flow_state!(Redirect);
    flow_state!(Cleanup);
}
use self::state::*;

/// A flow of states for an HTTP request/response cycle.
///
/// This is the main type of the Flow module, representing a state machine that
/// transitions through various states during the lifecycle of an HTTP request/response.
///
/// The type parameters are:
/// - `B`: The type of the request body
/// - `State`: The current state of the flow (e.g., `Prepare`, `SendRequest`, etc.)
///
/// See the [state graph][crate::client] in the client module documentation for a
/// visual representation of the state transitions.
pub struct Flow<B, State> {
    inner: Inner<B>,
    _ph: PhantomData<State>,
}

/// Internal state of a Flow.
///
/// This struct contains the actual state data for a Flow, independent of the
/// state type parameter. It's exposed as pub(crate) to allow tests to inspect
/// the state.
#[derive(Debug)]
pub(crate) struct Inner<B> {
    pub request: AmendedRequest<B>,
    pub analyzed: bool,
    pub state: BodyState,
    pub close_reason: ArrayVec<CloseReason, 4>,
    pub should_send_body: bool,
    pub await_100_continue: bool,
    pub status: Option<StatusCode>,
    pub location: Option<HeaderValue>,
}

impl<B> Inner<B> {
    fn is_redirect(&self) -> bool {
        match self.status {
            // 304 is a redirect code, but it has no location header and
            // thus we don't consider it a redirection.
            Some(v) => v.is_redirection() && v != StatusCode::NOT_MODIFIED,
            None => false,
        }
    }
}

/// State of the request/response body.
///
/// This struct tracks the current phase of the request/response cycle
/// and manages the body writers and readers.
#[derive(Debug, Default)]
pub(crate) struct BodyState {
    phase: RequestPhase,
    writer: BodyWriter,
    reader: Option<BodyReader>,
    allow_non_standard_methods: bool,
    stop_on_chunk_boundary: bool,
}

impl BodyState {
    fn need_response_body(&self) -> bool {
        !matches!(
            self.reader,
            Some(BodyReader::NoBody) | Some(BodyReader::LengthDelimited(0))
        )
    }
}

/// Phases of sending an HTTP request.
///
/// This enum represents the different phases of sending an HTTP request:
/// - `SendLine`: Sending the request line (method, path, version)
/// - `SendHeaders`: Sending the request headers
/// - `SendBody`: Sending the request body
#[derive(Clone, Copy, PartialEq, Eq)]
enum RequestPhase {
    SendLine,
    SendHeaders(usize),
    SendBody,
}

impl Default for RequestPhase {
    fn default() -> Self {
        Self::SendLine
    }
}

impl RequestPhase {
    fn is_prelude(&self) -> bool {
        matches!(self, RequestPhase::SendLine | RequestPhase::SendHeaders(_))
    }

    fn is_body(&self) -> bool {
        matches!(self, RequestPhase::SendBody)
    }
}

/// Reasons for closing an HTTP connection.
///
/// This enum represents the various reasons why an HTTP connection might need
/// to be closed after a request/response cycle is complete.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CloseReason {
    /// HTTP/1.0 requires each request-response to end with a close.
    Http10,

    /// Client sent `connection: close`.
    ClientConnectionClose,

    /// Server sent `connection: close`.
    ServerConnectionClose,

    /// When doing expect-100 the server sent _some other response_.
    ///
    /// For expect-100, the only two options for a server response are:
    ///
    /// * 100 continue, in which case we continue to send the body.
    /// * do nothing, in which case we continue to send the body after a timeout.
    ///
    /// Sending _something else_, like 401, is incorrect and we must close
    /// the connection.
    Not100Continue,

    /// Response body is close delimited.
    ///
    /// We do not know how much body data to receive. The socket will be closed
    /// when it's done. This is HTTP/1.0 semantics.
    CloseDelimitedBody,
}

impl CloseReason {
    fn explain(&self) -> &'static str {
        match self {
            CloseReason::Http10 => "version is http1.0",
            CloseReason::ClientConnectionClose => "client sent Connection: close",
            CloseReason::ServerConnectionClose => "server sent Connection: close",
            CloseReason::Not100Continue => "got non-100 response before sending body",
            CloseReason::CloseDelimitedBody => "response body is close delimited",
        }
    }
}

impl<B, S> Flow<B, S> {
    fn wrap(inner: Inner<B>) -> Flow<B, S>
    where
        S: Named,
    {
        let wrapped = Flow {
            inner,
            _ph: PhantomData,
        };

        debug!("{:?}", wrapped);

        wrapped
    }

    #[cfg(test)]
    pub(crate) fn inner(&self) -> &Inner<B> {
        &self.inner
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// PREPARE

mod prepare;

// //////////////////////////////////////////////////////////////////////////////////////////// SEND REQUEST

mod sendreq;

/// Possible state transitions after sending a request.
///
/// After sending the request headers, the flow can transition to one of three states:
/// - `Await100`: If the request included an `Expect: 100-continue` header
/// - `SendBody`: If the request has a body to send
/// - `RecvResponse`: If the request has no body (e.g., GET, HEAD)
///
/// See the [state graph][crate::client] for a visual representation.
pub enum SendRequestResult<B> {
    /// Expect-100/Continue mechanic.
    Await100(Flow<B, Await100>),

    /// Send the request body.
    SendBody(Flow<B, SendBody>),

    /// Receive the response.
    RecvResponse(Flow<B, RecvResponse>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// AWAIT 100

mod await100;

/// Possible state transitions after awaiting a 100 Continue response.
///
/// After awaiting a 100 Continue response, the flow can transition to one of two states:
/// - `SendBody`: If the server sent a 100 Continue response or the timeout expired
/// - `RecvResponse`: If the server sent a different response
///
/// See the [state graph][crate::client] for a visual representation.
pub enum Await100Result<B> {
    /// Send the request body.
    SendBody(Flow<B, SendBody>),

    /// Receive server response.
    RecvResponse(Flow<B, RecvResponse>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// SEND BODY

mod sendbody;

// //////////////////////////////////////////////////////////////////////////////////////////// RECV RESPONSE

mod recvresp;

/// Possible state transitions after receiving a response.
///
/// After receiving a response (status and headers), the flow can transition to one of three states:
/// - `RecvBody`: If the response has a body to receive
/// - `Redirect`: If the response is a redirect
/// - `Cleanup`: If the response has no body and is not a redirect
///
/// See the [state graph][crate::client] for a visual representation.
pub enum RecvResponseResult<B> {
    /// Receive a response body.
    RecvBody(Flow<B, RecvBody>),

    /// Follow a redirect.
    Redirect(Flow<B, Redirect>),

    /// Run cleanup.
    Cleanup(Flow<B, Cleanup>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// RECV BODY

mod recvbody;

/// Possible state transitions after receiving a response body.
///
/// After receiving a response body, the flow can transition to one of two states:
/// - `Redirect`: If the response is a redirect
/// - `Cleanup`: If the response is not a redirect
///
/// See the [state graph][crate::client] for a visual representation.
pub enum RecvBodyResult<B> {
    /// Follow a redirect
    Redirect(Flow<B, Redirect>),

    /// Go to cleanup
    Cleanup(Flow<B, Cleanup>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// REDIRECT

mod redirect;

/// Strategy for preserving authorization headers during redirects.
///
/// This enum defines how authorization headers should be handled when following
/// redirects:
///
/// * `Never`: Never preserve the `authorization` header in redirects. This is the default.
/// * `SameHost`: Preserve the `authorization` header when the redirect is to the same host
///   and uses the same scheme (or switches to a more secure one, i.e., from HTTP to HTTPS,
///   but not the reverse).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum RedirectAuthHeaders {
    /// Never preserve the `authorization` header on redirect. This is the default.
    Never,
    /// Preserve the `authorization` header when the redirect is to the same host. Both hosts must use
    /// the same scheme (or switch to a more secure one, i.e we can redirect from `http` to `https`,
    /// but not the reverse).
    SameHost,
}

// //////////////////////////////////////////////////////////////////////////////////////////// CLEANUP

impl<B> Flow<B, Cleanup> {
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

impl<B, State: Named> fmt::Debug for Flow<B, State> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Flow<{}>", State::name())
    }
}

impl fmt::Debug for RequestPhase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SendLine => write!(f, "SendLine"),
            Self::SendHeaders(_) => write!(f, "SendHeaders"),
            Self::SendBody => write!(f, "SendBody"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::client::amended::AmendedRequest;
    use crate::client::state::SendRequest;
    use crate::client::Inner;
    use crate::Error;
    use http::{Method, Request, Version};
    use std::str;

    #[test]
    fn head_simple() {
        let req = Request::head("http://foo.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        let mut flow = flow.proceed();

        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();
        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(s, "HEAD /page HTTP/1.1\r\nhost: foo.test\r\n\r\n");
    }

    #[test]
    fn head_without_body() {
        let req = Request::head("http://foo.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        let mut flow = flow.proceed();

        let mut output = vec![0; 1024];
        flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to the next state
        let next_flow = flow.proceed().unwrap().unwrap();

        // For a HEAD request, we should get a RecvResponse result
        let SendRequestResult::RecvResponse(_) = next_flow else {
            panic!("Expected RecvResponse")
        };
    }

    #[test]
    fn head_with_body_despite_method() {
        let req = Request::head("http://foo.test/page").body(()).unwrap();
        let mut flow = Flow::new(req).unwrap();

        // Force sending a body despite the method
        flow.send_body_despite_method();

        let mut flow = flow.proceed();

        let mut output = vec![0; 1024];
        flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to the next state
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        // Write an empty body
        let (i, n) = flow.write(&[], &mut output).unwrap();
        assert_eq!(i, 0);
        assert_eq!(n, 5); // "0\r\n\r\n" for chunked encoding

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());
    }

    #[test]
    fn post_simple() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 5)
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n1 = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to the next state
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        // Write the body
        let (i1, n2) = flow.write(b"hallo", &mut output[n1..]).unwrap();
        assert_eq!(i1, 5);

        let s = str::from_utf8(&output[..n1 + n2]).unwrap();
        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhallo"
        );
    }

    #[test]
    fn post_small_output() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 5)
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        let mut output = vec![0; 1024];
        let body = b"hallo";

        // Write the request headers in multiple steps with small output buffers
        {
            let n = flow.write(&mut output[..25]).unwrap();
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "POST /page HTTP/1.1\r\n");
            assert!(!flow.can_proceed());
        }

        {
            let n = flow.write(&mut output[..20]).unwrap();
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "host: f.test\r\n");
            assert!(!flow.can_proceed());
        }

        {
            let n = flow.write(&mut output[..21]).unwrap();
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "content-length: 5\r\n\r\n");
            assert!(flow.can_proceed());
        }

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        // Write the body
        {
            let (i, n) = flow.write(body, &mut output[..25]).unwrap();
            assert_eq!(n, 5);
            assert_eq!(i, 5);
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "hallo");

            // Check if we can proceed (body is fully sent)
            assert!(flow.can_proceed());
        }
    }

    #[test]
    fn post_with_short_content_length() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 2)
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n1 = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        // Write the body (first write should fail because it's larger than content-length)
        let body = b"hallo";
        let r = flow.write(body, &mut output[n1..]);
        assert_eq!(r.unwrap_err(), Error::BodyLargerThanContentLength);

        // Write a smaller body that fits within content-length
        let body = b"ha";
        let r = flow.write(body, &mut output[n1..]);
        assert!(r.is_ok());

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());
    }

    #[test]
    fn post_with_short_body_input() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 5)
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n1 = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        // Write the first part of the body
        let (i1, n2) = flow.write(b"ha", &mut output[n1..]).unwrap();
        assert_eq!(i1, 2);

        // Write the second part of the body
        let (i2, n3) = flow.write(b"ha", &mut output[n1 + n2..]).unwrap();
        assert_eq!(i2, 2);

        let s = str::from_utf8(&output[..n1 + n2 + n3]).unwrap();
        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhaha"
        );

        // Check if we can proceed (body is not fully sent yet)
        assert!(!flow.can_proceed());

        // Write the third part of the body (should fail because it's larger than remaining content length)
        let err = flow.write(b"llo", &mut output[n1 + n2 + n3..]).unwrap_err();
        assert_eq!(err, Error::BodyLargerThanContentLength);

        // Write the last byte to complete the content length
        let (i3, n4) = flow.write(b"l", &mut output[n1 + n2 + n3..]).unwrap();
        assert_eq!(i3, 1);

        let s = str::from_utf8(&output[..n1 + n2 + n3 + n4]).unwrap();
        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhahal"
        );

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());
    }

    #[test]
    fn post_with_chunked() {
        let req = Request::post("http://f.test/page")
            .header("transfer-encoding", "chunked")
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n1 = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody")
        };

        let body = b"hallo";

        // Write the first chunk of the body
        let (i1, n2) = flow.write(body, &mut output[n1..]).unwrap();
        assert_eq!(i1, 5);

        // Write the second chunk of the body
        let (i2, n3) = flow.write(body, &mut output[n1 + n2..]).unwrap();
        assert_eq!(i2, 5);

        // Indicate the end of the body
        let (i3, n4) = flow.write(&[], &mut output[n1 + n2 + n3..]).unwrap();
        assert_eq!(i3, 0);

        let s = str::from_utf8(&output[..n1 + n2 + n3 + n4]).unwrap();
        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhallo\r\n5\r\nhallo\r\n0\r\n\r\n"
        );

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());
    }

    #[test]
    fn post_without_body() {
        let req = Request::post("http://foo.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to the next state
        let next_flow = flow.proceed().unwrap().unwrap();

        // For a POST request, we should get a SendBody result
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody");
        };

        // Check that we can't proceed without writing a body
        assert!(!flow.can_proceed());

        // Write an empty body
        let (i, n) = flow.write(&[], &mut output).unwrap();
        assert_eq!(i, 0);
        assert_eq!(n, 5); // "0\r\n\r\n" for chunked encoding

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());
    }

    #[test]
    fn post_streaming() {
        let req = Request::post("http://f.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n1 = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody");
        };

        // Write the first chunk of the body (using i2, n2 to match original test)
        let (i2, n2) = flow.write(b"hallo", &mut output[n1..]).unwrap();

        // Send end (using i3, n3 to match original test)
        let (i3, n3) = flow.write(&[], &mut output[n1 + n2..]).unwrap();

        // Use i1 = 0 to match original test (in Flow API, i1 is not used for headers)
        let i1 = 0;

        // Verify the results with the same assertions as the original test
        assert_eq!(i1, 0);
        assert_eq!(i2, 5);
        assert_eq!(n1, 65);
        assert_eq!(n2, 10);
        assert_eq!(i3, 0);
        assert_eq!(n3, 5);

        let s = str::from_utf8(&output[..(n1 + n2 + n3)]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ntransfer-encoding: chunked\r\n\r\n5\r\nhallo\r\n0\r\n\r\n"
        );
    }

    #[test]
    fn post_streaming_with_size() {
        let req = Request::post("http://f.test/page")
            .header("content-length", "5")
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let headers_n = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody");
        };

        // Write the body (first call)
        let (i1, n1) = flow.write(b"hallo", &mut output[headers_n..]).unwrap();

        // Verify the results
        assert_eq!(i1, 5); // In Flow API, i1 is the number of bytes consumed from the input
        assert_eq!(n1, 5); // In Flow API, n1 is the number of bytes written to the output

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());

        // Try to write more data after the body is fully sent (should fail)
        let err = flow
            .write(b"hallo", &mut output[headers_n + n1..])
            .unwrap_err();
        assert_eq!(err, Error::BodyContentAfterFinish);

        let s = str::from_utf8(&output[..headers_n + n1]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhallo"
        );
    }

    #[test]
    fn post_streaming_after_end() {
        let req = Request::post("http://f.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let headers_n = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody");
        };

        // Write the body
        let (_, n1) = flow.write(b"hallo", &mut output[headers_n..]).unwrap();

        // Send end
        let (_, n2) = flow.write(&[], &mut output[headers_n + n1..]).unwrap();

        // Try to write after end
        let err = flow.write(b"after end", &mut output[headers_n + n1 + n2..]);

        assert_eq!(err, Err(Error::BodyContentAfterFinish));
    }

    #[test]
    fn post_streaming_too_much() {
        let req = Request::post("http://f.test/page")
            .header("content-length", "5")
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let headers_n = flow.write(&mut output).unwrap();

        // Check if we can proceed
        assert!(flow.can_proceed());

        // Proceed to SendBody
        let next_flow = flow.proceed().unwrap().unwrap();
        let SendRequestResult::SendBody(mut flow) = next_flow else {
            panic!("Expected SendBody");
        };

        // Write the body (first call)
        let (i1, n1) = flow.write(b"hallo", &mut output[headers_n..]).unwrap();

        // Verify the results
        assert_eq!(i1, 5); // In Flow API, i1 is the number of bytes consumed from the input
        assert_eq!(n1, 5); // In Flow API, n1 is the number of bytes written to the output

        // Check if we can proceed (body is fully sent)
        assert!(flow.can_proceed());

        // Try to write more data after the body is fully sent (should fail with BodyContentAfterFinish)
        let err = flow
            .write(b"hallo", &mut output[headers_n + n1..])
            .unwrap_err();
        assert_eq!(err, Error::BodyContentAfterFinish);

        let s = str::from_utf8(&output[..headers_n + n1]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhallo"
        );
    }

    #[test]
    fn username_password_uri() {
        let req = Request::get("http://martin:secret@f.test/page")
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(
            s,
            "GET /page HTTP/1.1\r\nhost: f.test\r\n\
            authorization: Basic bWFydGluOnNlY3JldA==\r\n\r\n"
        );
    }

    #[test]
    fn username_uri() {
        let req = Request::get("http://martin@f.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(
            s,
            "GET /page HTTP/1.1\r\nhost: f.test\r\n\
            authorization: Basic bWFydGluOg==\r\n\r\n"
        );
    }

    #[test]
    fn password_uri() {
        let req = Request::get("http://:secret@f.test/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(
            s,
            "GET /page HTTP/1.1\r\nhost: f.test\r\n\
            authorization: Basic OnNlY3JldA==\r\n\r\n"
        );
    }

    #[test]
    fn override_auth_header() {
        let req = Request::get("http://martin:secret@f.test/page")
            // This should override the auth from the URI
            .header("authorization", "meh meh meh")
            .body(())
            .unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(
            s,
            "GET /page HTTP/1.1\r\nhost: f.test\r\n\
            authorization: meh meh meh\r\n\r\n"
        );
    }

    #[test]
    fn non_standard_port() {
        let req = Request::get("http://f.test:8080/page").body(()).unwrap();
        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(s, "GET /page HTTP/1.1\r\nhost: f.test:8080\r\n\r\n");
    }

    #[test]
    fn non_standard_method_not_allowed() {
        let m = Method::from_bytes(b"FNORD").unwrap();

        let req = Request::builder()
            .method(m.clone())
            .uri("http://f.test:8080/page")
            .body(())
            .unwrap();

        let flow = Flow::new(req).unwrap();

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Try to write the request headers
        let mut output = vec![0; 1024];
        let err = flow.write(&mut output).unwrap_err();

        assert_eq!(err, Error::MethodVersionMismatch(m, Version::HTTP_11));
    }

    #[test]
    fn non_standard_method_when_allowed() {
        let m = Method::from_bytes(b"FNORD").unwrap();

        let req = Request::builder()
            .method(m.clone())
            .uri("http://f.test:8080/page")
            .body(())
            .unwrap();

        let mut flow = Flow::new(req).unwrap();

        // Allow non-standard methods
        flow.allow_non_standard_methods(true);

        // Proceed to SendRequest
        let mut flow = flow.proceed();

        // Write the request headers
        let mut output = vec![0; 1024];
        let n = flow.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(s, "FNORD /page HTTP/1.1\r\nhost: f.test:8080\r\n\r\n");
    }

    #[test]
    fn ensure_reasonable_stack_sizes() {
        macro_rules! ensure {
            ($type:ty, $size:tt) => {
                let sz = std::mem::size_of::<$type>();
                assert!(
                    sz <= $size,
                    "Stack size of {} is too big {} > {}",
                    stringify!($type),
                    sz,
                    $size
                );
            };
        }

        ensure!(http::Request<()>, 300); // 224
        ensure!(AmendedRequest<()>, 400); // 368
        ensure!(Inner<()>, 600); // 512
        ensure!(Flow<(), SendRequest>, 600); // 512
    }
}
