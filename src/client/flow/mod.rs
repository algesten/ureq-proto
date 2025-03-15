//! A sequence of calls, such as following redirects.

use std::fmt;
use std::marker::PhantomData;

use http::{HeaderValue, StatusCode};

use crate::body::{BodyReader, BodyWriter};
use crate::util::ArrayVec;

use super::amended::AmendedRequest;

#[doc(hidden)]
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

/// A flow of calls, in some state following the flow [state graph][crate::client]
pub struct Flow<B, State> {
    inner: Inner<B>,
    _ph: PhantomData<State>,
}

// pub(crate) for tests to inspect state
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

#[derive(Debug, Default)]
pub(crate) struct BodyState {
    phase: RequestPhase,
    writer: BodyWriter,
    reader: Option<BodyReader>,
    skip_method_body_check: bool,
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

/// Reasons for an ended flow that requires the connection to be closed.
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

/// Resulting states from sending a request.
///
/// After sending the request, there are three possible next states. See [state graph][crate::client].
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

/// Possible state transitions from awaiting 100.
///
/// See [state graph][crate::client]
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

/// The possible states after receiving a response.
///
/// See [state graph][crate::client]
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

/// Possible states after receiving a body.
///
/// See [state graph][crate::client]
pub enum RecvBodyResult<B> {
    /// Follow a redirect
    Redirect(Flow<B, Redirect>),

    /// Go to cleanup
    Cleanup(Flow<B, Cleanup>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// REDIRECT

mod redirect;

/// Strategy for keeping `authorization` headers during redirects.
///
/// * `Never` never preserves `authorization` header in redirects.
/// * `SameHost` send the authorization header in redirects only if the host of the redirect is
///   the same of the previous request, and both use the same scheme (or switch to a more secure one, i.e
///   we can redirect from `http` to `https`, but not the reverse).
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
