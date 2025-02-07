#![allow(missing_docs)]
//!
//!
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
//!     └─▶┌──────────────────┐◀──────────────┘
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

use std::fmt;
use std::io::Write;
use std::marker::PhantomData;

use amended::AmendedResponse;
use http::{header, Method, Request, Response, StatusCode, Version};

use crate::body::{BodyReader, BodyWriter};
use crate::ext::{HeaderIterExt, MethodExt};
use crate::parser::try_parse_request;
use crate::util::{log_data, Writer};
use crate::{ArrayVec, CloseReason, Error};

mod amended;

/// Max number of headers to parse from an HTTP requst
pub const MAX_REQUEST_HEADERS: usize = 128;

/// A flow of calls, in some state following the flow [state graph][crate::server]
pub struct Flow<State, B = ()> {
    inner: Inner<B>,
    _ph: PhantomData<State>,
}

// pub(crate) for tests to inspect state
#[derive(Debug)]
pub(crate) struct Inner<B> {
    pub phase: ResponsePhase,
    pub analyzed: bool,
    pub state: BodyState,
    pub response: Option<AmendedResponse<B>>,
    pub close_reason: ArrayVec<CloseReason, 4>,
    pub method: Option<Method>,
    pub expect_100: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ResponsePhase {
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

    flow_state!(RecvRequest);
    flow_state!(Send100);
    flow_state!(RecvBody);
    flow_state!(SendResponse);
    flow_state!(SendBody);
    flow_state!(Cleanup);
}
use self::state::*;

impl<S, B> Flow<S, B> {
    fn wrap(inner: Inner<B>) -> Flow<S, B>
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

// //////////////////////////////////////////////////////////////////////////////////////////// RECV REQUEST

mod recvreq;

/// The possible states after receiving a request.
///
/// See [state graph][crate::server]
pub enum RecvRequestResult {
    /// Client is expecting a 100-continue response.
    Send100(Flow<Send100>),
    /// Receive a request body.
    RecvBody(Flow<RecvBody>),
    /// Client did not send a body.
    SendResponse(Flow<SendResponse>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// SEND 100

mod send100;

fn append_request<B0, B>(
    inner: Inner<B0>,
    response: Response<B>,
    default_body_mode: BodyWriter,
) -> Inner<B> {
    Inner {
        phase: inner.phase,
        analyzed: inner.analyzed,
        state: BodyState {
            writer: Some(default_body_mode),
            ..inner.state
        },
        response: Some(AmendedResponse::new(response)),
        close_reason: inner.close_reason,
        method: inner.method,
        expect_100: inner.expect_100,
    }
}

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

mod recvbody;

// //////////////////////////////////////////////////////////////////////////////////////////// SEND RESPONSE

mod sendres;

// //////////////////////////////////////////////////////////////////////////////////////////// SEND RESPONSE

mod sendbody;

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

impl<State: Named, B> fmt::Debug for Flow<State, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Flow<{}>", State::name())
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
