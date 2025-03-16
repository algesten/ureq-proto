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

/// Max number of headers to parse from an HTTP requst
pub const MAX_REQUEST_HEADERS: usize = 128;

/// [state graph][crate::server]
pub struct Reply<State, B = ()> {
    inner: Inner<B>,
    _ph: PhantomData<State>,
}

// pub(crate) for tests to inspect state
#[derive(Debug)]
pub(crate) struct Inner<B> {
    pub phase: ResponsePhase,
    pub state: BodyState,
    pub response: Option<AmendedResponse<B>>,
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

impl<S, B> Reply<S, B> {
    fn wrap(inner: Inner<B>) -> Reply<S, B>
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
    Send100(Reply<Send100>),
    /// Receive a request body.
    RecvBody(Reply<RecvBody>),
    /// Client did not send a body.
    ProvideResponse(Reply<ProvideResponse>),
}

// //////////////////////////////////////////////////////////////////////////////////////////// SEND 100

mod send100;

fn append_request<B0, B>(inner: Inner<B0>, response: Response<B>) -> Inner<B> {
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

impl<B> Reply<Cleanup, B> {
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

impl<State: Named, B> fmt::Debug for Reply<State, B> {
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
