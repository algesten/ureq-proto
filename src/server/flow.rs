use std::fmt;
use std::marker::PhantomData;

use http::{header, Method, Request, Response};

use crate::ext::{HeaderIterExt, MethodExt};
use crate::{ArrayVec, CloseReason, Error};

use super::holder::CallHolder;

/// A flow of calls, in some state following the flow [state graph][crate::server]
pub struct Flow<State, B = ()> {
    inner: Inner<B>,
    _ph: PhantomData<State>,
}

// pub(crate) for tests to inspect state
#[derive(Debug)]
pub(crate) struct Inner<B> {
    pub call: CallHolder<B>,
    pub close_reason: ArrayVec<CloseReason, 4>,
    pub method: Option<Method>,
    pub expect_100: bool,
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

    fn call(&self) -> &CallHolder<B> {
        &self.inner.call
    }

    fn call_mut(&mut self) -> &mut CallHolder<B> {
        &mut self.inner.call
    }

    #[cfg(test)]
    pub(crate) fn inner(&self) -> &Inner<B> {
        &self.inner
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// RECV REQUEST

impl Flow<RecvRequest> {
    /// Create a new Flow.
    pub fn new() -> Result<Self, Error> {
        let close_reason = ArrayVec::from_fn(|_| CloseReason::Http10);

        let call = CallHolder::new()?;

        let inner = Inner {
            call,
            close_reason,
            method: None,
            expect_100: false,
        };

        Ok(Flow::wrap(inner))
    }

    /// Try reading a request from the input.
    ///
    /// The `(usize, Option<Request()>)` is `(input amount consumed, request`).
    pub fn try_request(&mut self, input: &[u8]) -> Result<(usize, Option<Request<()>>), Error> {
        let maybe_request = self.inner.call.as_recv_request_mut().try_request(input)?;

        let (input_used, request) = match maybe_request {
            Some(v) => v,
            // Not enough input for a full response yet
            None => return Ok((0, None)),
        };

        self.inner.method = Some(request.method().clone());
        self.inner.expect_100 = request.headers().iter().has_expect_100();

        if request.headers().iter().has(header::CONNECTION, "close") {
            self.inner
                .close_reason
                .push(CloseReason::ClientConnectionClose);
        }

        Ok((input_used, Some(request)))
    }

    pub fn can_proceed(&self) -> bool {
        todo!()
    }

    /// Proceed to the next state.
    ///
    /// This returns `None` if we have not finished receiving the request. It is guaranteed that if
    /// `can_proceed()` returns true, this will return `Some`.
    pub fn proceed(mut self) -> Option<RecvRequestResult> {
        if !self.can_proceed() {
            return None;
        }

        let call_body = match self.inner.call {
            CallHolder::RecvRequest(v) => v,
            _ => unreachable!(),
        };

        let method = self.inner.method.as_ref().unwrap();
        let has_request_body = method.need_request_body();

        if self.inner.expect_100 {
            let call_body = call_body.into_send_100();
            self.inner.call = CallHolder::Send100(call_body);
            Some(RecvRequestResult::Send100(Flow::wrap(self.inner)))
        } else if has_request_body {
            let call_body = call_body.into_body();
            self.inner.call = CallHolder::RecvBody(call_body);
            Some(RecvRequestResult::RecvBody(Flow::wrap(self.inner)))
        } else {
            let call_body = call_body.into_send_response();
            self.inner.call = CallHolder::SendResponse(call_body);
            Some(RecvRequestResult::SendResponse(Flow::wrap(self.inner)))
        }
    }
}

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

impl Flow<Send100> {
    /// Sends a 100 Continue response.
    ///
    /// This proceeds to receiving the body.
    ///
    /// Panics if output isn't large enough to contain the 100 Continue status line.
    pub fn accept(mut self, output: &mut [u8]) -> Result<(usize, Flow<RecvBody>), Error> {
        let output_used = self.call_mut().as_send_100_mut().send_100(output)?;

        if output_used == 0 {
            panic!("Not enough output to write 100-Continue");
        }

        let call_body = match self.inner.call {
            CallHolder::Send100(v) => v,
            _ => unreachable!(),
        };

        self.inner.call = CallHolder::RecvBody(call_body.into_recv_body());
        let flow = Flow::wrap(self.inner);

        Ok((output_used, flow))
    }

    pub fn reject<B>(self, response: Response<B>) -> Result<Flow<SendResponse, B>, Error> {
        if !response.status().is_client_error() && !response.status().is_server_error() {
            return Err(Error::BadReject100Status(response.status()));
        }

        let call_body = match self.inner.call {
            CallHolder::Send100(v) => v,
            _ => unreachable!(),
        };

        let call_body = call_body.reject(response)?;

        let inner = Inner {
            call: CallHolder::WithBody(call_body),
            close_reason: self.inner.close_reason,
            method: self.inner.method,
            expect_100: self.inner.expect_100,
        };

        Ok(Flow::wrap(inner))
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// RECV BODY

impl Flow<RecvBody> {}

// ////////////////////////////////////////////////////////////////////////////////////////////

impl<State: Named, B> fmt::Debug for Flow<State, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Flow<{}>", State::name())
    }
}
