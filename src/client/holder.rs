use std::mem;

use http::Request;

use crate::ext::MethodExt;
use crate::{BodyMode, Error};

use super::amended::AmendedRequest;
use super::call::state::{RecvBody, RecvResponse, WithBody, WithoutBody};
use super::call::Call;

/// Holder of [`Call`] regardless of type state
///
/// TODO(martin): is it weird to type state and then erase it?
#[derive(Debug)]
pub(crate) enum CallHolder<B> {
    WithoutBody(Call<WithoutBody, B>),
    WithBody(Call<WithBody, B>),
    RecvResponse(Call<RecvResponse, B>),
    RecvBody(Call<RecvBody, B>),
    Empty,
}

impl<B> CallHolder<B> {
    pub fn new(request: Request<B>) -> Result<Self, Error> {
        Ok(if request.method().need_request_body() {
            CallHolder::WithBody(Call::with_body(request)?)
        } else {
            CallHolder::WithoutBody(Call::without_body(request)?)
        })
    }

    pub fn request(&self) -> &AmendedRequest<B> {
        match self {
            CallHolder::WithoutBody(v) => v.amended(),
            CallHolder::WithBody(v) => v.amended(),
            CallHolder::RecvResponse(v) => v.amended(),
            CallHolder::RecvBody(v) => v.amended(),
            CallHolder::Empty => unreachable!(),
        }
    }

    pub fn request_mut(&mut self) -> &mut AmendedRequest<B> {
        match self {
            CallHolder::WithoutBody(v) => v.amended_mut(),
            CallHolder::WithBody(v) => v.amended_mut(),
            CallHolder::RecvResponse(v) => v.amended_mut(),
            CallHolder::RecvBody(v) => v.amended_mut(),
            CallHolder::Empty => unreachable!(),
        }
    }

    pub fn as_with_body(&self) -> &Call<WithBody, B> {
        match self {
            CallHolder::WithBody(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_with_body_mut(&mut self) -> &mut Call<WithBody, B> {
        match self {
            CallHolder::WithBody(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_recv_response(&self) -> &Call<RecvResponse, B> {
        match self {
            CallHolder::RecvResponse(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_recv_response_mut(&mut self) -> &mut Call<RecvResponse, B> {
        match self {
            CallHolder::RecvResponse(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_recv_body(&self) -> &Call<RecvBody, B> {
        match self {
            CallHolder::RecvBody(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_recv_body_mut(&mut self) -> &mut Call<RecvBody, B> {
        match self {
            CallHolder::RecvBody(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn analyze_request(&mut self) -> Result<(), Error> {
        match self {
            CallHolder::WithoutBody(v) => v.analyze_request(),
            CallHolder::WithBody(v) => v.analyze_request(),
            CallHolder::RecvResponse(v) => v.analyze_request(),
            CallHolder::RecvBody(v) => v.analyze_request(),
            CallHolder::Empty => unreachable!(),
        }
    }

    pub(crate) fn body_mode(&self) -> BodyMode {
        match self {
            CallHolder::WithoutBody(v) => v.body_mode(),
            CallHolder::WithBody(v) => v.body_mode(),
            CallHolder::RecvResponse(v) => v.body_mode(),
            CallHolder::RecvBody(v) => v.body_mode(),
            CallHolder::Empty => unreachable!(),
        }
    }

    pub(crate) fn convert_to_send_body(&mut self) {
        if !matches!(self, CallHolder::WithoutBody(_)) {
            return;
        }

        let without = mem::replace(self, CallHolder::Empty);
        let call = match without {
            CallHolder::WithoutBody(call) => call,
            _ => unreachable!(),
        };

        let call = call.into_send_body();
        let _ = mem::replace(self, CallHolder::WithBody(call));
    }

    pub(crate) fn allow_non_standard_methods(&mut self, v: bool) {
        match self {
            CallHolder::WithoutBody(c) => c.allow_non_standard_methods(v),
            CallHolder::WithBody(c) => c.allow_non_standard_methods(v),
            _ => unreachable!(),
        }
    }
}
