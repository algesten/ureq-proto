use crate::Error;

use super::call::state::{RecvBody, RecvRequest, Send100, SendResponse, WithBody, WithoutBody};
use super::call::Call;

#[derive(Debug)]
pub(crate) enum CallHolder<B> {
    RecvRequest(Call<RecvRequest>),
    Send100(Call<Send100>),
    RecvBody(Call<RecvBody>),
    SendResponse(Call<SendResponse>),
    WithoutBody(Call<WithoutBody, B>),
    WithBody(Call<WithBody, B>),
    Empty,
}

impl CallHolder<()> {
    pub fn new() -> Result<Self, Error> {
        Ok(CallHolder::RecvRequest(Call::new()))
    }

    pub fn as_recv_request_mut(&mut self) -> &mut Call<RecvRequest> {
        match self {
            CallHolder::RecvRequest(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_send_100_mut(&mut self) -> &mut Call<Send100> {
        match self {
            CallHolder::Send100(v) => v,
            _ => unreachable!(),
        }
    }
}
