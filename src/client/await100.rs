use http::StatusCode;

use crate::Error;
use crate::body::BodyWriter;
use crate::parser::try_parse_response;

use super::state::Await100;
use super::{Await100Result, Call};
use crate::CloseReason;

impl Call<Await100> {
    /// Attempt to read a 100-continue response.
    ///
    /// Tries to interpret bytes sent by the server as a 100-continue response. The expect-100 mechanic
    /// means we hope the server will give us an indication on whether to upload a potentially big
    /// request body, before we start doing it.
    ///
    /// * If the server supports expect-100, it will respond `HTTP/1.1 100 Continue\r\n\r\n`, or
    ///   some other response code (such as 403) if we are not allowed to post the body.
    /// * If the server does not support expect-100, it will not respond at all, in which case
    ///   we will proceed to sending the request body after some timeout.
    ///
    /// The results are:
    ///
    /// * `Ok(0)` - not enough data yet, continue waiting (or `proceed()` if you think we waited enough)
    /// * `Ok(n)` - `n` number of input bytes were consumed. Call `proceed()` next
    /// * `Err(e)` - some error that is not recoverable
    pub fn try_read_100(&mut self, input: &[u8]) -> Result<usize, Error> {
        // Parse a full interim/final response. RFC 9110 §15.2 allows header fields on
        // 1xx responses (e.g. Traefik puts Alt-Svc on 100 Continue), so allow a modest
        // number of headers and ignore them for 100 Continue.
        match try_parse_response::<32>(input) {
            Ok(v) => match v {
                Some((input_used, response)) => {
                    let status = response.status();

                    if status == StatusCode::CONTINUE {
                        // Got 100 Continue - proceed to send the body
                        self.inner.await_100_continue = false;
                        assert!(self.inner.state.writer.has_body());
                        Ok(input_used)
                    } else if status == StatusCode::SWITCHING_PROTOCOLS {
                        // 101 Switching Protocols - don't send body, mark connection, proceed to recv response
                        // https://curl.se/mail/lib-2004-08/0002.html
                        self.inner.await_100_continue = false;
                        self.inner.close_reason.push(CloseReason::ProtocolSwitch);
                        self.inner.state.writer = BodyWriter::new_none();
                        Ok(0)
                    } else if status.is_informational() {
                        // Other 1xx (102 Processing, 103 Early Hints, etc.)
                        // Consume transparently and continue waiting for 100 or timeout.
                        // Do NOT set await_100_continue to false - keep waiting.
                        Ok(input_used)
                    } else {
                        // Non-informational response (2xx-5xx) - don't send body
                        // https://curl.se/mail/lib-2004-08/0002.html
                        self.inner.await_100_continue = false;
                        self.inner.close_reason.push(CloseReason::Not100Continue);
                        self.inner.state.writer = BodyWriter::new_none();
                        Ok(0)
                    }
                }
                // Not enough input yet.
                None => Ok(0),
            },
            Err(e) => {
                self.inner.await_100_continue = false;

                if e == Error::HttpParseTooManyHeaders {
                    // More headers than we budgeted for on the interim parse. Treat as a
                    // final response already in flight and do not send the body.
                    self.inner.close_reason.push(CloseReason::Not100Continue);
                    self.inner.state.writer = BodyWriter::new_none();
                    Ok(0)
                } else {
                    Err(e)
                }
            }
        }
    }

    /// Tell if there is any point in waiting for more data from the server.
    ///
    /// Becomes `false` as soon as `try_read_100()` got enough data to determine what to do next.
    /// This might become `false` even if `try_read_100` returns `Ok(0)`.
    ///
    /// If this returns `false`, the user should continue with `proceed()`.
    pub fn can_keep_await_100(&self) -> bool {
        self.inner.await_100_continue
    }

    /// Proceed to the next state.
    pub fn proceed(self) -> Result<Await100Result, Error> {
        // We can always proceed out of Await100

        if self.inner.state.writer.has_body() {
            // TODO(martin): do i need this?
            // call.inner.call.analyze_request()?;
            let call = Call::wrap(self.inner);
            Ok(Await100Result::SendBody(call))
        } else {
            Ok(Await100Result::RecvResponse(Call::wrap(self.inner)))
        }
    }
}
