use http::{Method, Response, header};

use crate::body::BodyWriter;

use crate::body::response_body_allowed;
use crate::ext::HeaderIterExt;
use crate::{CloseReason, Error};

use super::state::{ProvideResponse, SendResponse};
use super::{Reply, append_request};

impl Reply<ProvideResponse> {
    /// Provide a response to the client's request.
    ///
    /// Takes a Response object and transitions to the SendResponse state.
    /// Handles setting appropriate headers for the response body if they weren't already set.
    pub fn provide(self, response: Response<()>) -> Result<Reply<SendResponse>, Error> {
        if self.inner.expect_100_reject
            && !response.status().is_client_error()
            && !response.status().is_server_error()
        {
            return Err(Error::BadReject100Status(response.status()));
        }

        let mut inner = append_request(self.inner, response);

        // unwrap are correct due to state we should be in when we get here.
        let response = inner.response.as_ref().unwrap();
        let server_close = if inner.http10 {
            response.headers().has(header::CONNECTION, "close")
        } else {
            response
                .headers()
                .any(|(h, v)| h == header::CONNECTION && v == "close")
        };
        if server_close {
            inner.record_close_reason(CloseReason::ServerConnectionClose);
        }

        let response = inner.response.as_mut().unwrap();

        let writer = inner.state.writer.take().unwrap();
        let info = response.analyze(writer)?;

        let body_provided = info.body_mode.has_body();

        let (_, status) = response.prelude();
        let status = status.into();
        let method = inner.method.as_ref().unwrap();

        let body_allowed = response_body_allowed(method, status);
        let force_send = inner.force_send_body;

        let should_send_body = body_allowed || force_send;

        // HEAD and 304 may describe the selected representation without sending it.
        // 1xx and 204 keep rejecting body headers.
        let metadata_only =
            status == 304 || (method == Method::HEAD && status >= 200 && status != 204);

        if body_provided && !should_send_body && !metadata_only {
            // User set a body header but method does not allow one
            return Err(Error::BodyNotAllowed);
        }

        let mut body_mode = info.body_mode;

        if inner.http10 {
            // Chunked coding is HTTP/1.1 only (RFC 9112 section 7.1). An HTTP/1.0
            // client reads the body until the connection closes, and a
            // Transfer-Encoding header means nothing to it.
            let has_transfer_encoding = response
                .headers()
                .any(|(h, _)| *h == header::TRANSFER_ENCODING);
            response.omit_transfer_encoding();
            if body_mode.is_chunked() || (has_transfer_encoding && should_send_body) {
                body_mode = BodyWriter::new_close_delimited();
            }
        }

        if body_provided
            && !info.res_body_header
            && should_send_body
            && !body_mode.is_close_delimited()
        {
            // User did not set a body header, we set one.
            let header = body_mode.body_header();
            response.set_header(header.0, header.1)?;
        }

        let writer = if metadata_only && !force_send {
            BodyWriter::new_none()
        } else {
            body_mode
        };

        let response_keep_alive = response.headers().has(header::CONNECTION, "keep-alive");

        if writer.is_close_delimited() {
            inner.record_close_reason(CloseReason::CloseDelimitedBody);
        }

        if inner.http10 && !inner.close_reason.is_empty() && response_keep_alive {
            inner.response.as_mut().unwrap().remove_keep_alive();
        }

        // An HTTP/1.0 client assumes the server closes unless told otherwise
        // (RFC 9112 Appendix C.2.2). Any recorded close reason means we do close:
        // the client did not ask for keep-alive, someone sent Connection: close,
        // the body is close delimited or the request body was left unread.
        if inner.http10 && inner.close_reason.is_empty() && !response_keep_alive {
            let response = inner.response.as_mut().unwrap();
            response.set_header(header::CONNECTION, "keep-alive")?;
        }

        inner.state.writer = Some(writer);

        Ok(Reply::wrap(inner))
    }

    /// Convert the state to send a body despite the method
    ///
    /// Methods like HEAD and CONNECT should not have attached bodies.
    /// Some broken APIs use bodies anyway and this is an escape hatch to
    /// interoperate with such services.
    pub fn force_send_body(&mut self) {
        self.inner.force_send_body = true;
    }
}
