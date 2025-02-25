//! A sequence of calls, such as following redirects.

use std::fmt;
use std::marker::PhantomData;

use http::header;
use http::uri::Scheme;
use http::{
    HeaderMap, HeaderName, HeaderValue, Method, Request, Response, StatusCode, Uri, Version,
};

use crate::body::calculate_max_input;
use crate::ext::{HeaderIterExt, MethodExt, StatusExt};
use crate::parser::try_parse_response;
use crate::util::ArrayVec;
use crate::{BodyMode, Error};

use super::holder::CallHolder;

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
    pub call: CallHolder<B>,
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

// //////////////////////////////////////////////////////////////////////////////////////////// PREPARE

impl<B> Flow<B, Prepare> {
    /// Create a new Flow.
    pub fn new(request: Request<B>) -> Result<Self, Error> {
        let mut close_reason = ArrayVec::from_fn(|_| CloseReason::Http10);

        if request.version() == Version::HTTP_10 {
            // request.analyze() in CallHolder::new() ensures the only versions are HTTP 1.0 and 1.1
            close_reason.push(CloseReason::Http10)
        }

        if request.headers().iter().has(header::CONNECTION, "close") {
            close_reason.push(CloseReason::ClientConnectionClose);
        }

        let should_send_body = request.method().need_request_body();
        let await_100_continue = request.headers().iter().has_expect_100();

        let call = CallHolder::new(request)?;

        let inner = Inner {
            call,
            close_reason,
            should_send_body,
            await_100_continue,
            status: None,
            location: None,
        };

        Ok(Flow::wrap(inner))
    }

    /// Inspect call method
    pub fn method(&self) -> &Method {
        self.call().request().method()
    }

    /// Inspect call URI
    pub fn uri(&self) -> &Uri {
        self.call().request().uri()
    }

    /// Inspect call HTTP version
    pub fn version(&self) -> Version {
        self.call().request().version()
    }

    /// Inspect call headers
    pub fn headers(&self) -> &HeaderMap {
        self.call().request().original_request_headers()
    }

    /// Set whether to allow non-standard HTTP methods.
    ///
    /// By default the methods are limited by the HTTP version.
    pub fn allow_non_standard_methods(&mut self, v: bool) {
        self.call_mut().allow_non_standard_methods(v);
    }

    /// Add more headers to the call
    pub fn header<K, V>(&mut self, key: K, value: V) -> Result<(), Error>
    where
        HeaderName: TryFrom<K>,
        <HeaderName as TryFrom<K>>::Error: Into<http::Error>,
        HeaderValue: TryFrom<V>,
        <HeaderValue as TryFrom<V>>::Error: Into<http::Error>,
    {
        self.call_mut().request_mut().set_header(key, value)
    }

    /// Convert the call to send body despite method.
    ///
    /// Methods like GET, HEAD and DELETE should not have a request body.
    /// Some broken APIs use bodies anyway, and this is an escape hatch to
    /// interoperate with such services.
    pub fn send_body_despite_method(&mut self) {
        self.inner.should_send_body = true;
        self.inner.call.convert_to_send_body();
    }

    /// Continue to the next flow state.
    pub fn proceed(self) -> Flow<B, SendRequest> {
        Flow::wrap(self.inner)
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// SEND REQUEST

impl<B> Flow<B, SendRequest> {
    /// Write the request to the buffer.
    ///
    /// Writes incrementally, it can be called repeatedly in situations where the output
    /// buffer is small.
    ///
    /// This includes the first row, i.e. `GET / HTTP/1.1` and all headers.
    /// The output buffer needs to be large enough for the longest row.
    ///
    /// Example:
    ///
    /// ```text
    /// POST /bar HTTP/1.1\r\n
    /// Host: my.server.test\r\n
    /// User-Agent: myspecialthing\r\n
    /// \r\n
    /// <body data>
    /// ```
    ///
    /// The buffer would need to be at least 28 bytes big, since the `User-Agent` row is
    /// 28 bytes long.
    ///
    /// If the output is too small for the longest line, the result is an `OutputOverflow` error.
    ///
    /// The `Ok(usize)` is the number of bytes of the `output` buffer that was used.
    pub fn write(&mut self, output: &mut [u8]) -> Result<usize, Error> {
        match &mut self.inner.call {
            CallHolder::WithoutBody(v) => v.write(output),
            CallHolder::WithBody(v) => v.write(&[], output).map(|r| r.1),
            _ => unreachable!(),
        }
    }

    /// The configured method.
    pub fn method(&self) -> &Method {
        self.call().request().method()
    }

    /// The uri being requested.
    pub fn uri(&self) -> &Uri {
        self.call().request().uri()
    }

    /// Version of the request.
    ///
    /// This can only be 1.0 or 1.1.
    pub fn version(&self) -> Version {
        self.call().request().version()
    }

    /// The configured headers.
    pub fn headers_map(&mut self) -> Result<HeaderMap, Error> {
        self.call_mut().analyze_request()?;
        let mut map = HeaderMap::new();
        for (k, v) in self.call().request().headers() {
            map.insert(k, v.clone());
        }
        Ok(map)
    }

    /// Check whether the entire request has been sent.
    ///
    /// This is useful when the output buffer is small and we need to repeatedly
    /// call `write()` to send the entire request.
    pub fn can_proceed(&self) -> bool {
        match &self.inner.call {
            CallHolder::WithoutBody(v) => v.is_finished(),
            CallHolder::WithBody(v) => v.is_body(),
            _ => unreachable!(),
        }
    }

    /// Attempt to proceed from this state to the next.
    ///
    /// Returns `None` if the entire request has not been sent. It is guaranteed that if
    /// `can_proceed()` returns `true`, this will return `Some`.
    pub fn proceed(mut self) -> Result<Option<SendRequestResult<B>>, Error> {
        if !self.can_proceed() {
            return Ok(None);
        }

        if self.inner.should_send_body {
            if self.inner.await_100_continue {
                Ok(Some(SendRequestResult::Await100(Flow::wrap(self.inner))))
            } else {
                let mut flow = Flow::wrap(self.inner);
                flow.inner.call.analyze_request()?;
                Ok(Some(SendRequestResult::SendBody(flow)))
            }
        } else {
            let call = match self.inner.call {
                CallHolder::WithoutBody(v) => v,
                _ => unreachable!(),
            };

            // unwrap here is ok because self.can_proceed() should check the necessary
            // error conditions that would prevent us from converting.
            let call_recv = call.into_receive().unwrap();

            let call = CallHolder::RecvResponse(call_recv);
            self.inner.call = call;

            let flow = Flow::wrap(self.inner);
            Ok(Some(SendRequestResult::RecvResponse(flow)))
        }
    }
}

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

impl<B> Flow<B, Await100> {
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
        // Try parsing a status line without any headers. The line we are looking for is:
        //
        //   HTTP/1.1 100 Continue\r\n\r\n
        //
        // There should be no headers.
        match try_parse_response::<0>(input) {
            Ok(v) => match v {
                Some((input_used, response)) => {
                    self.inner.await_100_continue = false;

                    if response.status() == StatusCode::CONTINUE {
                        // should_send_body ought to be true since initialization.
                        assert!(self.inner.should_send_body);
                        Ok(input_used)
                    } else {
                        // We encountered a status line, without headers, but it wasn't 100,
                        // so we should not continue to send the body. Furthermore we mustn't
                        // reuse the connection.
                        // https://curl.se/mail/lib-2004-08/0002.html
                        self.inner.close_reason.push(CloseReason::Not100Continue);
                        self.inner.should_send_body = false;
                        Ok(0)
                    }
                }
                // Not enough input yet.
                None => Ok(0),
            },
            Err(e) => {
                self.inner.await_100_continue = false;

                if e == Error::HttpParseTooManyHeaders {
                    // We encountered headers after the status line. That means the server did
                    // not send 100-continue, and also continued to produce an answer before we
                    // sent the body. Regardless of what the answer is, we must not send the body.
                    // A 200-answer would be nonsensical given we haven't yet sent the body.
                    //
                    // We do however want to receive the response to be able to provide
                    // the Response<()> to the user. Hence this is not considered an error.
                    self.inner.close_reason.push(CloseReason::Not100Continue);
                    self.inner.should_send_body = false;
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
    pub fn proceed(self) -> Result<Await100Result<B>, Error> {
        // We can always proceed out of Await100

        if self.inner.should_send_body {
            let mut flow = Flow::wrap(self.inner);
            flow.inner.call.analyze_request()?;
            Ok(Await100Result::SendBody(flow))
        } else {
            Ok(Await100Result::RecvResponse(Flow::wrap(self.inner)))
        }
    }
}

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

impl<B> Flow<B, SendBody> {
    /// Write request body from `input` to `output`.
    ///
    /// This is called repeatedly until the entire body has been sent. The output buffer is filled
    /// as much as possible for each call.
    ///
    /// Depending on request headers, the output might be `transfer-encoding: chunked`. Chunking means
    /// the output is slightly larger than the input due to the extra length headers per chunk.
    /// When not doing chunked, the input/output will be the same per call.
    ///
    /// The result `(usize, usize)` is `(input consumed, output used)`.
    ///
    /// **Important**
    ///
    /// To indicate that the body is fully sent, you call write with an `input` parameter set to `&[]`.
    /// This ends the `transfer-encoding: chunked` and ensures the state is correct to proceed.
    pub fn write(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        self.inner.call.as_with_body_mut().write(input, output)
    }

    /// Helper to avoid copying memory.
    ///
    /// When the transfer is _NOT_ chunked, `write()` just copies the `input` to the `output`.
    /// This memcopy might be possible to avoid if the user can use the `input` buffer directly
    /// against the transport.
    ///
    /// This function is used to "report" how much of the input that has been used. It's effectively
    /// the same as the first `usize` in the pair returned by `write()`.
    pub fn consume_direct_write(&mut self, amount: usize) -> Result<(), Error> {
        self.inner
            .call
            .as_with_body_mut()
            .consume_direct_write(amount)
    }

    /// Calculate the max amount of input we can transfer to fill the `output_len`.
    ///
    /// For chunked transfer, the input is less than the output.
    pub fn calculate_max_input(&mut self, output_len: usize) -> usize {
        let call = self.inner.call.as_with_body_mut();

        // For non-chunked, the entire output can be used.
        if !call.is_chunked() {
            return output_len;
        }

        calculate_max_input(output_len)
    }

    /// Test if call is chunked.
    ///
    /// This might need some processing, hence the &mut and
    pub fn is_chunked(&mut self) -> bool {
        let call = self.inner.call.as_with_body_mut();
        call.is_chunked()
    }

    /// Check whether the request body is fully sent.
    ///
    /// For requests with a `content-length` header set, this will only become `true` once the
    /// number of bytes communicated have been sent. For chunked transfer, this becomes `true`
    /// after calling `write()` with an input of `&[]`.
    pub fn can_proceed(&self) -> bool {
        self.inner.call.as_with_body().is_finished()
    }

    /// Proceed to the next state.
    ///
    /// Returns `None` if it's not possible to proceed. It's guaranteed that if `can_proceed()` returns
    /// `true`, this will result in `Some`.
    pub fn proceed(mut self) -> Option<Flow<B, RecvResponse>> {
        if !self.can_proceed() {
            return None;
        }

        let call_body = match self.inner.call {
            CallHolder::WithBody(v) => v,
            _ => unreachable!(),
        };

        // unwrap here is ok because self.can_proceed() should check the necessary
        // error conditions that would prevent us from converting.
        let call_recv = call_body.into_receive().unwrap();

        let call = CallHolder::RecvResponse(call_recv);
        self.inner.call = call;

        Some(Flow::wrap(self.inner))
    }
}

// //////////////////////////////////////////////////////////////////////////////////////////// RECV RESPONSE

impl<B> Flow<B, RecvResponse> {
    /// Try reading a response from the input.
    ///
    /// * `allow_partial_redirect` - if `true`, we can accept to find the `Location` header
    ///   and proceed without reading the entire header. This is useful for broken servers that
    ///   don't send an entire \r\n at the end of the preamble.
    ///
    /// The `(usize, Option<Response()>)` is `(input amount consumed, response`).
    ///
    /// Notice that it's possible that we get an `input amount consumed` despite not returning
    /// a `Some(Response)`. This can happen if the server returned a 100-continue, and due to
    /// timing reasons we did not receive it while we were in the `Await100` flow state. This
    /// "spurios" 100 will be discarded before we parse the actual response.
    pub fn try_response(
        &mut self,
        input: &[u8],
        allow_partial_redirect: bool,
    ) -> Result<(usize, Option<Response<()>>), Error> {
        let maybe_response = self
            .inner
            .call
            .as_recv_response_mut()
            .try_response(input, allow_partial_redirect)?;

        let (input_used, response) = match maybe_response {
            Some(v) => v,
            // Not enough input for a full response yet
            None => return Ok((0, None)),
        };

        if response.status() == StatusCode::CONTINUE && self.inner.await_100_continue {
            // We have received a "delayed" 100-continue. This means the server did
            // not produce the 100-continue response in time while we were in the
            // state Await100. This is not an error, it can happen if the network is slow.
            self.inner.await_100_continue = false;

            // We should consume the response and wait for the next.
            return Ok((input_used, None));
        }

        self.inner.status = Some(response.status());
        // We want the last Location header.
        self.inner.location = response
            .headers()
            .get_all(header::LOCATION)
            .into_iter()
            .last()
            .cloned();

        if response.headers().iter().has(header::CONNECTION, "close") {
            self.inner
                .close_reason
                .push(CloseReason::ServerConnectionClose);
        }

        Ok((input_used, Some(response)))
    }

    /// Tell if we have finished receiving the response.
    pub fn can_proceed(&self) -> bool {
        self.inner.call.as_recv_response().is_finished()
    }

    /// Proceed to the next state.
    ///
    /// This returns `None` if we have not finished receiving the response. It is guaranteed that if
    /// `can_proceed()` returns true, this will return `Some`.
    pub fn proceed(mut self) -> Option<RecvResponseResult<B>> {
        if !self.can_proceed() {
            return None;
        }

        let call_body = match self.inner.call {
            CallHolder::RecvResponse(v) => v,
            _ => unreachable!(),
        };

        let has_response_body = call_body.need_response_body();
        let call_body = call_body.do_into_body();

        if has_response_body {
            if call_body.is_close_delimited() {
                self.inner
                    .close_reason
                    .push(CloseReason::CloseDelimitedBody);
            }

            self.inner.call = CallHolder::RecvBody(call_body);

            Some(RecvResponseResult::RecvBody(Flow::wrap(self.inner)))
        } else {
            self.inner.call = CallHolder::RecvBody(call_body);

            Some(if self.inner.is_redirect() {
                RecvResponseResult::Redirect(Flow::wrap(self.inner))
            } else {
                RecvResponseResult::Cleanup(Flow::wrap(self.inner))
            })
        }
    }
}

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

impl<B> Flow<B, RecvBody> {
    /// Read the response body from `input` to `output`.
    ///
    /// Depending on response headers, we can be in `transfer-encoding: chunked` or not. If we are,
    /// there will be less `output` bytes than `input`.
    ///
    /// The result `(usize, usize)` is `(input consumed, output buffer used)`.
    pub fn read(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        self.inner.call.as_recv_body_mut().read(input, output)
    }

    /// Set if we are stopping on chunk boundaries.
    ///
    /// If `false`, we try to fill entire `output` on each read() call.
    /// Has no meaning unless the response in chunked.
    ///
    /// Defaults to `false`
    pub fn stop_on_chunk_boundary(&mut self, enabled: bool) {
        self.inner
            .call
            .as_recv_body_mut()
            .stop_on_chunk_boundary(enabled);
    }

    /// Tell if the reading is on a chunk boundary.
    ///
    /// Used when we want to read exactly chunk-by-chunk.
    ///
    /// Only releveant if we first enabled `stop_on_chunk_boundary()`.
    pub fn is_on_chunk_boundary(&self) -> bool {
        self.inner.call.as_recv_body().is_on_chunk_boundary()
    }

    /// Tell which kind of mode the response body is.
    pub fn body_mode(&self) -> BodyMode {
        self.call().body_mode()
    }

    /// Check if the response body has been fully received.
    pub fn can_proceed(&self) -> bool {
        let call = self.inner.call.as_recv_body();
        call.is_ended() || call.is_close_delimited()
    }

    /// Proceed to the next state.
    ///
    /// Returns `None` if we are not fully received the body. It is guaranteed that if `can_proceed()`
    /// returns `true`, this will return `Some`.
    pub fn proceed(self) -> Option<RecvBodyResult<B>> {
        if !self.can_proceed() {
            return None;
        }

        Some(if self.inner.is_redirect() {
            RecvBodyResult::Redirect(Flow::wrap(self.inner))
        } else {
            RecvBodyResult::Cleanup(Flow::wrap(self.inner))
        })
    }
}

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

impl<B> Flow<B, Redirect> {
    /// Construct a new `Flow` by following the redirect.
    ///
    /// There are some rules when follwing a redirect.
    ///
    /// * For 307/308
    ///     * POST/PUT results in `None`, since we do not allow redirecting a request body
    ///     * DELETE is intentionally excluded: <https://stackoverflow.com/questions/299628>
    ///     * All other methods retain the method in the redirect
    /// * Other redirect (301, 302, etc)
    ///     * HEAD results in HEAD in the redirect
    ///     * All other methods becomes GET
    pub fn as_new_flow(
        &mut self,
        redirect_auth_headers: RedirectAuthHeaders,
    ) -> Result<Option<Flow<B, Prepare>>, Error> {
        let header = match &self.inner.location {
            Some(v) => v,
            None => return Err(Error::NoLocationHeader),
        };

        let location = match header.to_str() {
            Ok(v) => v,
            Err(_) => {
                return Err(Error::BadLocationHeader(
                    String::from_utf8_lossy(header.as_bytes()).to_string(),
                ))
            }
        };

        // Previous request
        let previous = self.inner.call.request_mut();

        // Unwrap is OK, because we can't be here without having read a response.
        let status = self.inner.status.unwrap();
        let method = previous.method();

        // A new uri by combining the base from the previous request and the new location.
        let uri = previous.new_uri_from_location(location)?;

        // Perform the redirect method differently depending on 3xx code.
        let new_method = if status.is_redirect_retaining_status() {
            if method.need_request_body() {
                // only resend the request if it cannot have a body
                return Ok(None);
            } else if method == Method::DELETE {
                // NOTE: DELETE is intentionally excluded: https://stackoverflow.com/questions/299628
                return Ok(None);
            } else {
                method.clone()
            }
        } else {
            // this is to follow how curl does it. POST, PUT etc change
            // to GET on a redirect.
            if matches!(*method, Method::GET | Method::HEAD) {
                method.clone()
            } else {
                Method::GET
            }
        };

        let mut request = previous.take_request();
        *request.method_mut() = new_method;

        // Next state
        let mut next = Flow::new(request)?;

        let request = next.inner.call.request_mut();

        let keep_auth_header = match redirect_auth_headers {
            RedirectAuthHeaders::Never => false,
            RedirectAuthHeaders::SameHost => can_redirect_auth_header(request.uri(), &uri),
        };

        // Override with the new uri
        request.set_uri(uri);

        if !keep_auth_header {
            request.unset_header(header::AUTHORIZATION)?;
        }
        request.unset_header(header::COOKIE)?;
        request.unset_header(header::CONTENT_LENGTH)?;

        // TODO(martin): clear out unwanted headers

        Ok(Some(next))
    }

    /// The redirect status code.
    pub fn status(&self) -> StatusCode {
        self.inner.status.unwrap()
    }

    /// Whether we must close the connection corresponding to the current flow.
    ///
    /// This is used to inform connection pooling.
    pub fn must_close_connection(&self) -> bool {
        self.close_reason().is_some()
    }

    /// If we are closing the connection, give a reason why.
    pub fn close_reason(&self) -> Option<&'static str> {
        self.inner.close_reason.first().map(|s| s.explain())
    }

    /// Proceed to the cleanup state.
    pub fn proceed(self) -> Flow<B, Cleanup> {
        Flow::wrap(self.inner)
    }
}

fn can_redirect_auth_header(prev: &Uri, next: &Uri) -> bool {
    let host_prev = prev.authority().map(|a| a.host());
    let host_next = next.authority().map(|a| a.host());
    let scheme_prev = prev.scheme();
    let scheme_next = next.scheme();
    host_prev == host_next && (scheme_prev == scheme_next || scheme_next == Some(&Scheme::HTTPS))
}

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
