//! A single request-response. No redirection or other logic.

use std::fmt;
use std::io::Write;
use std::marker::PhantomData;

use base64::prelude::BASE64_STANDARD;
use base64::Engine;
use http::uri::Scheme;
use http::{header, HeaderName, HeaderValue, Method, Request, Response, StatusCode, Uri, Version};

use crate::body::{BodyReader, BodyWriter};
use crate::ext::SchemeExt;
use crate::parser::{try_parse_partial_response, try_parse_response};
use crate::util::{log_data, AuthorityExt, Writer};
use crate::{BodyMode, Error};

use super::amended::AmendedRequest;
use super::MAX_RESPONSE_HEADERS;

#[doc(hidden)]
pub mod state {
    /// Type state for requests without bodies via [`Call::without_body()`]
    #[doc(hidden)]
    pub struct WithoutBody(());

    /// Type state for streaming bodies via [`Call::with_streaming_body()`]
    #[doc(hidden)]
    pub struct WithBody(());

    /// Type state for receiving the HTTP Response
    #[doc(hidden)]
    pub struct RecvResponse(());

    /// Type state for receiving the response body
    #[doc(hidden)]
    pub struct RecvBody(());
}
use self::state::*;

/// An HTTP/1.1 call
///
/// This handles a single request-response including sending and receiving bodies.
/// It does not follow redirects, handle body transformations (such as compression),
/// connection handling or agent state (cookies).
///
/// In scope is everything to do with the actual transfer:
///
/// 1. `Method` dictating whether request and response having a body.
/// 2. Whether we are sending Content-Length delimited data or chunked
/// 3. `Host` header if not set (TODO(martin): this does not really belong here?)
/// 4. Writing and reading the request/response in a Sans-IO style.
///
pub struct Call<State, B> {
    request: AmendedRequest<B>,
    analyzed: bool,
    state: BodyState,
    _ph: PhantomData<State>,
}

impl<B> Call<(), B> {
    /// Creates a call for a [`Method`] that do not have a request body
    ///
    /// Methods like `HEAD` and `GET` do not use a request body. This creates
    /// a [`Call`] instance that does not expect a user provided body.
    ///
    /// ```
    /// # use ureq_proto::client::call::Call;
    /// # use ureq_proto::http::Request;
    /// let req = Request::head("http://foo.test/page").body(()).unwrap();
    /// Call::without_body(req).unwrap();
    /// ```
    pub fn without_body(request: Request<B>) -> Result<Call<WithoutBody, B>, Error> {
        Call::new(request, BodyWriter::new_none())
    }

    /// Creates a call for a [`Method`] that requires a request body
    ///
    /// Methods like `POST` and `PUT` expects a request body. This must be
    /// used even if the body is zero-sized (`content-length: 0`).
    ///
    /// ```
    /// # use ureq_proto::client::call::Call;
    /// # use ureq_proto::http::Request;
    /// let req = Request::put("http://foo.test/path").body(()).unwrap();
    /// Call::with_body(req).unwrap();
    /// ```
    pub fn with_body(request: Request<B>) -> Result<Call<WithBody, B>, Error> {
        Call::new(request, BodyWriter::new_chunked())
    }
}

impl<State, B> Call<State, B> {
    fn new(request: Request<B>, default_body_mode: BodyWriter) -> Result<Self, Error> {
        let request = AmendedRequest::new(request);

        Ok(Call {
            request,
            analyzed: false,
            state: BodyState {
                writer: default_body_mode,
                ..Default::default()
            },
            _ph: PhantomData,
        })
    }

    pub(crate) fn analyze_request(&mut self) -> Result<(), Error> {
        if self.analyzed {
            return Ok(());
        }

        let info = self.request.analyze(
            self.state.writer,
            self.state.skip_method_body_check,
            self.state.allow_non_standard_methods,
        )?;

        if !info.req_host_header {
            if let Some(host) = self.request.uri().host() {
                // User did not set a host header, and there is one in uri, we set that.
                // We need an owned value to set the host header.

                // This might append the port if it differs from the scheme default.
                let value = maybe_with_port(host, self.request.uri())?;

                self.request.set_header(header::HOST, value)?;
            }
        }

        if let Some(auth) = self.request.uri().authority() {
            if auth.userinfo().is_some() && !info.req_auth_header {
                let user = auth.username().unwrap_or_default();
                let pass = auth.password().unwrap_or_default();
                let creds = BASE64_STANDARD.encode(format!("{}:{}", user, pass));
                let auth = format!("Basic {}", creds);
                self.request.set_header(header::AUTHORIZATION, auth)?;
            }
        }

        if !info.req_body_header && info.body_mode.has_body() {
            // User did not set a body header, we set one.
            let header = info.body_mode.body_header();
            self.request.set_header(header.0, header.1)?;
        }

        self.state.writer = info.body_mode;

        self.analyzed = true;
        Ok(())
    }

    fn do_into_receive(self) -> Result<Call<RecvResponse, B>, Error> {
        if !self.state.writer.is_ended() {
            return Err(Error::UnfinishedRequest);
        }

        Ok(Call {
            request: self.request,
            analyzed: self.analyzed,
            state: BodyState {
                phase: Phase::RecvResponse,
                ..self.state
            },
            _ph: PhantomData,
        })
    }

    pub(crate) fn amended(&self) -> &AmendedRequest<B> {
        &self.request
    }

    pub(crate) fn amended_mut(&mut self) -> &mut AmendedRequest<B> {
        &mut self.request
    }

    pub(crate) fn body_mode(&self) -> BodyMode {
        self.state
            .reader
            .map(|r| r.body_mode())
            .unwrap_or(BodyMode::Chunked)
    }
}

fn maybe_with_port(host: &str, uri: &Uri) -> Result<HeaderValue, Error> {
    fn from_str(src: &str) -> Result<HeaderValue, Error> {
        HeaderValue::from_str(src).map_err(|e| Error::BadHeader(e.to_string()))
    }

    if let Some(port) = uri.port() {
        let scheme = uri.scheme().unwrap_or(&Scheme::HTTP);
        if let Some(scheme_default) = scheme.default_port() {
            if port != scheme_default {
                // This allocates, so we only do it if we absolutely have to.
                let host_port = format!("{}:{}", host, port);
                return from_str(&host_port);
            }
        }
    }

    // Fall back on no port (without allocating).
    from_str(host)
}

#[derive(Debug, Default)]
pub(crate) struct BodyState {
    phase: Phase,
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
enum Phase {
    SendLine,
    SendHeaders(usize),
    SendBody,
    RecvResponse,
    RecvBody,
}

impl Default for Phase {
    fn default() -> Self {
        Self::SendLine
    }
}

impl Phase {
    fn is_prelude(&self) -> bool {
        matches!(self, Phase::SendLine | Phase::SendHeaders(_))
    }

    fn is_body(&self) -> bool {
        matches!(self, Phase::SendBody)
    }
}

impl<B> Call<WithoutBody, B> {
    pub(crate) fn into_send_body(mut self) -> Call<WithBody, B> {
        assert!(!self.analyzed);

        self.state.skip_method_body_check = true;

        Call {
            request: self.request,
            analyzed: self.analyzed,
            state: self.state,
            _ph: PhantomData,
        }
    }

    /// Set whether to allow non-standard HTTP methods.
    ///
    /// By default the methods are limited by the HTTP version.
    ///
    /// Must be set before calling write()
    pub fn allow_non_standard_methods(&mut self, v: bool) {
        self.state.allow_non_standard_methods = v;
    }

    /// Write the request to the output buffer
    ///
    /// Returns how much of the output buffer that was used.
    ///
    /// ```
    /// # use ureq_proto::client::call::Call;
    /// # use ureq_proto::http::Request;
    /// let req = Request::head("http://foo.test/page").body(()).unwrap();
    /// let mut call = Call::without_body(req).unwrap();
    ///
    /// let mut output = vec![0; 1024];
    /// let n = call.write(&mut output).unwrap();
    /// let s = std::str::from_utf8(&output[..n]).unwrap();
    ///
    /// assert_eq!(s, "HEAD /page HTTP/1.1\r\nhost: foo.test\r\n\r\n");
    /// ```
    pub fn write(&mut self, output: &mut [u8]) -> Result<usize, Error> {
        self.analyze_request()?;

        let mut w = Writer::new(output);
        try_write_prelude(&self.request, &mut self.state, &mut w)?;

        let output_used = w.len();

        Ok(output_used)
    }

    /// Whether the request has been fully written.
    pub fn is_finished(&self) -> bool {
        !self.state.phase.is_prelude()
    }

    /// Proceed to receiving a response
    ///
    /// Once the request is finished writing, proceed to receiving a response. Will error
    /// if [`Call::is_finished()`] returns `false`.
    pub fn into_receive(self) -> Result<Call<RecvResponse, B>, Error> {
        self.do_into_receive()
    }
}

impl<B> Call<WithBody, B> {
    /// Write a request, and consecutive body to the output buffer
    ///
    /// The first argument `input` is the body input buffer. If the request contained
    /// a `content-length` header, this buffer must be smaller or same size as that
    /// `content-length`.
    ///
    /// When doing `transfer-encoding: chunked` signal the end of the body by
    /// providing the input `&[]`.
    ///
    /// Returns `(usize, usize)` where the first number is how many bytes of the `input` that
    /// were consumed. The second number is how many bytes of the `output` that were used.
    ///
    /// ```
    /// # use ureq_proto::client::call::Call;
    /// # use ureq_proto::http::Request;
    /// let req = Request::post("http://f.test/page")
    ///     .header("transfer-encoding", "chunked")
    ///     .body(())
    ///     .unwrap();
    /// let mut call = Call::with_body(req).unwrap();
    ///
    /// let body = b"hallo";
    ///
    /// // Send body
    /// let mut output = vec![0; 1024];
    /// let (_, n1) = call.write(body, &mut output).unwrap(); // send headers
    /// let (i, n2) = call.write(body, &mut output[n1..]).unwrap(); // send body
    /// assert_eq!(i, 5);
    ///
    /// // Indicate the body is finished by sending &[]
    /// let (_, n3) = call.write(&[], &mut output[n1 + n2..]).unwrap();
    /// let s = std::str::from_utf8(&output[..n1 + n2 + n3]).unwrap();
    ///
    /// assert_eq!(
    ///     s,
    ///     "POST /page HTTP/1.1\r\nhost: f.test\r\n\
    ///     transfer-encoding: chunked\r\n\r\n5\r\nhallo\r\n0\r\n\r\n"
    /// );
    /// ```
    pub fn write(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        self.analyze_request()?;

        let mut w = Writer::new(output);

        let mut input_used = 0;

        if self.is_prelude() {
            try_write_prelude(&self.request, &mut self.state, &mut w)?;
        } else if self.is_body() {
            if !input.is_empty() && self.state.writer.is_ended() {
                return Err(Error::BodyContentAfterFinish);
            }
            if let Some(left) = self.state.writer.left_to_send() {
                if input.len() as u64 > left {
                    return Err(Error::BodyLargerThanContentLength);
                }
            }
            input_used = self.state.writer.write(input, &mut w);
        }

        let output_used = w.len();

        Ok((input_used, output_used))
    }

    pub(crate) fn consume_direct_write(&mut self, amount: usize) -> Result<(), Error> {
        if let Some(left) = self.state.writer.left_to_send() {
            if amount as u64 > left {
                return Err(Error::BodyLargerThanContentLength);
            }
        } else {
            return Err(Error::BodyIsChunked);
        }

        self.state.writer.consume_direct_write(amount);

        Ok(())
    }

    pub(crate) fn is_prelude(&self) -> bool {
        self.state.phase.is_prelude()
    }

    pub(crate) fn is_body(&self) -> bool {
        self.state.phase.is_body()
    }

    pub(crate) fn is_chunked(&self) -> bool {
        self.state.writer.is_chunked()
    }

    /// Set whether to allow non-standard HTTP methods.
    ///
    /// By default the methods are limited by the HTTP version.
    ///
    /// Must be set before calling write()
    pub fn allow_non_standard_methods(&mut self, v: bool) {
        self.state.allow_non_standard_methods = v;
    }

    /// Tell if the request and body has been sent.
    pub fn is_finished(&self) -> bool {
        self.state.writer.is_ended()
    }

    /// Proceed to receiving a response
    ///
    /// Once the request is finished writing, proceed to receiving a response. Will error
    /// if [`Call::is_finished()`] returns `false`.
    pub fn into_receive(self) -> Result<Call<RecvResponse, B>, Error> {
        self.do_into_receive()
    }
}

fn try_write_prelude<B>(
    request: &AmendedRequest<B>,
    state: &mut BodyState,
    w: &mut Writer,
) -> Result<(), Error> {
    let at_start = w.len();

    loop {
        if try_write_prelude_part(request, state, w) {
            continue;
        }

        let written = w.len() - at_start;

        if written > 0 || state.phase.is_body() {
            return Ok(());
        } else {
            return Err(Error::OutputOverflow);
        }
    }
}

fn try_write_prelude_part<Body>(
    request: &AmendedRequest<Body>,
    state: &mut BodyState,
    w: &mut Writer,
) -> bool {
    match &mut state.phase {
        Phase::SendLine => {
            let success = do_write_send_line(request.prelude(), w);
            if success {
                state.phase = Phase::SendHeaders(0);
            }
            success
        }

        Phase::SendHeaders(index) => {
            let header_count = request.headers_len();
            let all = request.headers();
            let skipped = all.skip(*index);

            do_write_headers(skipped, index, header_count - 1, w);

            if *index == header_count {
                state.phase = Phase::SendBody;
            }
            false
        }

        // We're past the header.
        _ => false,
    }
}

fn do_write_send_line(line: (&Method, &str, Version), w: &mut Writer) -> bool {
    w.try_write(|w| write!(w, "{} {} {:?}\r\n", line.0, line.1, line.2))
}

fn do_write_headers<'a, I>(headers: I, index: &mut usize, last_index: usize, w: &mut Writer)
where
    I: Iterator<Item = (&'a HeaderName, &'a HeaderValue)>,
{
    for h in headers {
        let success = w.try_write(|w| {
            write!(w, "{}: ", h.0)?;
            w.write_all(h.1.as_bytes())?;
            write!(w, "\r\n")?;
            if *index == last_index {
                write!(w, "\r\n")?;
            }
            Ok(())
        });

        if success {
            *index += 1;
        } else {
            break;
        }
    }
}

impl<B> Call<RecvResponse, B> {
    /// Try reading response headers
    ///
    /// A response is only possible once the `input` holds all the HTTP response
    /// headers. Before that this returns `None`. When the response is succesfully read,
    /// the return value `(usize, Response<()>)` contains how many bytes were consumed
    /// of the `input`.
    ///
    /// Once the response headers are succesfully read, use [`Call::into_body()`] to proceed
    /// reading the response body.
    pub fn try_response(
        &mut self,
        input: &[u8],
        allow_partial_redirect: bool,
    ) -> Result<Option<(usize, Response<()>)>, Error> {
        // ~3k for 100 headers
        let (input_used, response) = match try_parse_response::<MAX_RESPONSE_HEADERS>(input)? {
            Some(v) => v,
            None => {
                // The caller decides whether to allow a partial parse.
                if !allow_partial_redirect {
                    return Ok(None);
                }

                // TODO(martin): I don't like this code. The mission is to be correct HTTP/1.1
                // and this is a hack to allow for broken servers.
                //
                // As a special case, to handle broken servers that does a redirect without
                // the final trailing \r\n, we try parsing the response as partial, and
                // if it is a redirect, we can allow the request to continue.
                let Some(mut r) = try_parse_partial_response::<MAX_RESPONSE_HEADERS>(input)? else {
                    return Ok(None);
                };

                // A redirection must have a location header.
                let is_complete_redirection =
                    r.status().is_redirection() && r.headers().contains_key(header::LOCATION);

                if !is_complete_redirection {
                    return Ok(None);
                }

                // Insert a synthetic connection: close, since the connection is
                // not valid after using a partial request.
                debug!("Partial redirection response, insert fake connection: close");
                r.headers_mut()
                    .insert(header::CONNECTION, HeaderValue::from_static("close"));

                (input.len(), r)
            }
        };

        log_data(&input[..input_used]);

        let http10 = response.version() == Version::HTTP_10;
        let status = response.status().as_u16();

        if status == StatusCode::CONTINUE {
            // There should be no headers for this response.
            if !response.headers().is_empty() {
                return Err(Error::HeadersWith100);
            }

            return Ok(Some((input_used, response)));
        }

        let header_lookup = |name: HeaderName| {
            if let Some(header) = response.headers().get(name) {
                return header.to_str().ok();
            }
            None
        };

        let recv_body_mode =
            BodyReader::for_response(http10, self.request.method(), status, &header_lookup)?;

        self.state.reader = Some(recv_body_mode);

        Ok(Some((input_used, response)))
    }

    /// Tell if the response has been received.
    pub fn is_finished(&self) -> bool {
        self.state.reader.is_some()
    }

    /// Continue reading the response body
    ///
    /// Errors if called before [`Call::try_response()`] has produced a [`Response`].
    ///
    /// Returns `None` if there is no body such as the response to a `HEAD` request.
    pub fn into_body(self) -> Result<Option<Call<RecvBody, B>>, Error> {
        let rbm = match &self.state.reader {
            Some(v) => v,
            None => return Err(Error::IncompleteResponse),
        };

        // No body is expected either due to Method or status. Call ends here.
        if matches!(rbm, BodyReader::NoBody) {
            return Ok(None);
        }

        let next = self.do_into_body();

        Ok(Some(next))
    }

    pub(crate) fn need_response_body(&self) -> bool {
        self.state.need_response_body()
    }

    pub(crate) fn do_into_body(self) -> Call<RecvBody, B> {
        Call {
            request: self.request,
            analyzed: self.analyzed,
            state: BodyState {
                phase: Phase::RecvBody,
                ..self.state
            },
            _ph: PhantomData,
        }
    }
}

impl<B> Call<RecvBody, B> {
    /// Read the input as a response body
    ///
    /// Returns `(usize, usize)` where the first number is how many bytes of the input was used
    /// and the second number how many of the output.
    pub fn read(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        let rbm = self.state.reader.as_mut().unwrap();

        if rbm.is_ended() {
            return Ok((0, 0));
        }

        rbm.read(input, output, self.state.stop_on_chunk_boundary)
    }

    /// Set whether we are stopping on chunk boundaries.
    ///
    /// If `false`, we are trying to fill the entire `output` in each `read()` call.
    ///
    /// Defaults to `false`.
    pub fn stop_on_chunk_boundary(&mut self, enabled: bool) {
        self.state.stop_on_chunk_boundary = enabled;
    }

    /// Tell if we are currently on a chunk boundary.
    ///
    /// Only relevant if we are first enabling `stop_on_chunk_boundary()`.
    pub fn is_on_chunk_boundary(&self) -> bool {
        let rbm = self.state.reader.as_ref().unwrap();
        rbm.is_on_chunk_boundary()
    }

    /// Tell if the response is over
    pub fn is_ended(&self) -> bool {
        let rbm = self.state.reader.as_ref().unwrap();
        rbm.is_ended()
    }

    /// Tell if response body is closed delimited
    ///
    /// HTTP/1.0 does not have `content-length` to serialize many requests over the same
    /// socket. Instead it uses socket close to determine the body is finished.
    pub fn is_close_delimited(&self) -> bool {
        let rbm = self.state.reader.as_ref().unwrap();
        matches!(rbm, BodyReader::CloseDelimited)
    }

    // /// Continue to reading trailer headers
    // pub fn into_trailer(self) -> Result<Option<Call<'b, Trailer>>, Error> {
    //     todo!()
    // }
}

// pub struct Trailer(());

impl<State, B> fmt::Debug for Call<State, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Call")
            .field("phase", &self.state.phase)
            .finish()
    }
}

impl fmt::Debug for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SendLine => write!(f, "SendLine"),
            Self::SendHeaders(_) => write!(f, "SendHeaders"),
            Self::SendBody => write!(f, "SendBody"),
            Self::RecvResponse => write!(f, "RecvResponse"),
            Self::RecvBody => write!(f, "RecvBody"),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::str;

    use http::{Method, Request};

    use crate::Error;

    #[test]
    fn ensure_send_sync() {
        fn is_send_sync<T: Send + Sync>(_t: T) {}

        is_send_sync(Call::without_body(Request::new(())).unwrap());

        is_send_sync(Call::with_body(Request::post("/").body(()).unwrap()).unwrap());
    }

    #[test]
    fn create_empty() {
        let req = Request::builder().body(()).unwrap();
        let _call = Call::without_body(req);
    }

    #[test]
    fn create_streaming() {
        let req = Request::builder().body(()).unwrap();
        let _call = Call::with_body(req);
    }

    #[test]
    fn head_simple() {
        let req = Request::head("http://foo.test/page").body(()).unwrap();
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();
        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(s, "HEAD /page HTTP/1.1\r\nhost: foo.test\r\n\r\n");
    }

    #[test]
    fn head_with_body() {
        let req = Request::head("http://foo.test/page").body(()).unwrap();
        let mut call = Call::with_body(req).unwrap();

        let err = call.write(&[], &mut []).unwrap_err();

        assert_eq!(err, Error::MethodForbidsBody(Method::HEAD));
    }

    #[test]
    fn post_simple() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 5)
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (i1, n1) = call.write(b"hallo", &mut output).unwrap();
        let (i2, n2) = call.write(b"hallo", &mut output[n1..]).unwrap();
        assert_eq!(i1, 0);
        assert_eq!(i2, 5);
        assert_eq!(n1, 56);
        assert_eq!(n2, 5);
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
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];

        let body = b"hallo";

        {
            let (i, n) = call.write(body, &mut output[..25]).unwrap();
            assert_eq!(i, 0);
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "POST /page HTTP/1.1\r\n");
            assert!(!call.is_finished());
        }

        {
            let (i, n) = call.write(body, &mut output[..20]).unwrap();
            assert_eq!(i, 0);
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "host: f.test\r\n");
            assert!(!call.is_finished());
        }

        {
            let (i, n) = call.write(body, &mut output[..21]).unwrap();
            assert_eq!(i, 0);
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "content-length: 5\r\n\r\n");
            assert!(!call.is_finished());
        }

        {
            let (i, n) = call.write(body, &mut output[..25]).unwrap();
            assert_eq!(n, 5);
            assert_eq!(i, 5);
            let s = str::from_utf8(&output[..n]).unwrap();
            assert_eq!(s, "hallo");
            assert!(call.is_finished());
        }
    }

    #[test]
    fn post_with_short_content_length() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 2)
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let body = b"hallo";

        let mut output = vec![0; 1024];
        let r = call.write(body, &mut output);
        assert!(r.is_ok());

        let r = call.write(body, &mut output);

        assert_eq!(r.unwrap_err(), Error::BodyLargerThanContentLength);
    }

    #[test]
    fn post_with_short_body_input() {
        let req = Request::post("http://f.test/page")
            .header("content-length", 5)
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (i1, n1) = call.write(b"ha", &mut output).unwrap();
        let (i2, n2) = call.write(b"ha", &mut output[n1..]).unwrap();
        assert_eq!(i1, 0);
        assert_eq!(i2, 2);
        assert_eq!(n1, 56);
        assert_eq!(n2, 2);
        let s = str::from_utf8(&output[..n1 + n2]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nha"
        );

        assert!(!call.is_finished());

        let (i, n2) = call.write(b"llo", &mut output).unwrap();
        assert_eq!(i, 3);
        let s = str::from_utf8(&output[..n2]).unwrap();

        assert_eq!(s, "llo");

        assert!(call.is_finished());
    }

    #[test]
    fn post_with_chunked() {
        let req = Request::post("http://f.test/page")
            .header("transfer-encoding", "chunked")
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let body = b"hallo";

        let mut output = vec![0; 1024];
        let (i1, n1) = call.write(body, &mut output).unwrap();
        let (i2, n2) = call.write(body, &mut output[n1..]).unwrap();
        let (_, n3) = call.write(&[], &mut output[n1 + n2..]).unwrap();
        assert_eq!(i1, 0);
        assert_eq!(i2, 5);
        assert_eq!(n1, 65);
        assert_eq!(n2, 10);
        assert_eq!(n3, 5);
        let s = str::from_utf8(&output[..n1 + n2 + n3]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ntransfer-encoding: chunked\
                \r\n\r\n5\r\nhallo\r\n0\r\n\r\n"
        );
    }

    #[test]
    fn post_without_body() {
        let req = Request::post("http://foo.test/page").body(()).unwrap();
        let mut call = Call::without_body(req).unwrap();

        let err = call.write(&mut []).unwrap_err();

        assert_eq!(err, Error::MethodRequiresBody(Method::POST));
    }

    #[test]
    fn post_streaming() {
        let req = Request::post("http://f.test/page").body(()).unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (i1, n1) = call.write(b"hallo", &mut output).unwrap();
        let (i2, n2) = call.write(b"hallo", &mut output[n1..]).unwrap();

        // Send end
        let (i3, n3) = call.write(&[], &mut output[n1 + n2..]).unwrap();

        assert_eq!(i1, 0);
        assert_eq!(i2, 5);
        assert_eq!(n1, 65);
        assert_eq!(n2, 10);
        assert_eq!(i3, 0);
        assert_eq!(n3, 5);

        let s = str::from_utf8(&output[..(n1 + n2 + n3)]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ntransfer-encoding: chunked\
                \r\n\r\n5\r\nhallo\r\n0\r\n\r\n"
        );
    }

    #[test]
    fn post_streaming_with_size() {
        let req = Request::post("http://f.test/page")
            .header("content-length", "5")
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (i1, n1) = call.write(b"hallo", &mut output).unwrap();
        let (i2, n2) = call.write(b"hallo", &mut output[n1..]).unwrap();
        assert_eq!(i1, 0);
        assert_eq!(n1, 56);
        assert_eq!(i2, 5);
        assert_eq!(n2, 5);

        let s = str::from_utf8(&output[..(n1 + n2)]).unwrap();

        assert_eq!(
            s,
            "POST /page HTTP/1.1\r\nhost: f.test\r\ncontent-length: 5\r\n\r\nhallo"
        );
    }

    #[test]
    fn post_streaming_after_end() {
        let req = Request::post("http://f.test/page").body(()).unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (_, n1) = call.write(b"hallo", &mut output).unwrap();

        // Send end
        let (_, n2) = call.write(&[], &mut output[n1..]).unwrap();

        let err = call.write(b"after end", &mut output[(n1 + n2)..]);

        assert_eq!(err, Err(Error::BodyContentAfterFinish));
    }

    #[test]
    fn post_streaming_too_much() {
        let req = Request::post("http://f.test/page")
            .header("content-length", "5")
            .body(())
            .unwrap();
        let mut call = Call::with_body(req).unwrap();

        let mut output = vec![0; 1024];
        let (_, n1) = call.write(b"hallo", &mut output).unwrap();
        let (_, n2) = call.write(b"hallo", &mut output[n1..]).unwrap();

        // this is more than content-length
        let err = call.write(b"fail", &mut output[n1 + n2..]).unwrap_err();

        assert_eq!(err, Error::BodyContentAfterFinish);
    }

    #[test]
    fn username_password_uri() {
        let req = Request::get("http://martin:secret@f.test/page")
            .body(())
            .unwrap();
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

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
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

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
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

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
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

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
        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

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

        let mut call = Call::without_body(req).unwrap();

        let mut output = vec![0; 1024];
        let err = call.write(&mut output).unwrap_err();

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

        let mut call = Call::without_body(req).unwrap();

        call.allow_non_standard_methods(true);

        let mut output = vec![0; 1024];
        let n = call.write(&mut output).unwrap();

        let s = str::from_utf8(&output[..n]).unwrap();

        assert_eq!(s, "FNORD /page HTTP/1.1\r\nhost: f.test:8080\r\n\r\n");
    }
}
