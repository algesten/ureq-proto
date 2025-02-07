use std::fmt;
use std::io::Write;
use std::marker::PhantomData;

use http::{Request, Response, StatusCode, Version};

use crate::body::{BodyReader, BodyWriter};
use crate::client::do_write_headers;
use crate::parser::try_parse_request;
use crate::util::{log_data, Writer};
use crate::Error;

use super::amended::AmendedResponse;
use super::MAX_REQUEST_HEADERS;

pub mod state {
    /// Type state for receiving a new HTTP Request.
    pub struct RecvRequest(());

    /// Type state when client sent Expect: 100-continue.
    pub struct Send100(());

    /// Type state for receiving a request body.
    pub struct RecvBody(());

    /// Type state for sending a response.
    pub struct SendResponse(());

    /// Type state for responses without bodies.
    pub struct WithoutBody(());

    /// Type state for responses with bodies.
    pub struct WithBody(());
}
use self::state::*;

pub struct Call<State, B = ()> {
    phase: Phase,
    analyzed: bool,
    state: BodyState,
    response: Option<AmendedResponse<B>>,
    _ph: PhantomData<State>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Phase {
    RecvRequest,
    Send100,
    RecvBody,
    SendStatus,
    SendHeaders(usize),
    SendBody,
}

impl Phase {
    fn is_prelude(&self) -> bool {
        matches!(self, Phase::SendStatus | Phase::SendHeaders(_))
    }

    fn is_body(&self) -> bool {
        matches!(self, Phase::SendBody)
    }
}

#[derive(Debug, Default)]
pub(crate) struct BodyState {
    reader: Option<BodyReader>,
    writer: Option<BodyWriter>,
    stop_on_chunk_boundary: bool,
}

impl Call<RecvRequest> {
    pub fn new() -> Self {
        Call::default()
    }

    /// Try reading request headers
    ///
    /// A request is only possible once the `input` holds all the HTTP request
    /// headers. Before that this returns `None`. When the request is succesfully read,
    /// the return value `(usize, Request<()>)` contains how many bytes were consumed
    /// of the `input`.
    pub fn try_request(&mut self, input: &[u8]) -> Result<Option<(usize, Request<()>)>, Error> {
        // ~3k for 100 headers
        let (input_used, request) = match try_parse_request::<MAX_REQUEST_HEADERS>(input)? {
            Some(v) => v,
            None => {
                return Ok(None);
            }
        };

        log_data(&input[..input_used]);

        let http10 = request.version() == Version::HTTP_10;
        let method = request.method();

        let header_lookup = |name: http::HeaderName| {
            if let Some(header) = request.headers().get(name) {
                return header.to_str().ok();
            }
            None
        };

        let reader = BodyReader::for_request(http10, method, &header_lookup)?;
        self.state.reader = Some(reader);

        Ok(Some((input_used, request)))
    }

    pub fn into_send_100(self) -> Call<Send100> {
        assert!(self.state.reader.is_some());

        Call {
            phase: Phase::Send100,
            state: self.state,
            ..Default::default()
        }
    }

    pub fn into_body(self) -> Call<RecvBody> {
        assert!(self.state.reader.is_some());

        Call {
            phase: Phase::RecvBody,
            state: self.state,
            ..Default::default()
        }
    }

    pub fn into_send_response(self) -> Call<SendResponse> {
        assert!(self.state.reader.is_some());

        Call {
            phase: Phase::SendStatus,
            state: self.state,
            ..Default::default()
        }
    }
}

impl Call<Send100> {
    pub fn send_100(&mut self, output: &mut [u8]) -> Result<usize, Error> {
        let mut w = Writer::new(output);

        do_write_send_line((Version::HTTP_11, StatusCode::CONTINUE), &mut w, true);

        let output_used = w.len();

        Ok(output_used)
    }

    pub fn into_recv_body(self) -> Call<RecvBody> {
        Call {
            phase: Phase::RecvBody,
            state: self.state,
            ..Default::default()
        }
    }

    pub fn reject<B>(self, response: Response<B>) -> Result<Call<WithBody, B>, Error> {
        append_request(self, response, BodyWriter::new_chunked())
    }
}

impl Call<RecvBody> {
    /// Read the input as a request body
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

    /// Tell if the request body is over
    pub fn is_ended(&self) -> bool {
        let rbm = self.state.reader.as_ref().unwrap();
        rbm.is_ended()
    }

    pub fn into_send_response(self) -> Call<SendResponse> {
        assert!(self.is_ended());

        Call {
            phase: Phase::SendStatus,
            state: self.state,
            ..Default::default()
        }
    }
}

impl Call<SendResponse> {
    /// Proceeds sending a response without a body.
    pub fn without_body<B>(self, response: Response<B>) -> Result<Call<WithoutBody, B>, Error> {
        append_request(self, response, BodyWriter::new_none())
    }

    /// Creates a call for a [`Method`] that requires a request body
    ///
    /// Methods like `POST` and `PUT` expects a request body. This must be
    /// used even if the body is zero-sized (`content-length: 0`).
    pub fn with_body<B>(self, response: Response<B>) -> Result<Call<WithBody, B>, Error> {
        append_request(self, response, BodyWriter::new_chunked())
    }
}

fn append_request<S0, B0, State, B>(
    call: Call<S0, B0>,
    response: Response<B>,
    default_body_mode: BodyWriter,
) -> Result<Call<State, B>, Error> {
    Ok(Call {
        phase: Phase::SendStatus,
        state: BodyState {
            writer: Some(default_body_mode),
            ..call.state
        },
        response: Some(AmendedResponse::new(response)),
        ..Default::default()
    })
}

impl<State, B> Call<State, B> {
    pub(crate) fn analyze_request(&mut self) -> Result<(), Error> {
        if self.analyzed {
            return Ok(());
        }

        // unwrap are correct due to state we should be in when we get here.
        let response = self.response.as_mut().unwrap();
        let writer = self.state.writer.unwrap();

        let info = response.analyze(writer)?;

        if !info.res_body_header && info.body_mode.has_body() {
            // User did not set a body header, we set one.
            let header = info.body_mode.body_header();
            response.set_header(header.0, header.1)?;
        }

        self.state.writer = Some(info.body_mode);

        self.analyzed = true;
        Ok(())
    }
}

impl<B> Call<WithoutBody, B> {
    pub fn write(&mut self, output: &mut [u8]) -> Result<usize, Error> {
        self.analyze_request()?;

        // unwrap is ok because we are not here without providing it
        let response = self.response.as_ref().unwrap();

        let mut w = Writer::new(output);
        try_write_prelude(response, &mut self.phase, &mut w)?;

        let output_used = w.len();

        Ok(output_used)
    }

    /// Whether the response has been fully written.
    pub fn is_finished(&self) -> bool {
        !self.phase.is_prelude()
    }
}

impl<B> Call<WithBody, B> {
    pub fn write(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        self.analyze_request()?;

        // unwrap is ok because we are not here without providing it
        let response = self.response.as_ref().unwrap();
        // unwrap is ok because analyze_request() sets it.
        let writer = self.state.writer.as_mut().unwrap();

        let mut w = Writer::new(output);

        let mut input_used = 0;

        if self.phase.is_prelude() {
            try_write_prelude(response, &mut self.phase, &mut w)?;
        } else if self.phase.is_body() {
            if !input.is_empty() && writer.is_ended() {
                return Err(Error::BodyContentAfterFinish);
            }
            if let Some(left) = writer.left_to_send() {
                if input.len() as u64 > left {
                    return Err(Error::BodyLargerThanContentLength);
                }
            }
            input_used = writer.write(input, &mut w);
        }

        let output_used = w.len();

        Ok((input_used, output_used))
    }

    pub(crate) fn consume_direct_write(&mut self, amount: usize) -> Result<(), Error> {
        // unwrap is ok because we must have called analyze in above write()
        // to use consume_direct_write()
        let writer = self.state.writer.as_mut().unwrap();

        if let Some(left) = writer.left_to_send() {
            if amount as u64 > left {
                return Err(Error::BodyLargerThanContentLength);
            }
        } else {
            return Err(Error::BodyIsChunked);
        }

        writer.consume_direct_write(amount);

        Ok(())
    }

    pub fn is_finished(&self) -> bool {
        self.state.writer.map(|w| w.is_ended()).unwrap_or_default()
    }
}

fn try_write_prelude<B>(
    response: &AmendedResponse<B>,
    phase: &mut Phase,
    w: &mut Writer,
) -> Result<(), Error> {
    let at_start = w.len();

    loop {
        if try_write_prelude_part(response, phase, w) {
            continue;
        }

        let written = w.len() - at_start;

        if written > 0 || phase.is_body() {
            return Ok(());
        } else {
            return Err(Error::OutputOverflow);
        }
    }
}

fn try_write_prelude_part<Body>(
    response: &AmendedResponse<Body>,
    phase: &mut Phase,
    w: &mut Writer,
) -> bool {
    match phase {
        Phase::SendStatus => {
            let success = do_write_send_line(response.prelude(), w, false);
            if success {
                *phase = Phase::SendHeaders(0);
            }
            success
        }

        Phase::SendHeaders(index) => {
            let header_count = response.headers_len();
            let all = response.headers();
            let skipped = all.skip(*index);

            do_write_headers(skipped, index, header_count - 1, w);

            if *index == header_count {
                *phase = Phase::SendBody;
            }
            false
        }

        // We're past the header.
        _ => false,
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

impl<State, B> fmt::Debug for Call<State, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Call").field("phase", &self.phase).finish()
    }
}

impl fmt::Debug for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Phase::RecvRequest => write!(f, "RecvRequest"),
            Phase::Send100 => write!(f, "Send100"),
            Phase::RecvBody => write!(f, "RecvBody"),
            Phase::SendStatus => write!(f, "SendStatus"),
            Phase::SendHeaders(_) => write!(f, "SendHeaders"),
            Phase::SendBody => write!(f, "SendBody"),
        }
    }
}

impl<S, B> Default for Call<S, B> {
    fn default() -> Self {
        Self {
            phase: Phase::RecvRequest,
            analyzed: Default::default(),
            state: Default::default(),
            response: Default::default(),
            _ph: Default::default(),
        }
    }
}
