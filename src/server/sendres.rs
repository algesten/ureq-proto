use crate::client::do_write_headers;
use crate::util::Writer;
use crate::Error;

use super::state::{SendBody, SendResponse};
use super::{do_write_send_line, Reply, ResponsePhase};

impl<B> Reply<SendResponse, B> {
    /// Write the response headers to the output buffer.
    ///
    /// Returns the number of bytes written to the output buffer.
    pub fn write(&mut self, output: &mut [u8]) -> Result<usize, Error> {
        // unwrap is ok because we are not here without providing it
        let response = self.inner.response.as_ref().unwrap();

        let mut w = Writer::new(output);
        try_write_prelude(response, &mut self.inner.phase, &mut w)?;

        let output_used = w.len();

        Ok(output_used)
    }

    /// Whether the response has been fully written.
    pub fn is_finished(&self) -> bool {
        !self.inner.phase.is_prelude()
    }

    /// Proceed to sending a response body.
    ///
    /// This is only possible when the response headers are fully written.
    pub fn proceed(self) -> Reply<SendBody, B> {
        assert!(self.is_finished());

        let inner = self.inner;
        Reply::wrap(inner)
    }
}

fn try_write_prelude<B>(
    response: &super::amended::AmendedResponse<B>,
    phase: &mut ResponsePhase,
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
    response: &super::amended::AmendedResponse<Body>,
    phase: &mut ResponsePhase,
    w: &mut Writer,
) -> bool {
    match phase {
        ResponsePhase::Status => {
            let success = do_write_send_line(response.prelude(), w, false);
            if success {
                *phase = ResponsePhase::Headers(0);
            }
            success
        }

        ResponsePhase::Headers(index) => {
            let header_count = response.headers_len();
            let all = response.headers();
            let skipped = all.skip(*index);

            do_write_headers(skipped, index, header_count - 1, w);

            if *index == header_count {
                *phase = ResponsePhase::Body;
            }
            false
        }

        // We're past the header.
        _ => false,
    }
}
