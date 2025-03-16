use http::{StatusCode, Version};

use crate::util::Writer;
use crate::Error;

use super::state::{ProvideResponse, RecvBody, Send100};
use super::{do_write_send_line, Reply};

impl Reply<Send100> {
    /// Sends a 100 Continue response.
    ///
    /// This proceeds to receiving the body.
    ///
    /// Panics if output isn't large enough to contain the 100 Continue status line.
    pub fn accept(self, output: &mut [u8]) -> Result<(usize, Reply<RecvBody>), Error> {
        let mut w = Writer::new(output);

        do_write_send_line((Version::HTTP_11, StatusCode::CONTINUE), &mut w, true);

        let output_used = w.len();

        let flow = Reply::wrap(self.inner);

        Ok((output_used, flow))
    }

    pub fn reject(mut self) -> Result<Reply<ProvideResponse>, Error> {
        self.inner.expect_100_reject = true;
        Ok(Reply::wrap(self.inner))
    }
}
