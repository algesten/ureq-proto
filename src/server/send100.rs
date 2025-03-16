use http::{Response, StatusCode, Version};

use crate::body::BodyWriter;
use crate::util::Writer;
use crate::Error;

use super::state::{RecvBody, Send100, SendResponse};
use super::{append_request, do_write_send_line, Reply};

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

        let reply = Reply::wrap(self.inner);

        Ok((output_used, reply))
    }

    pub fn reject<B>(self, response: Response<B>) -> Result<Reply<SendResponse, B>, Error> {
        if !response.status().is_client_error() && !response.status().is_server_error() {
            return Err(Error::BadReject100Status(response.status()));
        }

        let inner = append_request(self.inner, response, BodyWriter::new_chunked());

        Ok(Reply::wrap(inner))
    }
}
