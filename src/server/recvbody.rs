use crate::Error;

use super::state::{ProvideResponse, RecvBody};
use super::Reply;

impl Reply<RecvBody> {
    /// Read the input as a request body
    ///
    /// Returns `(usize, usize)` where the first number is how many bytes of the input was used
    /// and the second number how many of the output.
    pub fn read(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        let rbm = self.inner.state.reader.as_mut().unwrap();

        if rbm.is_ended() {
            return Ok((0, 0));
        }

        rbm.read(input, output, self.inner.state.stop_on_chunk_boundary)
    }

    /// Set whether we are stopping on chunk boundaries.
    ///
    /// If `false`, we are trying to fill the entire `output` in each `read()` call.
    ///
    /// Defaults to `false`.
    pub fn stop_on_chunk_boundary(&mut self, enabled: bool) {
        self.inner.state.stop_on_chunk_boundary = enabled;
    }

    /// Tell if we are currently on a chunk boundary.
    ///
    /// Only relevant if we are first enabling `stop_on_chunk_boundary()`.
    pub fn is_on_chunk_boundary(&self) -> bool {
        let rbm = self.inner.state.reader.as_ref().unwrap();
        rbm.is_on_chunk_boundary()
    }

    /// Tell if the request body is over
    pub fn is_ended(&self) -> bool {
        let rbm = self.inner.state.reader.as_ref().unwrap();
        rbm.is_ended()
    }

    /// Proceed to sending a response.
    ///
    /// This is only possible when the request body is fully read.
    pub fn proceed<B>(self) -> Result<Reply<ProvideResponse>, Error> {
        assert!(self.is_ended());

        Ok(Reply::wrap(self.inner))
    }
}
