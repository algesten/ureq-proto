use crate::{BodyMode, Error};

use super::Reply;
use super::state::{ProvideResponse, RecvBody};

impl Reply<RecvBody> {
    /// Read the input as a request body.
    ///
    /// This method reads data from the input buffer (the request body from the client)
    /// and writes it to the output buffer. It handles different transfer encodings
    /// (chunked, content-length, etc.) automatically.
    ///
    /// * `input` - A byte slice containing the input data from the client
    /// * `output` - A mutable byte slice to write the decoded body data to
    ///
    /// Returns a tuple `(usize, usize)` where:
    /// - The first element is the number of bytes consumed from the input
    /// - The second element is the number of bytes written to the output
    pub fn read(&mut self, input: &[u8], output: &mut [u8]) -> Result<(usize, usize), Error> {
        let rbm = self.inner.state.reader.as_mut().unwrap();

        if rbm.is_ended() {
            return Ok((0, 0));
        }

        rbm.read(input, output, self.inner.state.stop_on_chunk_boundary)
    }

    /// Helper to avoid copying memory.
    ///
    /// For length-delimited bodies, `read()` just copies `input` to `output`.
    /// This copy can be avoided if the caller consumes the transport's input
    /// buffer directly, or reads from the transport into its own buffer.
    ///
    /// Report the number of body bytes consumed this way, equivalent to the
    /// first `usize` returned by `read()`. Limit transport reads to [`Self::remaining`]
    /// so bytes belonging to the next request are not consumed.
    ///
    /// Returns [`Error::BodyIsChunked`] for chunked bodies, or
    /// [`Error::BodyLargerThanContentLength`] if `amount` exceeds the remaining
    /// length. On error, the remaining length is unchanged.
    pub fn consume_direct_read(&mut self, amount: usize) -> Result<(), Error> {
        let reader = self.inner.state.reader.as_mut().unwrap();
        let crate::body::BodyReader::LengthDelimited(left) = reader else {
            return Err(Error::BodyIsChunked);
        };
        if amount as u64 > *left {
            return Err(Error::BodyLargerThanContentLength);
        }
        *left -= amount as u64;
        Ok(())
    }

    /// Remaining bytes in a length-delimited request body.
    ///
    /// Returns `None` for chunked bodies and `Some(0)` after a length-delimited
    /// body has been fully consumed, including through [`Self::read`].
    pub fn remaining(&self) -> Option<u64> {
        match self.inner.state.reader.as_ref().unwrap() {
            crate::body::BodyReader::LengthDelimited(left) => Some(*left),
            _ => None,
        }
    }

    /// Set whether we are stopping on chunk boundaries.
    ///
    /// If `false`, we are trying to fill the entire `output` in each `read()` call.
    ///
    /// This is useful when processing chunked transfer encoding and you want to
    /// handle each chunk separately.
    ///
    /// * `enabled` - Whether to stop reading at chunk boundaries
    ///
    /// Defaults to `false` (read as much as possible).
    pub fn stop_on_chunk_boundary(&mut self, enabled: bool) {
        self.inner.state.stop_on_chunk_boundary = enabled;
    }

    /// Tell if we are currently on a chunk boundary.
    ///
    /// This method is useful when you've enabled `stop_on_chunk_boundary()` to
    /// determine if the current position is at a chunk boundary.
    ///
    /// Returns `true` if the current position is at a chunk boundary, `false` otherwise.
    ///
    /// Only relevant if you are using chunked transfer encoding and have enabled
    /// `stop_on_chunk_boundary()`.
    pub fn is_on_chunk_boundary(&self) -> bool {
        let rbm = self.inner.state.reader.as_ref().unwrap();
        rbm.is_on_chunk_boundary()
    }

    /// Tell which kind of mode the response body is.
    pub fn body_mode(&self) -> BodyMode {
        self.inner.state.reader.as_ref().unwrap().body_mode()
    }

    /// Tell if the request body is over
    ///
    /// Returns `true` if the entire request body has been received, `false` otherwise.
    pub fn is_ended(&self) -> bool {
        let rbm = self.inner.state.reader.as_ref().unwrap();
        rbm.is_ended()
    }

    /// Proceed to sending a response.
    ///
    /// This is only possible when the request body is fully read.
    ///
    /// Returns the Reply in the ProvideResponse state.
    ///
    /// Panics if the request body has not been fully read.
    pub fn proceed(self) -> Result<Reply<ProvideResponse>, Error> {
        assert!(self.is_ended());

        Ok(Reply::wrap(self.inner))
    }
}
