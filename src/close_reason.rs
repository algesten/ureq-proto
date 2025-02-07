/// Reasons for closing an HTTP connection.
///
/// This enum represents the various reasons why an HTTP connection might need
/// to be closed after a request/response cycle is complete.
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
    pub(crate) fn explain(&self) -> &'static str {
        match self {
            CloseReason::Http10 => "version is http1.0",
            CloseReason::ClientConnectionClose => "client sent Connection: close",
            CloseReason::ServerConnectionClose => "server sent Connection: close",
            CloseReason::Not100Continue => "got non-100 response before sending body",
            CloseReason::CloseDelimitedBody => "response body is close delimited",
        }
    }
}
