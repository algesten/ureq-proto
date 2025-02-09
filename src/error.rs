use std::fmt;

use http::{Method, Version};

/// Error type for ureq-proto
#[derive(Debug, PartialEq, Eq)]
#[allow(missing_docs)]
#[non_exhaustive]
pub enum Error {
    BadHeader(String),
    UnsupportedVersion,
    MethodVersionMismatch(Method, Version),
    TooManyHostHeaders,
    TooManyContentLengthHeaders,
    BadHostHeader,
    BadAuthorizationHeader,
    BadContentLengthHeader,
    MethodForbidsBody(Method),
    MethodRequiresBody(Method),
    OutputOverflow,
    ChunkLenNotAscii,
    ChunkLenNotANumber,
    ChunkExpectedCrLf,
    BodyContentAfterFinish,
    BodyLargerThanContentLength,
    UnfinishedRequest,
    HttpParseFail(String),
    HttpParseTooManyHeaders,
    MissingResponseVersion,
    ResponseMissingStatus,
    ResponseInvalidStatus,
    IncompleteResponse,
    NoLocationHeader,
    BadLocationHeader(String),
    HeadersWith100,
    BodyIsChunked,
    RequestMissingMethod,
    RequestInvalidMethod,
}

impl From<httparse::Error> for Error {
    fn from(value: httparse::Error) -> Self {
        Error::HttpParseFail(value.to_string())
    }
}

impl std::error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::BadHeader(v) => write!(f, "bad header: {}", v),
            Error::UnsupportedVersion => write!(f, "unsupported http version"),
            Error::MethodVersionMismatch(m, v) => {
                write!(f, "{} not valid for HTTP version {:?}", m, v)
            }
            Error::TooManyHostHeaders => write!(f, "more than one host header"),
            Error::TooManyContentLengthHeaders => write!(f, "more than one content-length header"),
            Error::BadHostHeader => write!(f, "host header is not a string"),
            Error::BadAuthorizationHeader => write!(f, "authorization header is not a string"),
            Error::BadContentLengthHeader => write!(f, "content-length header not a number"),
            Error::MethodForbidsBody(v) => write!(f, "method forbids body: {}", v),
            Error::MethodRequiresBody(v) => write!(f, "method requires body: {}", v),
            Error::OutputOverflow => write!(f, "output too small to write output"),
            Error::ChunkLenNotAscii => write!(f, "chunk length is not ascii"),
            Error::ChunkLenNotANumber => write!(f, "chunk length cannot be read as a number"),
            Error::ChunkExpectedCrLf => write!(f, "chunk expected crlf as next character"),
            Error::BodyContentAfterFinish => {
                write!(f, "attempt to stream body after sending finish (&[])")
            }
            Error::BodyLargerThanContentLength => {
                write!(f, "attempt to write larger body than content-length")
            }
            Error::UnfinishedRequest => write!(f, "request is not finished"),
            Error::HttpParseFail(v) => write!(f, "http parse fail: {}", v),
            Error::HttpParseTooManyHeaders => write!(f, "http parse resulted in too many headers"),
            Error::MissingResponseVersion => write!(f, "http response missing version"),
            Error::ResponseMissingStatus => write!(f, "http response missing status"),
            Error::ResponseInvalidStatus => write!(f, "http response invalid status"),
            Error::IncompleteResponse => write!(f, "must read http response before body"),
            Error::NoLocationHeader => write!(f, "missing a location header"),
            Error::BadLocationHeader(v) => write!(f, "location header is malformed: {}", v),
            Error::HeadersWith100 => write!(f, "received headers with 100-continue response"),
            Error::BodyIsChunked => write!(f, "body is chunked"),
            Error::RequestMissingMethod => write!(f, "http request is missing a method"),
            Error::RequestInvalidMethod => write!(f, "http request invalid method"),
        }
    }
}
