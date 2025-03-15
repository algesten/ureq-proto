use http::uri::Scheme;
use http::{header, HeaderName, HeaderValue, Method, StatusCode, Version};

use crate::Error;

pub(crate) trait MethodExt {
    fn is_http10(&self) -> bool;
    fn is_http11(&self) -> bool;
    fn need_request_body(&self) -> bool;
    fn verify_version(&self, version: Version) -> Result<(), Error>;
}

impl MethodExt for Method {
    fn is_http10(&self) -> bool {
        self == Method::GET || self == Method::HEAD || self == Method::POST
    }

    fn is_http11(&self) -> bool {
        self == Method::PUT
            || self == Method::DELETE
            || self == Method::CONNECT
            || self == Method::OPTIONS
            || self == Method::TRACE
            || self == Method::PATCH
    }

    fn need_request_body(&self) -> bool {
        self == Method::POST || self == Method::PUT || self == Method::PATCH
    }

    fn verify_version(&self, v: Version) -> Result<(), Error> {
        if v != Version::HTTP_10 && v != Version::HTTP_11 {
            return Err(Error::UnsupportedVersion);
        }

        let method_ok = self.is_http10() || v == Version::HTTP_11 && self.is_http11();

        if !method_ok {
            return Err(Error::MethodVersionMismatch(self.clone(), v));
        }

        Ok(())
    }
}

pub(crate) trait HeaderIterExt {
    fn has(self, key: HeaderName, value: &str) -> bool;
    fn has_expect_100(self) -> bool;
}

impl<'a, I: Iterator<Item = (&'a HeaderName, &'a HeaderValue)>> HeaderIterExt for I {
    fn has(self, key: HeaderName, value: &str) -> bool {
        self.filter(|i| i.0 == key).any(|i| i.1 == value)
    }

    fn has_expect_100(self) -> bool {
        self.has(header::EXPECT, "100-continue")
    }
}

pub(crate) trait StatusExt {
    /// Detect 307/308 redirect
    fn is_redirect_retaining_status(&self) -> bool;
}

impl StatusExt for StatusCode {
    fn is_redirect_retaining_status(&self) -> bool {
        *self == StatusCode::TEMPORARY_REDIRECT || *self == StatusCode::PERMANENT_REDIRECT
    }
}

pub trait SchemeExt {
    fn default_port(&self) -> Option<u16>;
}

impl SchemeExt for Scheme {
    fn default_port(&self) -> Option<u16> {
        if *self == Scheme::HTTPS {
            Some(443)
        } else if *self == Scheme::HTTP {
            Some(80)
        } else {
            debug!("Unknown scheme: {}", self);
            None
        }
    }
}
