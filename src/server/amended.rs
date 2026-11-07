use std::fmt;

use http::{HeaderName, HeaderValue, Response, StatusCode, Version, header};

use crate::Error;
use crate::body::{BodyWriter, parse_content_length_value};
use crate::ext::HeaderIterExt;

pub(crate) struct AmendedResponse {
    response: Response<()>,
    headers: Vec<(HeaderName, HeaderValue)>,
}

impl AmendedResponse {
    pub fn new(response: Response<()>) -> Self {
        AmendedResponse {
            response,
            headers: vec![],
        }
    }

    pub fn prelude(&self) -> (Version, StatusCode) {
        let r = &self.response;
        (r.version(), r.status())
    }

    pub fn set_header<K, V>(&mut self, name: K, value: V) -> Result<(), Error>
    where
        HeaderName: TryFrom<K>,
        <HeaderName as TryFrom<K>>::Error: Into<http::Error>,
        HeaderValue: TryFrom<V>,
        <HeaderValue as TryFrom<V>>::Error: Into<http::Error>,
    {
        let name = <HeaderName as TryFrom<K>>::try_from(name)
            .map_err(Into::into)
            .map_err(|e| Error::BadHeader(e.to_string()))?;
        let value = <HeaderValue as TryFrom<V>>::try_from(value)
            .map_err(Into::into)
            .map_err(|e| Error::BadHeader(e.to_string()))?;
        self.headers.push((name, value));
        Ok(())
    }

    /// Remove HTTP/1.1 framing headers after analyzing the original response.
    pub fn remove_transfer_encoding(&mut self) -> bool {
        let has_transfer_encoding = self.headers().any(|(k, _)| *k == header::TRANSFER_ENCODING);
        if has_transfer_encoding {
            self.headers.retain(|(name, _)| {
                *name != header::TRANSFER_ENCODING && *name != header::CONTENT_LENGTH
            });
            self.response
                .headers_mut()
                .remove(header::TRANSFER_ENCODING);
            self.response.headers_mut().remove(header::CONTENT_LENGTH);
        }
        has_transfer_encoding
    }

    pub fn remove_keep_alive(&mut self) {
        self.headers.retain_mut(|(name, value)| {
            *name != header::CONNECTION || remove_keep_alive_token(value)
        });
        let headers = self.response.headers_mut();
        let values: Vec<_> = headers
            .get_all(header::CONNECTION)
            .iter()
            .cloned()
            .collect();
        headers.remove(header::CONNECTION);
        for mut value in values {
            if remove_keep_alive_token(&mut value) {
                headers.append(header::CONNECTION, value);
            }
        }
    }

    pub fn headers(&self) -> impl Iterator<Item = (&HeaderName, &HeaderValue)> {
        self.headers
            .iter()
            .map(|v| (&v.0, &v.1))
            .chain(self.response.headers().iter())
    }

    fn headers_get_all(&self, key: HeaderName) -> impl Iterator<Item = &HeaderValue> {
        self.headers()
            .filter(move |(k, _)| *k == key)
            .map(|(_, v)| v)
    }

    fn headers_get(&self, key: HeaderName) -> Option<&HeaderValue> {
        self.headers_get_all(key).next()
    }

    pub fn headers_len(&self) -> usize {
        self.headers().count()
    }

    pub fn analyze(&self, wanted_mode: BodyWriter) -> Result<ResponseInfo, Error> {
        let count_len = self.headers_get_all(header::CONTENT_LENGTH).count();
        if count_len > 1 {
            return Err(Error::TooManyContentLengthHeaders);
        }

        // We control what we send: a single header line holding a single
        // number. A comma separated list must not reach the wire.
        let content_length = self
            .headers_get(header::CONTENT_LENGTH)
            .map(|h| parse_content_length_value(h.as_bytes()))
            .transpose()?;

        let has_chunked = self.headers().has(header::TRANSFER_ENCODING, "chunked");

        // https://www.rfc-editor.org/rfc/rfc9112#section-6.1
        // A sender MUST NOT send a Content-Length header field in any message
        // that contains a Transfer-Encoding header field.
        if has_chunked && content_length.is_some() {
            return Err(Error::ConflictingBodyHeaders);
        }

        let mut res_body_header = false;

        let body_mode = if has_chunked {
            res_body_header = true;
            BodyWriter::new_chunked()
        } else if let Some(n) = content_length {
            res_body_header = true;
            BodyWriter::new_sized(n)
        } else {
            wanted_mode
        };

        Ok(ResponseInfo {
            body_mode,
            res_body_header,
        })
    }
}

fn remove_keep_alive_token(value: &mut HeaderValue) -> bool {
    let Ok(text) = value.to_str() else {
        return true;
    };
    let tokens: Vec<_> = text
        .split(',')
        .map(str::trim)
        .filter(|token| !token.eq_ignore_ascii_case("keep-alive") && !token.is_empty())
        .collect();
    if tokens.is_empty() {
        return false;
    }
    *value = HeaderValue::from_str(&tokens.join(", ")).expect("valid header tokens");
    true
}

pub(crate) struct ResponseInfo {
    pub body_mode: BodyWriter,
    pub res_body_header: bool,
}

impl fmt::Debug for AmendedResponse {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AmendedResponse")
            .field("status", &self.response.status())
            .field("headers", &self.headers)
            .finish()
    }
}

#[cfg(test)]
mod transfer_encoding_tests {
    use super::*;

    #[test]
    fn response_writer_detects_chunked_in_lists_and_repeated_headers() {
        for values in [
            vec!["gzip, chunked"],
            vec!["chunked, gzip"],
            vec!["gzip", "ChUnKeD"],
        ] {
            let mut response = Response::builder().status(200);
            for value in values {
                response = response.header(header::TRANSFER_ENCODING, value);
            }
            let response = AmendedResponse::new(response.body(()).unwrap());
            assert!(
                response
                    .analyze(BodyWriter::new_none())
                    .unwrap()
                    .body_mode
                    .is_chunked()
            );
        }
    }

    #[test]
    fn response_rejects_content_length_with_listed_chunked() {
        for value in ["gzip, chunked", "chunked, gzip"] {
            let response = Response::builder()
                .header(header::TRANSFER_ENCODING, value)
                .header(header::CONTENT_LENGTH, "5")
                .body(())
                .unwrap();
            assert!(matches!(
                AmendedResponse::new(response).analyze(BodyWriter::new_none()),
                Err(Error::ConflictingBodyHeaders)
            ));
        }
    }
}
