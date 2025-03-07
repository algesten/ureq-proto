//! Supporting crate for [ureq](https://crates.io/crates/ureq).
//!
//! This crate contains types used to implement ureq.
//!
//!

#![forbid(unsafe_code)]
#![warn(clippy::all)]
#![allow(clippy::uninlined_format_args)]
#![deny(missing_docs)]
// I don't think elided lifetimes help in understanding the code.
#![allow(clippy::needless_lifetimes)]

#[macro_use]
extern crate log;

// Re-export the basis for this library.
pub use http;

mod error;
pub use error::Error;

mod chunk;
mod ext;
mod util;

mod body;
pub use body::BodyMode;

pub mod client;
pub mod server;

mod close_reason;
pub use close_reason::CloseReason;

/// Low level HTTP parser
///
/// This is to bridge `httparse` crate to `http` crate.
pub mod parser;

#[doc(hidden)]
pub use util::ArrayVec;
