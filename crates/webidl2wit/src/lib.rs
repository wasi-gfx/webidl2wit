//! Utilities for converting [WebIDL][webidl] to [WebAssembly Interface Types][wit]
//!
//! Converting between WebIDL and WIT is primarily useful when building components that will
//! interact with wider ecosystems that are build on WebIDL, the most prominent example
//! being W3C spec-compliant web browsers.
//!
//! [webidl]: <https://developer.mozilla.org/en-US/docs/Glossary/WebIDL>
//! [wit]: <https://github.com/WebAssembly/component-model/blob/main/design/mvp/WIT.md>
//!

mod borrow_params;
mod custom_resources;
mod translations;
mod types_;

pub use translations::{webidl_to_wit, ConversionOptions, HandleUnsupported, ResourceInheritance};
pub use wit_encoder::{Ident, PackageName};
