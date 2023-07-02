#[macro_use]
extern crate lazy_static;

pub const TRACE: bool = true;

pub mod ops;
pub mod chunk;
pub mod value;
pub mod metadata;
pub mod vm;