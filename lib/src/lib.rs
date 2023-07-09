#[macro_use]
extern crate lazy_static;
extern crate unicode_segmentation;

pub mod ops;
pub mod chunk;
pub mod value;
pub mod metadata;
pub mod vm;
pub mod compiler;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod location;

pub mod constants {
    pub const TRACE: bool = false;
    pub const MAX_STACK: usize = isize::MAX as usize;
}