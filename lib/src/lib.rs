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
    pub const MAX_STACK: usize = isize::MAX as usize;

    pub const TRACE_NONE: u8                = 0b00000000_u8;
    pub const TRACE_LEXER: u8               = 0b00000001_u8;
    pub const TRACE_PARSER: u8              = 0b00000010_u8;
    pub const TRACE_VM: u8                  = 0b00000100_u8;
    pub const TRACE_OUTPUT_PARSE_TREE: u8   = 0b00001000_u8;
    pub const TRACE_OUTPUT_COMPILED: u8     = 0b00010000_u8;
    pub const TRACE_OUTPUT_INPUT: u8        = 0b00100000_u8;
    pub const TRACE_BENCHMARK: u8           = 0b01000000_u8;

    pub const TRACE: u8                     = TRACE_NONE
                                              //| TRACE_LEXER
                                              //| TRACE_PARSER
                                              //| TRACE_VM
                                              | TRACE_OUTPUT_PARSE_TREE
                                              | TRACE_OUTPUT_COMPILED
                                              //| TRACE_OUTPUT_INPUT
                                              //| TRACE_BENCHMARK
                                              ;
}