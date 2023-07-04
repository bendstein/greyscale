use self::token_type::TokenType;
use crate::util::string::{GraphemeString};
use std::ops::Range;

pub mod token_type;

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Token {
    token_type: TokenType,
    range: Range<usize>,
    line: usize
}

impl Token {
    pub fn new(token_type: TokenType, range: Range<usize>, line: usize) -> Self {
        Self {
            token_type,
            range,
            line
        }
    }

    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    pub fn range(&self) -> &Range<usize> {
        &self.range
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn get_value(&self, string: &GraphemeString) -> String {
        string.substring(&self.range)
    }
}