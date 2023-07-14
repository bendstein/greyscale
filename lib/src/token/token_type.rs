use std::ops::Range;

use crate::{lexer::symbols};

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum TokenType {
    //Single character tokens
    LParen, RParen,
    LBrace, RBrace,
    LBrac, RBrac,

    Comma, Dot, Colon, Semi,

    SQuote, DQuote, Backtick,

    //One or more character tokens
    Bang, BangEqual,
    Equal, EqualEqual,

    Plus, PlusEqual,
    Minus, MinusEqual,
    Star, StarEqual,
    Slash, SlashEqual,
    Percent, PercentEqual,

    Amp, AmpEqual,
    AmpAmp, AmpAmpEqual,

    Pipe, PipeEqual,
    PipePipe, PipePipeEqual,

    Caret, CaretEqual,
    CaretCaret, CaretCaretEqual,

    Tilde,

    Greater, GreaterEqual,
    GreaterGreater, GreaterGreaterEqual,

    Less, LessEqual,
    LessLess, LessLessEqual,

    //Literals

    Identifier(Range<usize>),

    String(Range<usize>, StringType),

    Number(Range<usize>, Option<usize>, Base),

    //Keywords
    If, Else,

    True, False, Null,

    Loop, While, For,

    Class, Super, This,

    Let, Func,

    Print,

    Continue, Break,

    #[default]
    Return
}

impl TokenType {
    pub fn as_string(&self) -> String {
        match self {
            TokenType::LParen => symbols::L_PAREN.to_string(),
            TokenType::RParen => symbols::R_PAREN.to_string(),
            TokenType::LBrace => symbols::L_BRACE.to_string(),
            TokenType::RBrace => symbols::R_BRACE.to_string(),
            TokenType::LBrac => symbols::L_BRAC.to_string(),
            TokenType::RBrac => symbols::R_BRAC.to_string(),
            TokenType::Comma => symbols::COMMA.to_string(),
            TokenType::Dot => symbols::DOT.to_string(),
            TokenType::Colon => symbols::COLON.to_string(),
            TokenType::Semi => symbols::SEMI.to_string(),
            TokenType::SQuote => symbols::S_QUOTE.to_string(),
            TokenType::DQuote => symbols::D_QUOTE.to_string(),
            TokenType::Backtick => symbols::BACKTICK.to_string(),
            TokenType::Bang => symbols::BANG.to_string(),
            TokenType::BangEqual => format!("{}{}", symbols::BANG, symbols::EQUAL),
            TokenType::Equal => symbols::EQUAL.to_string(),
            TokenType::EqualEqual => format!("{}{}", symbols::EQUAL, symbols::EQUAL),
            TokenType::Plus => symbols::PLUS.to_string(),
            TokenType::PlusEqual => format!("{}{}", symbols::PLUS, symbols::EQUAL),
            TokenType::Minus => symbols::MINUS.to_string(),
            TokenType::MinusEqual => format!("{}{}", symbols::MINUS, symbols::EQUAL),
            TokenType::Star => symbols::STAR.to_string(),
            TokenType::StarEqual => format!("{}{}", symbols::STAR, symbols::EQUAL),
            TokenType::Slash => symbols::SLASH.to_string(),
            TokenType::SlashEqual => format!("{}{}", symbols::SLASH, symbols::EQUAL),
            TokenType::Percent => symbols::PERCENT.to_string(),
            TokenType::PercentEqual => format!("{}{}", symbols::PERCENT, symbols::EQUAL),
            TokenType::Amp => symbols::AMP.to_string(),
            TokenType::AmpEqual => format!("{}{}", symbols::AMP, symbols::EQUAL),
            TokenType::AmpAmp => format!("{}{}", symbols::AMP, symbols::AMP),
            TokenType::AmpAmpEqual => format!("{}{}{}", symbols::AMP, symbols::AMP, symbols::EQUAL),
            TokenType::Pipe => symbols::PIPE.to_string(),
            TokenType::PipeEqual => format!("{}{}", symbols::PIPE, symbols::EQUAL),
            TokenType::PipePipe => format!("{}{}", symbols::PIPE, symbols::PIPE),
            TokenType::PipePipeEqual => format!("{}{}{}", symbols::PIPE, symbols::PIPE, symbols::EQUAL),
            TokenType::Caret => symbols::CARET.to_string(),
            TokenType::CaretEqual => format!("{}{}", symbols::CARET, symbols::EQUAL),
            TokenType::CaretCaret => format!("{}{}", symbols::CARET, symbols::CARET),
            TokenType::CaretCaretEqual => format!("{}{}{}", symbols::CARET, symbols::CARET, symbols::EQUAL),
            TokenType::Tilde => symbols::TILDE.to_string(),
            TokenType::Greater => symbols::GREATER.to_string(),
            TokenType::GreaterEqual => format!("{}{}", symbols::GREATER, symbols::EQUAL),
            TokenType::GreaterGreater => format!("{}{}", symbols::GREATER, symbols::GREATER),
            TokenType::GreaterGreaterEqual => format!("{}{}{}", symbols::GREATER, symbols::GREATER, symbols::EQUAL),
            TokenType::Less => symbols::LESS.to_string(),
            TokenType::LessEqual => format!("{}{}", symbols::LESS, symbols::EQUAL),
            TokenType::LessLess => format!("{}{}", symbols::LESS, symbols::LESS),
            TokenType::LessLessEqual => format!("{}{}{}", symbols::LESS, symbols::LESS, symbols::EQUAL),
            TokenType::Identifier(_) => "ID".to_string(),
            TokenType::String(_, t) => format!("{}STR", match t {
                StringType::Literal => "",
                StringType::Interpolated => "I",
                StringType::InterpolatedSegment => "ISEG",
                StringType::Escaped => "ESC",
            }),
            TokenType::Number(_, _, b) => format!("{}NUM", match b {
                Base::Binary => "b",
                Base::Hexadecimal => "x",
                Base::Decimal => ""
            }),
            TokenType::If => symbols::IF.to_string(),
            TokenType::Else => symbols::ELSE.to_string(),
            TokenType::True => symbols::TRUE.to_string(),
            TokenType::False => symbols::FALSE.to_string(),
            TokenType::Null => symbols::NULL.to_string(),
            TokenType::Loop => symbols::LOOP.to_string(),
            TokenType::While => symbols::WHILE.to_string(),
            TokenType::For => symbols::FOR.to_string(),
            TokenType::Class => symbols::CLASS.to_string(),
            TokenType::Super => symbols::SUPER.to_string(),
            TokenType::This => symbols::THIS.to_string(),
            TokenType::Let => symbols::LET.to_string(),
            TokenType::Func => symbols::FUNC.to_string(),
            TokenType::Print => symbols::PRINT.to_string(),
            TokenType::Continue => symbols::CONTINUE.to_string(),
            TokenType::Break => symbols::BREAK.to_string(),
            TokenType::Return => symbols::RETURN.to_string(),
        }
    }

    pub fn as_program_string(&self, program: &[&str]) -> String {
        match self {
            Self::Identifier(range) => format!("{}[{}]", self.as_string(), program[range.clone()].join("")),
            Self::String(range, _) => format!("{}[{}]", self.as_string(), program[range.clone()].join("")),
            Self::Number(range, _, _) => format!("{}[{}]", self.as_string(), program[range.clone()].join("")),
            _ => self.as_string()
        }
    }
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Base {
    #[default]
    Decimal,
    Binary,
    Hexadecimal
}

impl Base {
    pub fn is_decimal(&self) -> bool {
        matches!(self, Self::Decimal)
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, Self::Binary)
    }

    pub fn is_hexadecimal(&self) -> bool {
        matches!(self, Self::Hexadecimal)
    }
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum StringType {
    #[default]
    Literal,
    Interpolated,
    InterpolatedSegment,
    Escaped
}

impl StringType {
    pub fn is_literal(&self) -> bool {
        matches!(self, Self::Literal)
    }

    pub fn is_interpolated(&self) -> bool {
        matches!(self, Self::Interpolated)
    }

    pub fn is_interpolated_segment(&self) -> bool {
        matches!(self, Self::InterpolatedSegment)
    }

    pub fn is_escaped(&self) -> bool {
        matches!(self, Self::Escaped)
    }
}