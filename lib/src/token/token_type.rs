use std::ops::Range;

use crate::{lexer::symbols, util::string::GraphemeString};

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

    Tilde, TildeEqual,

    Greater, GreaterEqual,
    GreaterGreater, GreaterGreaterEqual,

    Less, LessEqual,
    LessLess, LessLessEqual,

    //Literals

    Identifier(Range<usize>),

    String(Range<usize>, StringType),

    Number(Range<usize>, Base),

    //Keywords
    If, Else,

    True, False, Null,

    Loop, While, For,

    Class, Super, This,

    Let, Func,

    Print,

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
            TokenType::Tilde => symbols::TILDE.to_string(),
            TokenType::TildeEqual => format!("{}{}", symbols::TILDE, symbols::EQUAL),
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
                StringType::Interpolated => "I"
            }),
            TokenType::Number(_, b) => format!("{}NUM", match b {
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
            TokenType::Return => symbols::RETURN.to_string(),
        }
    }

    pub fn as_program_string(&self, program: &GraphemeString) -> String {
        match self {
            TokenType::Identifier(id) => program.substring(id),
            TokenType::String(s, _) => program.substring(s),
            TokenType::Number(n, _) => program.substring(n),
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
    pub fn base_num(&self) -> u8 {
        match self {
            Base::Binary => 2,
            Base::Decimal => 10,
            Base::Hexadecimal => 16
        }
    }

    pub fn allowed_digits(&self) -> &'static [&'static str] {
        let digits = &*DIGITS;
        &digits[0..self.base_num() as usize]
    }

    pub fn digit_is_allowed(&self, digit: &str) -> bool {
        self.allowed_digits().contains(&digit.to_uppercase().as_str())
    }
}

lazy_static! {
    pub static ref DIGITS: Vec<&'static str> = vec![
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"
    ];
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum StringType {
    #[default]
    Literal,
    Interpolated
}