use crate::token::token_type::{Base, StringType};
use crate::token::{Token, token_type::TokenType};
use crate::util::string::GraphemeString;
use crate::vm::error::GreyscaleError;

pub mod symbols;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,
    program: &'a GraphemeString<'a>
}

impl<'a> Lexer<'a> {
    pub fn new(program: &'a GraphemeString<'a>) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 0,
            program
        }
    }
    
    // pub fn reset(&mut self) {
        //     self.start = 0;
        //     self.current = 0;
        //     self.line = 0;
        // }
        
        pub fn scan_token(&mut self) -> Option<Result<Token, GreyscaleError>> {
            self.start = self.current;

            if let Err(skip_err) = self.skip_comment_whitespace() {
                return Some(Err(skip_err));
            }

            if self.is_at_end() {
                return None;
            }
            
            let c = self.advance()?;
            
            match c {
                symbols::L_PAREN => Some(Ok(self.make_token(TokenType::LParen))),
                symbols::R_PAREN => Some(Ok(self.make_token(TokenType::RParen))),
                symbols::L_BRACE => Some(Ok(self.make_token(TokenType::LBrace))),
                symbols::R_BRACE => Some(Ok(self.make_token(TokenType::RBrace))),
                symbols::L_BRAC => Some(Ok(self.make_token(TokenType::LBrac))),
                symbols::R_BRAC => Some(Ok(self.make_token(TokenType::RBrac))),
                symbols::COMMA => Some(Ok(self.make_token(TokenType::Comma))),
                symbols::DOT => Some(Ok(self.make_token(TokenType::Dot))),
                symbols::COLON => Some(Ok(self.make_token(TokenType::Colon))),
                symbols::SEMI => Some(Ok(self.make_token(TokenType::Semi))),
                symbols::S_QUOTE => {
                    let _ = self.advance_n(-1);
                    Some(self.match_string())
                },
                symbols::D_QUOTE => {
                    let _ = self.advance_n(-1);
                    Some(self.match_string())
                },
                symbols::BACKTICK => {
                    let _ = self.advance_n(-1);
                    Some(self.match_string())
                },
                symbols::BANG => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::BangEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Bang)))
                },
                symbols::EQUAL => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::EqualEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Equal)))
                },
                symbols::PLUS => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::PlusEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Plus)))
                },
                symbols::MINUS => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::MinusEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Minus)))
                },
                symbols::STAR => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::StarEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Star)))
                },
                symbols::SLASH => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::SlashEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Slash)))
                },
                symbols::PERCENT => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::PercentEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Percent)))
                },
                symbols::TILDE => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::TildeEqual)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Tilde)))
                },
                symbols::AMP => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::AmpEqual)));
                    }
                    else if self.match_symbol(symbols::AMP) {
                        
                        if self.match_symbol(symbols::EQUAL) {
                            return Some(Ok(self.make_token(TokenType::AmpAmpEqual)));
                        }
                        
                        return Some(Ok(self.make_token(TokenType::AmpAmp)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Amp)))
                },
                symbols::PIPE => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::PipeEqual)));
                    }
                    else if self.match_symbol(symbols::PIPE) {
                        
                        if self.match_symbol(symbols::EQUAL) {
                            return Some(Ok(self.make_token(TokenType::PipePipeEqual)));
                        }
                        
                        return Some(Ok(self.make_token(TokenType::PipePipe)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Pipe)))
                },
                symbols::GREATER => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::GreaterEqual)));
                    }
                    else if self.match_symbol(symbols::GREATER) {
                        
                        if self.match_symbol(symbols::EQUAL) {
                            return Some(Ok(self.make_token(TokenType::GreaterGreaterEqual)));
                        }
                        
                        return Some(Ok(self.make_token(TokenType::GreaterGreater)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Greater)))
                },
                symbols::LESS => {
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::LessEqual)));
                    }
                    else if self.match_symbol(symbols::LESS) {  

                        if self.match_symbol(symbols::EQUAL) {
                            return Some(Ok(self.make_token(TokenType::LessLessEqual)));
                        }
                        
                        return Some(Ok(self.make_token(TokenType::LessLess)));
                    }
                    
                    Some(Ok(self.make_token(TokenType::Less)))
                },
                _ => {
                    //If number literal
                    if Base::Decimal.digit_is_allowed(c) {
                        let _ = self.advance_n(-1);
                        return Some(self.match_number());
                    }

                    Some(Err(GreyscaleError::CompileErr(format!("Unexpected character '{}'", &self.current().unwrap_or_default()))))
                }
            }       
        }
        
        fn current(&self) -> Option<&'a str> {
            self.peek_n(-1)
        }
        
        fn peek(&self) -> Option<&'a str> {
            self.peek_n(0)
        }
        
        fn peek_n(&self, n: isize) -> Option<&'a str> {
            if self.program.is_empty() {
                None
            }
            else {
                let sum = self.current.checked_add_signed(n)?;
                
                if sum >= self.program.len() {
                    None
                }
                else {
                    Some(self.program.char_at(sum))
                }
            }
        }
        
        fn advance(&mut self) -> Option<&'a str> {
            self.advance_n(1)
        }
        
        fn advance_n(&mut self, n: isize) -> Option<&'a str> {
            self.current = self.current.checked_add_signed(n).unwrap_or(self.start);
            self.current()
        }

        fn match_symbol(&mut self, symbol: &str) -> bool {
            if let Some(next) = self.peek() {
                if next == symbol {
                    self.advance();
                    true
                }
                else {
                    false
                }
            }
            else {
                false
            }
        }
        
        fn match_symbol_case_insensitive(&mut self, symbol: &str) -> bool {
            if let Some(next) = self.peek() {
                if next.eq_ignore_ascii_case(symbol) {
                    self.advance();
                    true
                }
                else {
                    false
                }
            }
            else {
                false
            }
        }

        fn make_token(&self, token_type: TokenType) -> Token {
            Token::new(token_type, self.start..self.current, self.line)
        }
        
        fn match_string(&mut self) -> Result<Token, GreyscaleError> {
            if let Some(open) = self.advance() {

                let string_type = match open {
                    symbols::D_QUOTE | symbols::S_QUOTE => Ok(StringType::Literal),
                    symbols::BACKTICK => Ok(StringType::Interpolated),
                    _ => Err(GreyscaleError::CompileErr("Invalid start of string.".to_string()))
                }?;

                let mut closed = false;

                //Collect all symbols until the closing quote, of the same type as the open, is reached.
                while let Some(next) = self.advance() {
                    if next == open {
                        closed = true;
                        break;
                    }
                    else if matches!(next, "\n" | "\r\n") {
                        self.line += 1;
                    }
                }

                if !closed {
                    Err(GreyscaleError::CompileErr("Unterminated string literal".to_string()))
                }
                else {
                    Ok(self.make_token(TokenType::String(self.start..self.current, string_type)))
                }
            }
            else {
                Err(GreyscaleError::CompileErr("Unexpected end of input".to_string()))
            }
        }

        fn match_number(&mut self) -> Result<Token, GreyscaleError> {
            //Match first digit
            if let Some(next) = self.advance() {
                let first_digit = next.parse::<u8>()
                    .map_err(|_| GreyscaleError::CompileErr("Expected a digit".to_string()))?;              

                let mut seen_digit = false;

                //If first digit is 0, it could be part of the base prefix
                let base: Base = if first_digit == 0 {
                    if self.match_symbol_case_insensitive(symbols::B) {
                        Base::Binary
                    }
                    else if self.match_symbol_case_insensitive(symbols::X) {
                        Base::Hexadecimal
                    }
                    else {
                        seen_digit = true;
                        Base::Decimal
                    }
                }
                else {
                    seen_digit = true;
                    Base::Decimal
                };

                let mut seen_dot = false;

                while let Some(next) = self.peek() {
                    //Any number of underscores allowed as visual separators.
                    if next == symbols::UNDERSCORE {
                        let _ = self.advance();
                        continue;
                    }

                    //Match decimal point
                    if next == symbols::DOT {
                        //If a digit hasn't been seen yet, this is invalid
                        if !seen_digit {
                            let _ = self.advance();
                            return Err(GreyscaleError::CompileErr("Expected a digit".to_string()));
                        }

                        //If next symbol is a digit, this is a decimal point
                        if let Some(next) = self.peek_n(1) {
                            if base.digit_is_allowed(next) {

                                //Cannot have multiple decimal points
                                if seen_dot {
                                    let _ = self.advance();
                                    return Err(GreyscaleError::CompileErr("Number cannot have multiple radix points.".to_string()));
                                }

                                seen_dot = true;
                                seen_digit = true;
                                let _ = self.advance_n(2);
                                continue;
                            }
                        }

                        //If there isn't a digit after the dot, not part of number. Stop.
                        break;
                    }

                    //Match a valid digit
                    if base.digit_is_allowed(next) {
                        seen_digit = true;
                        let _ = self.advance();
                        continue;
                    }

                    //This is not part of a number. Stop.
                    break;
                }

                if !seen_digit {
                    Err(GreyscaleError::CompileErr("Expected a digit".to_string()))
                }
                else {
                    Ok(self.make_token(TokenType::Number(self.start..self.current, base)))
                }
            }
            else {
                Err(GreyscaleError::CompileErr("Unexpected end of input".to_string()))
            }
        }

        fn skip_comment_whitespace(&mut self) -> Result<(), GreyscaleError> {
            if self.is_at_end() {
                return Ok(());
            }

            while let Some(next) = self.peek() {
                //On whitespace character, advance
                if next.is_empty() || matches!(next, " " | "\r" | "\t") {
                    self.advance();
                    continue;
                }
                //On new line, increment line and advance
                else if matches!(next, "\n" | "\r\n") {
                    self.line += 1;
                    self.advance();
                    continue;
                }
                //If symbol is a slash, try to match a comment
                else if next == symbols::SLASH {
                    //Determine whether this is a comment start by checking the symbol after the slash
                    if let Some(next) = self.peek_n(1) {
                        let is_block;

                        //Line comment; everything until a new line is reached is part of the comment
                        if next == symbols::SLASH {
                            let _ = self.advance_n(2);
                            is_block = false;
                        }
                        //Block comment; everything until the terminating */ is reached is part of the comment
                        else if next == symbols::STAR {
                            let _ = self.advance_n(2);
                            is_block = true;
                        }
                        else {
                            break;
                        }

                        let mut terminated = false;

                        //For each start of a block comment, increment. For each end, decrement.
                        let mut block_count = 1;

                        while let Some(next) = self.advance() {
                            if matches!(next, "\n" | "\r\n") {
                                self.line += 1;
                                
                                //If this is not a block comment, then the comment is finished
                                if !is_block {
                                    terminated = true;
                                    break;
                                }
                            }
                            else if next == symbols::SLASH && self.match_symbol(symbols::STAR) {
                                //Increment block counter on /*
                                block_count += 1;
                            }
                            else if next == symbols::STAR && self.match_symbol(symbols::SLASH) {
                                //Decrement block counter on */
                                block_count -= 1;

                                //If this is a block comment, and the comment starts and ends have all
                                //been paired, done
                                if is_block && block_count == 0 {
                                    terminated = true;
                                    break;
                                }
                            }
                        }

                        if !terminated {
                            return Err(GreyscaleError::CompileErr("Unterminated comment".to_string()));
                        }

                        continue;
                    }
                }

                break;
            }

            Ok(())
        }
        
        fn is_at_end(&self) -> bool {
            self.current >= self.program.len()
        }
    }
    
    impl<'a> Iterator for Lexer<'a> {
        type Item = Result<Token, GreyscaleError>;
        
        fn next(&mut self) -> Option<Self::Item> {
            self.scan_token()
        }
    }