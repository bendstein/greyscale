use crate::token::token_type::StringType;
use crate::token::{Token, token_type::TokenType};
use crate::util::string::GraphemeString;
use crate::vm::error::GreyscaleError;

use super::symbols;

#[derive(Debug, Clone)]
pub struct InterpStringLexer<'a> {
    start: usize,
    current: usize,
    line: usize,
    end: usize,
    started: bool,
    terminated: bool,
    program: &'a GraphemeString<'a>
}

impl<'a> InterpStringLexer<'a> {
    pub fn new(program: &'a GraphemeString<'a>) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 0,
            end: program.len(),
            started: false,
            terminated: false,
            program
        }
    }

    pub fn move_range(mut self, start: usize, end: usize, line: usize) -> Self {
        self.start = start;
        self.current = start;
        self.end = end;
        self.line = line;
        self.started = false;
        self.terminated = false;
        self
    }

    pub fn reset_range(mut self) -> Self {
        self.start = 0;
        self.current = 0;
        self.end = self.program.len();
        self.line = 0;
        self.started = false;
        self.terminated = false;
        self
    }

    pub fn scan_token(&mut self) -> Option<Result<Token, GreyscaleError>> {
        self.start = self.current;

        if self.terminated {
            return None;
        }
        else if self.is_at_end() {
            //Mark string as terminated to prevent infinite errors
            self.terminated = true;
            return Some(Err(GreyscaleError::CompileErr("Unterminated interpolated string literal.".to_string())));
        }

        //Match first backtick
        if !self.started {
            if self.match_symbol(symbols::BACKTICK) {
                self.started = true;
            }
            else {
                //Mark string as terminated to prevent infinite errors
                self.terminated = true;
                return Some(Err(GreyscaleError::CompileErr("Expected a backtick to start the interpolated string literal.".to_string())));
            }
        }

        let c = self.advance();

        if c.is_none() {
            if self.terminated {
                return None;
            }
            else if self.is_at_end() {
                //Mark string as terminated to prevent infinite errors
                self.terminated = true;
                return Some(Err(GreyscaleError::CompileErr("Unterminated interpolated string literal.".to_string())));
            }
        }

        let c = c.unwrap();

        match c {
            symbols::L_BRACE => {
                //Do not start an interpolated segment on an escaped left brace
                if self.match_symbol(symbols::L_BRACE) {
                    let start = if self.program.char_at(self.start) == symbols::BACKTICK {
                        self.start + 1
                    }
                    else {
                        self.start
                    };

                    Some(Ok(self.make_token(TokenType::String(start..self.current, StringType::Escaped))))
                }
                //Otherwise, start the interpolated segment
                else {
                    let _ = self.advance_n(-1);
                    Some(self.match_interp_segment())
                }
            },
            symbols::R_BRACE => {
                let start = if self.program.char_at(self.start) == symbols::BACKTICK {
                    self.start + 1
                }
                else {
                    self.start
                };

                //To parallel l_brace, accept either single or escaped r_brace in a string
                if self.match_symbol(symbols::R_BRACE) {
                    Some(Ok(self.make_token(TokenType::String(start..self.current, StringType::Escaped))))
                }
                else {
                    Some(Ok(self.make_token(TokenType::String(start..self.current, StringType::Literal))))
                }
            },
            //Terminate interpolated string
            symbols::BACKTICK => {
                self.terminated = true;
                None
            },
            _ => {
                let _ = self.advance_n(-1);
                //String literal segment
                Some(self.match_string())
            }
        }
    }

    fn match_string(&mut self) -> Result<Token, GreyscaleError> {
        let start = self.current;

        while let Some(next) = self.peek() {
            //Exiting string literal portion
            if matches!(next, symbols::L_BRACE | symbols::R_BRACE | symbols::BACKTICK) {
                break;
            }
            //Do not terminate literal portion on an escaped backtick
            else if next == symbols::B_SLASH {
                let _ = self.advance();
                let _ = self.match_symbol(symbols::BACKTICK);
            }
            //Otherwise, accept symbol as part of string literal
            else {
                let _ = self.advance();
            }
        }

        Ok(self.make_token(TokenType::String(start..self.current, StringType::Literal)))
    }

    fn match_interp_segment(&mut self) -> Result<Token, GreyscaleError> {
        let start = self.current;

        //Ensure braces and backticks are balanced
        let mut control_stack: Vec<&str> = Vec::new();

        if let Some(open) = self.advance() {
            if open != symbols::L_BRACE {
                return Err(GreyscaleError::CompileErr("Invalid start of interpolated {} segment.".to_string()));
            }

            control_stack.push(open);

            loop {
                //Ignore whitespace and comments
                self.skip_comment_whitespace()?;

                if let Some(next) = self.advance() {
                    match next {
                        symbols::L_BRACE => {
                            if let Some(top) = control_stack.last() {
                                if top == &symbols::BACKTICK {
                                    //Do not increment brace count on an escaped left brace
                                    if self.match_symbol(symbols::L_BRACE) {

                                    }
                                    else {
                                        control_stack.push(next);
                                    }
                                }
                            }
                        },
                        symbols::R_BRACE => {
                            if let Some(top) = control_stack.last() {
                                //Do not increment brace count on an escaped left brace
                                if top == &symbols::BACKTICK && self.match_symbol(symbols::R_BRACE) {

                                }
                                else if top == &symbols::L_BRACE {
                                    let _ = control_stack.pop();

                                    if control_stack.is_empty() {
                                        break;
                                    }
                                }
                            }
                        },
                        //Handle inner backticks
                        symbols::BACKTICK => {
                            if let Some(top) = control_stack.last() {
                                //If top is a backtick, pop it
                                if top == &symbols::BACKTICK {
                                    let _ = control_stack.pop();

                                    if control_stack.is_empty() {
                                        break;
                                    }
                                }
                                //Otherwise, push it
                                else {
                                    control_stack.push(symbols::BACKTICK);
                                }
                            }
                        },
                        //Otherwise, consume character as part of string
                        _ => { }
                    }
                }
                else {
                    //Done
                    break;
                }
            }

            if !control_stack.is_empty() {
                //If reached this point, whole string was consumed. Mark as terminated.
                self.terminated = true;
                Err(GreyscaleError::CompileErr("Mismatched curly braces.".to_string()))
            }
            else {
                //Remove outer braces
                Ok(self.make_token(TokenType::String(start + 1..self.current - 1, StringType::InterpolatedSegment)))
            }
        }
        else {
            Err(GreyscaleError::CompileErr("Invalid interpolated segment.".to_string()))
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
    
    fn make_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.start..self.current, self.line)
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
                    
                    if is_block && !terminated {
                        return Err(GreyscaleError::CompileErr("Unterminated block comment".to_string()));
                    }
                    
                    continue;
                }
            }
            
            break;
        }
        
        Ok(())
    }

    pub fn is_at_end(&self) -> bool {
        self.terminated ||
        self.current >= self.end || 
            self.current >= self.program.len()
    }
}

impl<'a> Iterator for InterpStringLexer<'a> {
    type Item = Result<Token, GreyscaleError>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}