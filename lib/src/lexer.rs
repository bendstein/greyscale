use std::ops::Range;
use std::rc::Rc;

use crate::token::token_type::{Base, StringType};
use crate::token::{Token, token_type::TokenType};
use crate::vm::error::GreyscaleError;

pub mod symbols;
pub mod interpolated_string_lexer;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    start: usize,
    current: usize,
    line: usize,
    end: usize,
    program: Rc<Vec<&'a str>>
}

#[derive(Debug, Default, Clone, Copy, Eq, PartialEq, PartialOrd, Ord)]
pub struct LexerState {
    pub start: usize,
    pub current: usize,
    pub line: usize,
    pub end: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(program: Rc<Vec<&'a str>>) -> Self {
        Self {
            start: 0,
            current: 0,
            line: 0,
            end: program.len(),
            program
        }
    }

    pub fn with_state(mut self, state: LexerState) -> Self {
        self.start = state.start;
        self.current = state.start;
        self.end = state.end;
        self.line = state.line;
        self
    }

    pub fn with_reset_state(mut self) -> Self {
        self.start = 0;
        self.current = 0;
        self.end = self.program.len();
        self.line = 0;
        self
    }

    pub fn set_state(&mut self, state: LexerState) {
        self.start = state.start;
        self.current = state.start;
        self.end = state.end;
        self.line = state.line;
    }
        
    pub fn get_state(&self) -> LexerState {
        LexerState {
            start: self.start,
            current: self.current,
            end: self.end,
            line: self.line
        }
    }

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
            symbols::CARET => {
                if self.match_symbol(symbols::EQUAL) {
                    return Some(Ok(self.make_token(TokenType::CaretEqual)));
                }
                else if self.match_symbol(symbols::CARET) {
                    
                    if self.match_symbol(symbols::EQUAL) {
                        return Some(Ok(self.make_token(TokenType::CaretCaretEqual)));
                    }
                    
                    return Some(Ok(self.make_token(TokenType::CaretCaret)));
                }
                
                Some(Ok(self.make_token(TokenType::Caret)))
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
                //If numeric, match number literal
                if Self::is_digit(c, Base::Decimal) {
                    let _ = self.advance_n(-1);
                    return Some(self.match_number());
                }
                //If alpha, match identifier
                else if Self::is_alpha(c) {
                    let _ = self.advance_n(-1);
                    return Some(self.match_identifier());
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
            
            if sum >= self.end || sum >= self.program.len() {
                None
            }
            else {
                Some(self.program[sum])
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
        let start = self.current;
        
        //For an interpolated string, balance braces and backticks to ensure we match the whole string
        let mut control_stack: Vec<&str> = Vec::new();

        if let Some(open) = self.advance() {
            
            let string_type = match open {
                symbols::D_QUOTE | symbols::S_QUOTE => Ok(StringType::Literal),
                symbols::BACKTICK => {
                    control_stack.push(symbols::BACKTICK);
                    Ok(StringType::Interpolated)
                },
                _ => Err(GreyscaleError::CompileErr("Invalid start of string.".to_string()))
            }?;
            
            let mut closed = false;
            
            //Collect all symbols until the closing quote, of the same type as the open, is reached.
            while let Some(next) = self.advance() {

                //Handle interpolated string
                if string_type == StringType::Interpolated {
                    if next == symbols::BACKTICK {
                        if let Some(top) = control_stack.last() {
                            //If top is a backtick, pop it
                            if top == &symbols::BACKTICK {
                                let _ = control_stack.pop();
                            }
                            //Otherwise, push it
                            else {
                                control_stack.push(symbols::BACKTICK);
                            }
                        }
    
                        //If the stack isn't empty, interpolated string isn't yet complete
                        if !control_stack.is_empty() {
                            continue;
                        }
                    }
                    else if next == symbols::L_BRACE {
                        //Do not push an escaped brace
                        if self.match_symbol(symbols::L_BRACE) {
                                
                        }
                        //Otherwise, if inside of a backtick, push to stack
                        else if let Some(top) = control_stack.last() {
                            if top == &symbols::BACKTICK {
                                control_stack.push(next);
                            }
                        }

                        continue;
                    }
                    else if next == symbols::R_BRACE {
                        //Do not pop for an escaped brace
                        if self.match_symbol(symbols::R_BRACE) {

                        }
                        //If right brace, and top of stack is left brace, pop
                        else if let Some(top) = control_stack.last() {
                            if top == &symbols::L_BRACE {
                                let _ = control_stack.pop();
                            }
                        }

                        continue;
                    }
                }

                if next == open {
                    closed = true;
                    break;
                }
                else if matches!(next, "\n" | "\r\n") {
                    self.line += 1;
                }
                //Do not close string on escaped quote
                else if next == symbols::B_SLASH {
                    let _ = self.match_symbol(open);
                }
            }
            
            if !closed {
                Err(GreyscaleError::CompileErr("Unterminated string literal".to_string()))
            }
            else {
                //Omit quotes from string tokens, except for interpolated
                let range = match string_type {
                    StringType::Interpolated => start..self.current,
                    _ => start + 1..self.current - 1
                };

                Ok(self.make_token(TokenType::String(range, string_type)))
            }
        }
        else {
            Err(GreyscaleError::CompileErr("Unexpected end of input".to_string()))
        }
    }
    
    fn match_number(&mut self) -> Result<Token, GreyscaleError> {
        let start = self.current;
        
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
                        if Self::is_digit(next, base) {
                            
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
                if Self::is_digit(next, base) {
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
                Ok(self.make_token(TokenType::Number(start..self.current, base)))
            }
        }
        else {
            Err(GreyscaleError::CompileErr("Unexpected end of input".to_string()))
        }
    }
    
    fn match_identifier(&mut self) -> Result<Token, GreyscaleError> {
        let start = self.current;

        //Match first char
        if let Some(next) = self.advance() {
            if !Self::is_alpha(next) {
                Err(GreyscaleError::CompileErr(format!("Unexpected symbol {next}")))
            }
            else {
                while let Some(next) = self.peek() {
                    if Self::is_alpha(next) || Self::is_digit(next, Base::Decimal) {
                        let _ = self.advance();
                    }
                    else {
                        break;
                    }
                }

                if let Some(keyword) = self.get_keyword(start..self.current) {
                    Ok(self.make_token(keyword))
                }
                else {
                    Ok(self.make_token(TokenType::Identifier(start..self.current))) 
                }
            }
        }
        else {
            Err(GreyscaleError::CompileErr("Unexpected end of input".to_string()))
        }
    }

    fn get_keyword(&self, range: Range<usize>) -> Option<TokenType> {
        let id = self.program[range].join("");
        let id = id.as_str();

        match id {
            symbols::IF => Some(TokenType::If),
            symbols::ELSE => Some(TokenType::Else),
            symbols::TRUE => Some(TokenType::True),
            symbols::FALSE => Some(TokenType::False),
            symbols::NULL => Some(TokenType::Null),
            symbols::LOOP => Some(TokenType::Loop),
            symbols::WHILE => Some(TokenType::While),
            symbols::FOR => Some(TokenType::For),
            symbols::CLASS => Some(TokenType::Class),
            symbols::SUPER => Some(TokenType::Super),
            symbols::THIS => Some(TokenType::This),
            symbols::LET => Some(TokenType::Let),
            symbols::FUNC => Some(TokenType::Func),
            symbols::PRINT => Some(TokenType::Print),
            symbols::RETURN => Some(TokenType::Return),
            _ => None
        }
    }

    fn is_digit(symbol: &str, base: Base) -> bool {
        if symbol.is_empty() {
            return false;
        }
        
        let chars = symbol.chars().collect::<Vec<char>>();

        if chars.len() != 1 {
            false
        }
        else {
            let c = *chars.first().unwrap();

            match base {
                Base::Binary => c == '0' || c == '1',
                Base::Decimal => ('0'..='9').contains(&c),
                Base::Hexadecimal => ('0'..='9').contains(&c)
                    || ('a'..='f').contains(&c)
                    || ('A'..='F').contains(&c)
            }
        }
    }

    fn is_alpha(symbol: &str) -> bool {
        if symbol.is_empty() {
            return false;
        }

        let chars = symbol.chars().collect::<Vec<char>>();

        if chars.len() != 1 {
            false
        }
        else {
            let c = *chars.first().unwrap();

            ('a'..='z').contains(&c)
                || ('A'..='Z').contains(&c)
                || c == '_'
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
        self.current >= self.end || 
            self.current >= self.program.len()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, GreyscaleError>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.scan_token()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LexerIterWithHistoryItem {
    pub state: LexerState,
    pub token: Result<Token, GreyscaleError>
}

impl LexerIterWithHistoryItem {
    pub fn new(state: LexerState, token: &Result<Token, GreyscaleError>) -> Self {
        Self {
            state,
            token: token.clone()
        }
    }
}

pub struct LexerIterWithHistory<'a> {
    lexer: Lexer<'a>,
    history: Vec<Option<LexerIterWithHistoryItem>>,
    n: usize
}

impl<'a> LexerIterWithHistory<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            history: Vec::new(),
            n: 0
        }
    }

    pub fn zero(&mut self) {
        self.n = 0;
    }

    pub fn clear(&mut self) {
        self.history.clear();
        self.n = 0;
    }

    pub fn current(&mut self) -> Option<LexerIterWithHistoryItem> {
        if self.n < self.history.len() {
            self.history[self.n].clone()
        }
        else {
            self.lex()
        }
    }

    pub fn current_token(&mut self) -> Option<Result<Token, GreyscaleError>> {
        let maybe_item = self.current();

        if let Some(item) = maybe_item {
            Some(item.token)
        }
        else {
            None
        }
    }

    pub fn move_by(&mut self, n: isize) -> Option<LexerIterWithHistoryItem> {
        let mut current = None;

        let maybe_end = self.n.checked_add_signed(n);

        let end = if let Some(e) = maybe_end {
            e
        }
        else if n < 0 {
            0
        }
        else {
            self.history.len() 
        };

        if end > self.n {
            for i in self.n..=end {
                self.n = i;
                current = Some(self.current()?);
            }
        }

        current
    }

    pub fn peek(&mut self) -> Option<Result<Token, GreyscaleError>> {
        self.peek_n(1)
    }

    pub fn peek_n(&mut self, n: isize) -> Option<Result<Token, GreyscaleError>> {
        self.n.checked_add_signed(n)?;
        let n_original = self.n;

        let result = self.move_by(n);

        self.n = n_original;

        if let Some(item) = result {
            Some(item.token)
        }
        else {
            None
        }
    }

    pub fn advance(&mut self) -> Option<LexerIterWithHistoryItem> {
        self.move_by(1)
    }
    
    pub fn rewind(&mut self) -> Option<LexerIterWithHistoryItem> {
        self.move_by(-1)
    }

    pub fn current_position(&self) -> usize {
        self.n
    }

    pub fn set_position(&mut self, n: usize) {
        self.n = n
    }

    pub fn get_inner_state(&self) -> LexerState {
        self.lexer.get_state()
    }

    pub fn clear_inner_and_set_state(&mut self, new_state: LexerState) {
        self.clear();
        self.lexer.set_state(new_state);
    }

    pub fn is_at_end(&self) -> bool {
        (self.n >= self.history.len() && self.lexer.is_at_end())
            || (!self.history.is_empty() && self.history.last().unwrap().is_none())
    }

    fn lex(&mut self) -> Option<LexerIterWithHistoryItem> {
        let current_state = self.lexer.get_state();
        let scanned = self.lexer.scan_token();

        if let Some(token) = scanned {
            self.history.push(Some(LexerIterWithHistoryItem::new(current_state, &token)));
            self.history.last().unwrap().clone()
        }
        else {
            self.history.push(None);
            None
        }
    }

}