use std::rc::Rc;

use crate::{vm::error::{GreyscaleError}, lexer::{Lexer, LexerIterWithHistory, interpolated_string_lexer::InterpStringLexer, LexerState, LexerIterWithHistoryItem}, token::{Token, token_type::{TokenType, Base, StringType}}, constants};
use ast::{AST, Node};
use ast::expression::ExprNode;
use ast::statement::StmtNode;
use crate::location::Location;

use self::{ast::{expression::{BinaryRHS, Binary, Assignment, Unary, Call, Literal, Identifier, InterpolatedString}, LiteralType, statement::{Expression, Print, Declaration, Block}}, settings::ParserSettings};

pub mod ast;
pub mod settings;

pub struct Parser<'a> {
    program: Rc<Vec<&'a str>>,
    lexer: LexerIterWithHistory<'a>,
    errors: Vec<GreyscaleError>,
    settings: ParserSettings
}

lazy_static! {
    static ref OP_PRIORITY: Vec<Vec<TokenType>> = vec![
        //Logical or
        vec![TokenType::PipePipe],
        //Logical xor
        vec![TokenType::CaretCaret],
        //Logical and
        vec![TokenType::AmpAmp],
        //Bitwise or
        vec![TokenType::Pipe],
        //Bitwise xor
        vec![TokenType::Caret],
        //Bitwise and
        vec![TokenType::Amp],
        //Equality
        vec![TokenType::EqualEqual, TokenType::BangEqual],
        //Comparison
        vec![TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual],
        //Shift
        vec![TokenType::LessLess, TokenType::GreaterGreater],
        //Plus/Minus
        vec![TokenType::Plus, TokenType::Minus],
        //Multiply/divide/modulus
        vec![TokenType::Star, TokenType::Slash, TokenType::Percent]
    ]; 
}

impl<'a> Parser<'a> {
    pub fn parse(program: Rc<Vec<&'a str>>) -> Result<AST, GreyscaleError> {
        Self::parse_with_settings(program, ParserSettings::default())
    }

    pub fn parse_with_settings(program: Rc<Vec<&'a str>>, settings: ParserSettings) -> Result<AST, GreyscaleError> {
        let lexer = Lexer::new(Rc::clone(&program));

        let mut parser = Self {
            program: Rc::clone(&program),
            lexer: LexerIterWithHistory::new(lexer),
            errors: Vec::new(),
            settings
        };

        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<AST, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Program");
        }

        let mut statements: Vec<Node> = Vec::new();

        fn inner(parser: &mut Parser<'_>) -> Result<Node, GreyscaleError> {
            let maybe_statement = parser.statement()?
                .ok_or_else(|| parser.make_error("Expected a statement.".to_string(), 0))?;
            Ok(Node::Statement(Box::new(maybe_statement)))
        }

        while !self.is_at_end() {         
            //Skip any current whitespace in the lexer
            self.lexer.skip_comment_whitespace()?;

            //If skipping whitespace exhausts lexer, break
            if self.is_at_end() {
                break;
            }

            //Match next statement
            match inner(self) {
                Ok(node) => {
                    statements.push(node);
                },
                Err(err) => {
                    self.errors.push(err);

                    //Scan until the next synchronization token is found
                    self.sync();
                },
            }

            //If tracing, output error count, statement count, and tokens
            if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
                println!("--------------------------------------------");
                println!("Current error count: {}.", self.errors.len());
                println!("Current statement count: {}.", statements.len());

                let mut tokens: String = String::new();

                let position = self.lexer.current_position();

                for (i, maybe_item) in self.lexer.history().iter().enumerate() {

                    if i > position {
                        break;
                    }

                    let s: String = if let Some(item) = maybe_item.clone() {
                        if let Ok(token) = item.token {
                            token.token_type().as_program_string(&self.program)
                        }
                        else {
                            String::from("Err")
                        }
                    }
                    else {
                        String::from("None")
                    };

                    if i > 0 {
                        tokens = format!("{tokens}, {s}");
                    }
                    else {
                        tokens = s;
                    }
                }

                println!("Tokens in previous statement: {tokens}");
                
                if statements.is_empty() {
                    println!("Previous statement type: None");
                }
                else {
                    println!("Previous statement type: {}", statements.last().unwrap().name());
                }

                println!("--------------------------------------------");
            }

            //Clear saved token history after each statement
            self.lexer.clear();
        }

        if self.errors.is_empty() {
            Ok(AST::new(statements))
        }
        else {
            Err(GreyscaleError::AggregateErr(self.errors.clone()))
        }
    }

    fn statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Statement");
        }

        //Try match block statement
        if let Some(block_stmt) = self.block_statement()? {
            return Ok(Some(block_stmt));
        }

        //Try match print statement
        if let Some(print_stmt) = self.print_statement()? {
            return Ok(Some(print_stmt));
        }

        //Try match declaration statement
        if let Some(decl_stmt) = self.declaration_statement()? {
            return Ok(Some(decl_stmt));
        }

        //Try match expression statement
        if let Some(expr_stmt) = self.expression_statement()? {
            return Ok(Some(expr_stmt));
        }

        Ok(None)
    }

    fn expression(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: expression");
        }

        //Start at lowest priority
        self.assignment_expr()
    }

    fn make_error(&self, message: String, position: usize) -> GreyscaleError {
        GreyscaleError::CompileErr(message, self.location_at_position(position))
    }

    fn location_at_position(&self, position: usize) -> Location {
        let history: Vec<LexerIterWithHistoryItem> = self.lexer.history().iter()
            .take_while(|h| h.is_some())
            .map(|h| h.clone().unwrap())
            .collect();

        if history.is_empty() || position >= history.len() {
            let state = self.lexer.get_inner_state();
            Location {
                column: state.column,
                line: state.line
            }
        }
        else {
            let item = history[position].clone();
            Location {
                column: item.state.column,
                line: item.state.line
            }
        }
    }

    //An error occurred. Scan until a synchronization token is found before continuing
    fn sync(&mut self) {
        while !self.is_at_end() {
            //Check for sync token
            if let Some(Ok(prev_token)) = self.lexer.peek_n(-1) {
                //Ignore tokenization error, it should already be reported
                let prev_token_type = prev_token.token_type();

                //If token is a sync token, stop and continue parsing
                if matches!(prev_token_type, TokenType::Semi | TokenType::RBrace | TokenType::RBrac | TokenType::RParen) {
                    break;
                }
            }

            if let Some(next) = self.lexer.current_token() {
                match next {
                    //If tokenization error, report and continue
                    Err(next_err) => {
                        self.errors.push(next_err);
                    },
                    Ok(next_token) => {
                        let next_token_type = next_token.token_type();

                        //If next token is sync token, stop and continue parsing
                        if matches!(next_token_type, TokenType::Class | TokenType::Func | TokenType::Let | TokenType::For
                            | TokenType::Loop | TokenType::While | TokenType::If | TokenType::Else | TokenType::Print | TokenType::Return) {
                            break;
                        }
                    }
                }
            }

            //Advance lexer
            self.lexer.advance();
        }
    }

    fn is_at_end(&self) -> bool {
        self.lexer.is_at_end()
    }
}

//Expression types
impl<'a> Parser<'a> {
    fn assignment_expr(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Assignment Expression");
        }

        let start_position = self.lexer.current_position();

        //Match id assignment_op expression 

        //Match higher priority expression
        if let Some(lhs) = self.binary_op()? {
            //Match assignment
            if let Some(token) = self.lexer.current_token() {
                let assignment_token = token?;
                let token_type = assignment_token.token_type();

                //If token is the assignment operator =, or any of the combined assignment operators (i.e. +=)
                if matches!(token_type, TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                    TokenType::StarEqual | TokenType::SlashEqual | TokenType::PercentEqual | TokenType::CaretEqual |
                    TokenType::CaretCaretEqual | TokenType::AmpEqual | TokenType::AmpAmpEqual | TokenType::PipeEqual |
                    TokenType::PipePipeEqual | TokenType::LessLessEqual | TokenType::GreaterGreaterEqual) {
                        //Make sure lhs is a valid target for assignment
                        if let ExprNode::Identifier(id_expr, _) = lhs {
                            let id = id_expr.id;

                            self.lexer.advance();

                            let rhs_position = self.lexer.current_position();

                            //Match expression of this priority or higher
                            let rhs = self.assignment_expr()?
                                .ok_or_else(|| self.make_error(format!("Expected an expression on right-hand side of binary operator '{}'.", token_type.as_string()), rhs_position))?;
    
                            return Ok(Some(ExprNode::Assignment(Assignment { 
                                id, 
                                assignment_type: assignment_token, 
                                assignment: Box::new(rhs) 
                            }, self.location_at_position(start_position))))
                        }
                        else {
                            return Err(self.make_error("Left-hand side is not a valid target for assignment.".to_string(), start_position));
                        }
                    }
            }

            //Not an assignment expression. Return lhs.
            return Ok(Some(lhs));
        }

        //Not an expression
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn binary_op(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Binary Infix Expression");
        }

        self.binary_op_priority(0)
    }

    fn binary_op_priority(&mut self, priority: usize) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Binary Infix Expression {priority}");
        }

        let start_position = self.lexer.current_position();

        fn call_next(parser: &mut Parser<'_>, current_priority: usize) -> Result<Option<ExprNode>, GreyscaleError> {
            let next_priority = current_priority + 1;

            if next_priority < OP_PRIORITY.len() {
                parser.binary_op_priority(next_priority)
            }
            else {
                parser.prefix()
            }
        }

        //match (higher priority expression) (operator (higher priority expression))*
        let maybe_lhs = call_next(self, priority)?;

        if let Some(lhs) = maybe_lhs {
            let mut adjoining: Vec<BinaryRHS> = Vec::new();

            let allowed_tokens = &OP_PRIORITY[priority];

            //Collect right hand side
            loop {
                let loop_start_position = self.lexer.current_position();

                //Match operator
                if let Some(token) = self.lexer.current_token() {
                    let op_token = token?;
                    let token_type = op_token.token_type();

                    //If operator is of this priority
                    if allowed_tokens.contains(token_type) {
                        self.lexer.advance();

                        let segment_position = self.lexer.current_position();

                        //Match next segment
                        let next = call_next(self, priority)?
                            .ok_or_else(|| self.make_error(format!("Expected an expression on right-hand side of binary operator '{}'.", token_type.as_string()), segment_position))?;
    
                        //Push operator and expression to stack
                        adjoining.push(BinaryRHS { 
                            op: op_token, 
                            right: Box::new(next) 
                        });
    
                        continue;
                    }
                }
                
                //Done with expression priority
                self.lexer.set_position(loop_start_position);
                break;
            }

            //If no adjoining, return lhs, otherwise, return lhs and chain of binary expressions
            if adjoining.is_empty() {
                return Ok(Some(lhs));
            }
            else {
                //Return chain of binary expressions
                return Ok(Some(ExprNode::Binary(Binary {
                    left: Box::new(lhs),
                    right: adjoining
                }, self.location_at_position(start_position))))
            }
        }

        //Not a binary expression of this priority or above
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn prefix(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Unary Prefix Expression");
        }

        let start_position = self.lexer.current_position();

        //Collect unary prefixes of the same priority
        let mut prefixes: Vec<Token> = Vec::new();

        loop {
            if let Some(token) = self.lexer.current_token() {
                let prefix_token = token?;

                //Collect prefix and advance. If not prefix, break.
                if matches!(prefix_token.token_type(), TokenType::Bang | TokenType::Minus | TokenType::Tilde) {
                    self.lexer.advance();
                    prefixes.push(prefix_token);
                    continue;
                }
                else {
                    break;
                }
            }

            //No token found. Break.
            break;
        }

        if !prefixes.is_empty() {
            let last_prefix_token_type = prefixes.last().unwrap().token_type();

            //There is at least one unary prefix; match rhs
            let next = self.call()?
                .ok_or_else(|| self.make_error(format!("Expected an expression on right-hand side of unary operator '{}'.", last_prefix_token_type.as_string()), start_position))?;

            return Ok(Some(ExprNode::Unary(Unary {
                ops: prefixes,
                expr: Box::new(next)
            }, self.location_at_position(start_position))));
        }

        //Not a unary expression, roll down to next priority
        self.lexer.set_position(start_position);
        self.call()
    }

    fn call(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Call Expression");
        }

        let start_position = self.lexer.current_position();

        //Match expression of next priority
        let maybe_func = self.func()?;

        if let Some(expr) = maybe_func {
            //Match and collect calls
            let mut calls: Vec<Vec<ExprNode>> = Vec::new();

            loop {
                let loop_start = self.lexer.current_position();

                //Match l paren
                if let Some(token) = self.lexer.current_token() {
                    let lparen_token = token?;
                    
                    if lparen_token.token_type() == &TokenType::LParen {
                        self.lexer.advance();

                        let mut arguments: Vec<ExprNode> = Vec::new();

                        loop {
                            let args_start = self.lexer.current_position();
    
                            //Try to match argument
                            let maybe_expr = self.expression()?;
    
                            if let Some(expr) = maybe_expr {
                                arguments.push(expr);

                                if let Some(token) = self.lexer.current_token() {
                                    let next_token = token?;
                                    let token_type = next_token.token_type();

                                    //If next symbol is a comma, require that the follwoing symbol isn't a right paren
                                    if token_type == &TokenType::Comma {
                                        if let Some(token) = self.lexer.peek_n(1) {
                                            let rparen_token = token?;

                                            if rparen_token.token_type() == &TokenType::RParen {
                                                return Err(self.make_error("Unexpected trailing comma at end of call.".to_string(), args_start));
                                            }
                                        }

                                        //Continue to match arguments
                                        self.lexer.advance();
                                    }
                                    else if token_type == &TokenType::RParen {
                                        //Done match arguments
                                        break;
                                    }
                                    else {
                                        return Err(self.make_error(format!("Unexpected token '{}' in call.", token_type.as_program_string(&self.program)), args_start));
                                    }
                                }
                            }
                            //Not an argument
                            else {
                                self.lexer.set_position(args_start);
                                break;
                            }
                        }

                        //Match right paren
                        if let Some(token) = self.lexer.current_token() {
                            let rparen_token = token?;

                            if rparen_token.token_type() == &TokenType::RParen {
                                self.lexer.advance();
                                calls.push(arguments);
                                continue;
                            }
                        }

                        return Err(self.make_error("Unterminated call operator.".to_string(), loop_start));
                    }
                }

                //Not a call
                self.lexer.set_position(loop_start);
                break;
            }

            //If no calls, just the expression
            if calls.is_empty() {
                return Ok(Some(expr));
            }
            //Otherwise, expression plus chain of calls
            else {
                return Ok(Some(ExprNode::Call(Call {
                    callable: Box::new(expr),
                    calls
                }, self.location_at_position(start_position))));
            }
        }

        //Not a call expression or higher priority
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn func(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Fucntion Expression");
        }

        let maybe_func = self.func_inline()?;

        if let Some(func) = maybe_func {
            return Ok(Some(func));
        }

        let maybe_func = self.func_block()?;

        if let Some(func) = maybe_func {
            return Ok(Some(func))
        }

        //Roll down to next priority
        self.primary()
    }

    fn func_inline(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Lambda Expression");
        }

        //TODO
        Ok(None)
    }

    fn func_block(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Function Block Expression");
        }

        //TODO
        Ok(None)
    }

    fn primary(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Primary Expression");
        }

        let start_position = self.lexer.current_position();

        if let Some(token) = self.lexer.current_token() {
            let primary_token = token?;
            let token_type = primary_token.token_type();

            //Match identifier
            if let TokenType::Identifier(_) = token_type {
                self.lexer.advance();
                return Ok(Some(ExprNode::Identifier(Identifier {
                    id: primary_token
                }, self.location_at_position(start_position))));
            }
            //Match literals
            else if let TokenType::String(_, _) = token_type {
                self.lexer.advance();
                
                //Handle string literal
                return self.parse_string(primary_token, self.location_at_position(start_position)).map(Some);
            }
            else if let TokenType::Number(_, _, _) = token_type {
                self.lexer.advance();

                //Handle number literal
                return self.parse_number(primary_token, self.location_at_position(start_position)).map(Some);
            }
            else if token_type == &TokenType::True {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Boolean(true)
                }, self.location_at_position(start_position))));
            }
            else if token_type == &TokenType::False {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Boolean(false)
                }, self.location_at_position(start_position))));
            }
            else if token_type == &TokenType::Null {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Null
                }, self.location_at_position(start_position))));
            }
            //Match expression in parentheses
            else if token_type == &TokenType::LParen {
                self.lexer.advance();

                let maybe_inner = self.expression()?;

                if let Some(inner) = maybe_inner {
                    if let Some(token) = self.lexer.current_token() {
                        let rparen_token = token?;

                        if rparen_token.token_type() == &TokenType::RParen {
                            self.lexer.advance();

                            //Return the expression inside of the parentheses
                            return Ok(Some(inner));
                        }
                    }
                }
                else {
                    return Err(self.make_error("Expected an expression after left paren.".to_string(), start_position));
                }
            }
        }

        //Not an expression
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn parse_number(&self, token: Token, start_loc: Location) -> Result<ExprNode, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Number Expression");
        }

        let token_type = token.token_type();

        if let TokenType::Number(range, dot_location, base) = token_type {
            
            fn get_digit(d: &str) -> u8 {
                match d {
                    "0" => 0,
                    "1" => 1,
                    "2" => 2,
                    "3" => 3,
                    "4" => 4,
                    "5" => 5,
                    "6" => 6,
                    "7" => 7,
                    "8" => 8,
                    "9" => 9,
                    "a" | "A" => 10,
                    "b" | "B" => 11,
                    "c" | "C" => 12,
                    "d" | "D" => 13,
                    "e" | "E" => 14,
                    "f" | "F" => 15,
                    _ => 0
                }
            }

            let base_num = match base {
                Base::Decimal => 10,
                Base::Binary => 2,
                Base::Hexadecimal => 16
            };

            //Parse as decimal
            if let Some(radix) = dot_location {
                let mut n: f64 = 0_f64;

                let start_exp: i32 = *radix as i32;
                let end_exp: i32 = (range.end - range.start - radix) as i32;
                
                for (index, i) in (0..start_exp).enumerate() {
                    let d = self.program[range.start + index];
                    let digit = get_digit(d);
                    n += (digit as f64) * (base_num as f64).powi(*radix as i32 - 1 - i);
                }

                for (index, i) in (1..end_exp).enumerate() {
                    let d = self.program[range.start + *radix + 1 + index];
                    let digit = get_digit(d);
                    n += (digit as f64) / (base_num as f64).powi(i);
                }

                return Ok(ExprNode::Literal(Literal {
                    value: LiteralType::Double(n)
                }, start_loc))
            }
            //Parse as integer
            else {
                let mut n: i64 = 0_i64;

                let end_exp: u32 = (range.end - range.start) as u32;
                
                for (index, i) in (0..end_exp).enumerate() {
                    let d = self.program[range.start + index];
                    let digit = get_digit(d);
                    n += (digit as i64) * (base_num as i64).pow(end_exp - 1 - i);
                }

                return Ok(ExprNode::Literal(Literal {
                    value: LiteralType::Integer(n)
                }, start_loc))
            }
        }

        Err(GreyscaleError::CompileErr("Expected a number.".to_string(), start_loc))
    }

    fn parse_string(&self, token: Token, start_loc: Location) -> Result<ExprNode, GreyscaleError>  {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: String Expression");
        }

        let token_type = token.token_type().clone();

        if let TokenType::String(range, string_type) = token_type {

            match string_type {
                StringType::Literal => {
                    let content = self.program[range].join("");
                    return Ok(ExprNode::Literal(Literal {
                        value: LiteralType::String(content)
                    }, start_loc));
                },
                StringType::Interpolated => {
                    let mut segments: Vec<ExprNode> = Vec::new();

                    //let lexer_current_state = self.lexer.get_inner_state();

                    //Create a tokenizer for the interpolated string, positioned at the current token
                    let mut interp_lexer = InterpStringLexer::new(self.program.clone())
                        .with_state(LexerState {
                            start: range.start,
                            current: range.start,
                            line: start_loc.line,
                            column: start_loc.column,
                            end: range.end
                        });

                    let mut prev_state = interp_lexer.get_state();

                    //Recursively parse subtokens
                    while let Some(interp_token_result) = interp_lexer.scan_token() {
                        let current_state = prev_state;
                        prev_state = interp_lexer.get_state();

                        let interp_token = interp_token_result?;

                        if let TokenType::String(interp_range, segment_type) = interp_token.token_type() {

                            let line = current_state.line;
                            let column = if segment_type.is_interpolated_segment() {
                                current_state.column + 1
                            }
                            else {
                                current_state.column
                            };

                            let token_lexer = Lexer::new(self.program.clone())
                                .with_state(LexerState {
                                    start: interp_range.start,
                                    current: interp_range.start,
                                    line,
                                    column,
                                    end: interp_range.end
                                });

                            let mut token_parser = Self {
                                program: self.program.clone(),
                                lexer: LexerIterWithHistory::new(token_lexer),
                                errors: Vec::new(),
                                settings: self.settings
                            };

                            let expr = match segment_type {
                                //Recursively parse expression
                                StringType::InterpolatedSegment => {
                                    token_parser.expression()?
                                        .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression".to_string(), start_loc))
                                },
                                //Handle segment as string
                                _ => token_parser.parse_string(interp_token, Location {
                                    line,
                                    column
                                })
                            }?;

                            segments.push(expr);
                        }
                        else {
                            return Err(GreyscaleError::CompileErr("Unexpected token in interpolated string.".to_string(), start_loc));
                        }
                    }

                    //If no segments, return an empty string literal, otherwise
                    //return an interpolated string with each concatenated segment
                    if segments.is_empty() {
                        return Ok(ExprNode::Literal(Literal {
                            value: LiteralType::String("".to_string())
                        }, start_loc));
                    }
                    else {
                        return Ok(ExprNode::InterpolatedString(InterpolatedString 
                        { 
                            segments
                        }, start_loc));
                    }
                },
                StringType::InterpolatedSegment => {
                    return Err(GreyscaleError::CompileErr("Unexpected token.".to_string(), start_loc));
                },
                StringType::Escaped => {
                    return Err(GreyscaleError::CompileErr("Escape sequences are not yet supported".to_string(), start_loc));
                },
            }
        }
        
        Err(GreyscaleError::CompileErr("Expected a string.".to_string(), start_loc))
    }
}

//Statement types
impl<'a> Parser<'a>  {
    fn expression_statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Expression Statement");
        }

        let start_position = self.lexer.current_position();

        //Match an expression
        let maybe_expr = self.expression()?;

        if let Some(expr) = maybe_expr {
            //Match semicolon, allowing implicit if enabled
            self.match_semicolon(self.settings.allow_implicit_final_semicolon)?;
            
            //Return expression statement
            return Ok(Some(StmtNode::Expression(Expression {
                expression: Box::new(expr)
            }, self.location_at_position(start_position),
            self.location_at_position(self.lexer.current_position()))));
        }

        //Not an expression statement
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn print_statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Print Statement");
        }

        let start_position = self.lexer.current_position();

        //Match keyword print
        if let Some(token) = self.lexer.current_token() {
            let printtoken = token?;

            let token_type = printtoken.token_type();
            if token_type == &TokenType::Print {
                //Advance lexer
                self.lexer.advance();

                let expr_position = self.lexer.current_position();

                //Match an expression
                if let Some(expr) = self.expression()? {
                    //Match semicolon, allowing implicit if enabled
                    self.match_semicolon(self.settings.allow_implicit_final_semicolon)?;

                    //Return print statement
                    return Ok(Some(StmtNode::Print(Print {
                        expression: Box::new(expr)
                    }, self.location_at_position(start_position),
                    self.location_at_position(self.lexer.current_position()))));
                }
                else {
                    return Err(self.make_error(format!("Expected an expression after keyword '{}'.", token_type.as_string()), expr_position));
                }
            }
        }

        //Not an expression statement
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn declaration_statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Declaration Statement");
        }

        let start_position = self.lexer.current_position();

        //Match keyword let
        if let Some(token) = self.lexer.current_token() {
            let let_token = token?;
            let let_token_type = let_token.token_type();

            if let_token_type == &TokenType::Let {
                //Advance lexer
                self.lexer.advance();

                let id_position = self.lexer.current_position();

                //Match identifier
                if let Some(token) = self.lexer.current_token() {
                    let id_token = token?;
                    let id_token_type = id_token.token_type();

                    if let TokenType::Identifier(_) = id_token_type {
                        //Advance lexer
                        self.lexer.advance();

                        //If next token is =, match assignment as well
                        let mut assignment: Option<ExprNode> = None;

                        if let Some(token) = self.lexer.current_token() {
                            let assign_token = token?;
                            let assign_token_type = assign_token.token_type();

                            if let TokenType::Equal = assign_token_type {
                                //Advance lexer
                                self.lexer.advance();

                                let expr_position = self.lexer.current_position();

                                //Match an expression
                                if let Some(expr) = self.expression()? {
                                    assignment = Some(expr);
                                }
                                else {
                                    return Err(self.make_error(format!("Expected an expression after assignment operator '{}'.", assign_token_type.as_string()), expr_position));
                                }
                            }
                        }

                        //Match semicolon, allowing implicit if enabled
                        self.match_semicolon(self.settings.allow_implicit_final_semicolon)?;

                        //Return declaration statement
                        return Ok(Some(StmtNode::Declaration(Declaration {
                            id: id_token,
                            assignment: assignment.map(Box::from)
                        }, self.location_at_position(start_position),
                        self.location_at_position(self.lexer.current_position()))));
                    }
                    else {
                        return Err(self.make_error(format!("Expected an identifier after keyword '{}'.", let_token_type.as_string()), id_position));
                    }
                }
            }
        }

        //Not a declaration statement
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn block_statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Block Statement");
        }

        let start_position = self.lexer.current_position();

        //Match token {
        if let Some(token) = self.lexer.current_token() {
            let lbrace_token = token?;
            let lbrace_token_type = lbrace_token.token_type();

            if let TokenType::LBrace = lbrace_token_type {
                //Advance lexer
                self.lexer.advance();

                //Match statements in block
                let mut statements: Vec<StmtNode> = Vec::new();

                while let Some(token) = self.lexer.current_token() {
                    let loop_start = self.lexer.current_position();
                    
                    let rbrace_token = token?;
                    let rbrace_token_type = rbrace_token.token_type();

                    if let TokenType::RBrace = rbrace_token_type {
                        //Advance lexer and return block
                        self.lexer.advance();
                        
                        return Ok(Some(StmtNode::Block(Block {
                            statements
                        }, self.location_at_position(start_position),
                        self.location_at_position(self.lexer.current_position()))));
                    }

                    //If terminating bracket wasn't found, match statement
                    let stmt = self.statement()?
                        .ok_or_else(|| self.make_error("Expected a statement.".to_string(), loop_start))?;
                    statements.push(stmt);
                }

                //If this point was reached, the block was missing a terminating }
                return Err(self.make_error("Unterminated block.".to_string(), start_position));
            }
        }

        //Not a block statement
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn match_semicolon(&mut self, allow_implicit: bool) -> Result<(), GreyscaleError> {
        if (constants::TRACE & constants::TRACE_PARSER) == constants::TRACE_PARSER {
            println!("Parser: Semicolon");
        }

        let start_position = self.lexer.current_position();

        //Match a semicolon
        if let Some(token) = self.lexer.current_token() {
            let semitoken = token?;

            if let TokenType::Semi = semitoken.token_type() {
                self.lexer.advance();

                return Ok(());
            }
        }
        //If implicit final semicolons are enabled, don't require a semicolon
        else if allow_implicit {
            return Ok(());
        }

        //No semicolon was found
        Err(self.make_error("Expected a semicolon.".to_string(), start_position))
    }
}