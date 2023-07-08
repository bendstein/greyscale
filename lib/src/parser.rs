use std::rc::Rc;

use crate::{vm::error::GreyscaleError, lexer::{Lexer, LexerIterWithHistory, interpolated_string_lexer::InterpStringLexer, LexerState}, token::{Token, token_type::{TokenType, Base, StringType}}};
use ast::{AST, Node};
use ast::expression::ExprNode;
use ast::statement::StmtNode;

use self::ast::{expression::{BinaryRHS, Binary, Assignment, Unary, Call, Literal, Identifier, InterpolatedString}, LiteralType, statement::Expression};

pub mod ast;

pub struct Parser<'a> {
    program: Rc<Vec<&'a str>>,
    lexer: LexerIterWithHistory<'a>,
    errors: Vec<GreyscaleError>
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
        let lexer = Lexer::new(Rc::clone(&program));

        let mut parser = Self {
            program: Rc::clone(&program),
            lexer: LexerIterWithHistory::new(lexer),
            errors: Vec::new()
        };

        parser.parse_program()
    }

    pub fn parse_expression(expression: Rc<Vec<&'a str>>) -> Result<ExprNode, GreyscaleError> {
        let lexer = Lexer::new(Rc::clone(&expression));

        let mut parser = Self {
            program: Rc::clone(&expression),
            lexer: LexerIterWithHistory::new(lexer),
            errors: Vec::new()
        };

        parser.expression()?
            .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression".to_string()))
    }

    fn parse_program(&mut self) -> Result<AST, GreyscaleError> {
        let mut statements: Vec<Node> = Vec::new();

        fn inner(parser: &mut Parser<'_>) -> Result<Node, GreyscaleError> {
            let maybe_statement = parser.statement()?
                .ok_or_else(|| GreyscaleError::CompileErr("Expected a statement.".to_string()))?;
            Ok(Node::Statement(Box::new(maybe_statement)))
        }

        while !self.is_at_end() {
            match inner(self) {
                Ok(node) => {
                    statements.push(node);
                },
                Err(err) => {
                    self.errors.push(err)
                },
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
        let maybe_expr_stmt = self.expression_statement()?;

        if let Some(expr_stmt) = maybe_expr_stmt {
            return Ok(Some(expr_stmt));
        }

        Ok(None)
    }

    fn expression_statement(&mut self) -> Result<Option<StmtNode>, GreyscaleError> {
        let start_position = self.lexer.current_position();

        //Match an expression
        let maybe_expr = self.expression()?;

        if let Some(expr) = maybe_expr {
            //Match a semicolon
            if let Some(token) = self.lexer.current_token() {
                let semitoken = token?;

                if semitoken.token_type() == &TokenType::Semi {
                    //self.lexer.advance();

                    return Ok(Some(StmtNode::Expression(Expression {
                        expression: Box::new(expr)
                    })));
                }
            }

            //No semicolon was found
            return Err(GreyscaleError::CompileErr("Expected a semicolon.".to_string()));
        }

        //Not an expression statement
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn expression(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        //Start at lowest priority
        self.assignment_expr()
    }

    fn assignment_expr(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        let start_position = self.lexer.current_position();

        //Match id assignment_op expression 

        //Match identifier
        if let Some(token) = self.lexer.current_token() {
            let id_token = token?;

            if let TokenType::Identifier(_) = id_token.token_type() {
                self.lexer.advance();

                //Match assignment
                if let Some(token) = self.lexer.current_token() {
                    let assignment_token = token?;

                    //If token is the assignment operator =, or any of the combined assignment operators (i.e. +=)
                    if matches!(assignment_token.token_type(), TokenType::Equal | TokenType::PlusEqual | TokenType::MinusEqual |
                        TokenType::StarEqual | TokenType::SlashEqual | TokenType::PercentEqual | TokenType::CaretEqual |
                        TokenType::CaretCaretEqual | TokenType::AmpEqual | TokenType::AmpAmpEqual | TokenType::PipeEqual |
                        TokenType::PipePipeEqual | TokenType::LessLessEqual | TokenType::GreaterGreaterEqual) {
                            self.lexer.advance();

                            //Parse RHS
                            let rhs = self.expression()?
                                .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression.".to_string()))?;

                            return Ok(Some(ExprNode::Assignment(Assignment { 
                                id: id_token, 
                                assignment_type: assignment_token, 
                                assignment: Box::new(rhs) 
                            })))
                        }
                }
            }
        }

        //Not an assignment expression; move to next highest priority
        self.lexer.set_position(start_position);
        self.binary_op()
    }

    fn binary_op(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        self.binary_op_priority(0)
    }

    fn binary_op_priority(&mut self, priority: usize) -> Result<Option<ExprNode>, GreyscaleError> {
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

                    //If operator is of this priority
                    if allowed_tokens.contains(op_token.token_type()) {
                        self.lexer.advance();

                        //Match next segment
                        let next = call_next(self, priority)?
                            .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression.".to_string()))?;
    
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
                })))
            }
        }

        //Not a binary expression of this priority or above
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn prefix(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
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
            //There is at least one unary prefix; match rhs
            let next = self.call()?
                .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression.".to_string()))?;

            return Ok(Some(ExprNode::Unary(Unary {
                ops: prefixes,
                expr: Box::new(next)
            })));
        }

        //Not a unary expression, roll down to next priority
        self.lexer.set_position(start_position);
        self.call()
    }

    fn call(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
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

                                    //If next symbol is a comma, require that the follwoing symbol isn't a right paren
                                    if next_token.token_type() == &TokenType::Comma {
                                        if let Some(token) = self.lexer.peek_n(1) {
                                            let rparen_token = token?;

                                            if rparen_token.token_type() == &TokenType::RParen {
                                                return Err(GreyscaleError::CompileErr("Unexpected trailing comma.".to_string()));
                                            }
                                        }

                                        //Continue to match arguments
                                        self.lexer.advance();
                                    }
                                    else if next_token.token_type() == &TokenType::RParen {
                                        //Done match arguments
                                        break;
                                    }
                                    else {
                                        return Err(GreyscaleError::CompileErr("Unexpected token.".to_string()));
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

                        return Err(GreyscaleError::CompileErr("Unterminated call.".to_string()));
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
                })));
            }
        }

        //Not a call expression or higher priority
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn func(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
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
        //TODO
        Ok(None)
    }

    fn func_block(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        //TODO
        Ok(None)
    }

    fn primary(&mut self) -> Result<Option<ExprNode>, GreyscaleError> {
        let start_position = self.lexer.current_position();

        if let Some(token) = self.lexer.current_token() {
            let primary_token = token?;
            let token_type = primary_token.token_type();

            //Match identifier
            if let TokenType::Identifier(_) = token_type {
                self.lexer.advance();
                return Ok(Some(ExprNode::Identifier(Identifier {
                    id: primary_token
                })));
            }
            //Match literals
            else if let TokenType::String(_, _) = token_type {
                self.lexer.advance();
                
                //Handle string literal
                return self.parse_string(primary_token).map(Some);
            }
            else if let TokenType::Number(_, _, _) = token_type {
                self.lexer.advance();

                //Handle number literal
                return self.parse_number(primary_token).map(Some);
            }
            else if token_type == &TokenType::True {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Boolean(true)
                })));
            }
            else if token_type == &TokenType::False {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Boolean(false)
                })));
            }
            else if token_type == &TokenType::Null {
                self.lexer.advance();
                return Ok(Some(ExprNode::Literal(Literal {
                    value: ast::LiteralType::Null
                })));
            }
            //Match expression in parentheses
            else if token_type == &TokenType::LParen {
                self.lexer.advance();

                let maybe_inner = self.expression()?;

                if let Some(inner) = maybe_inner {
                    if let Some(token) = self.lexer.current_token() {
                        let rparen_token = token?;

                        if rparen_token.token_type() == &TokenType::RParen {
                            //Return the expression inside of the parentheses
                            return Ok(Some(inner));
                        }
                    }
                }
                else {
                    return Err(GreyscaleError::CompileErr("Expected an expression.".to_string()));
                }
            }
        }

        //Not an expression
        self.lexer.set_position(start_position);
        Ok(None)
    }

    fn parse_number(&self, token: Token) -> Result<ExprNode, GreyscaleError> {
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
                }))
            }
            //Parse as integer
            else {
                let mut n: u64 = 0_u64;

                let end_exp: u32 = (range.end - range.start) as u32;
                
                for (index, i) in (0..end_exp).enumerate() {
                    let d = self.program[range.start + index];
                    let digit = get_digit(d);
                    n += (digit as u64) * (base_num as u64).pow(end_exp - 1 - i);
                }

                return Ok(ExprNode::Literal(Literal {
                    value: LiteralType::Integer(n)
                }))
            }
        }

        Err(GreyscaleError::CompileErr("Expected a number.".to_string()))
    }

    fn parse_string(&self, token: Token) -> Result<ExprNode, GreyscaleError>  {
        let token_type = token.token_type().clone();

        if let TokenType::String(range, string_type) = token_type {

            match string_type {
                StringType::Literal => {
                    let content = self.program[range].join("");
                    return Ok(ExprNode::Literal(Literal {
                        value: LiteralType::String(content)
                    }));
                },
                StringType::Interpolated => {
                    let mut segments: Vec<ExprNode> = Vec::new();

                    //Create a tokenizer for the interpolated string, positioned at the current token
                    let interp_lexer = InterpStringLexer::new(self.program.clone())
                        .with_state(LexerState {
                            start: range.start,
                            current: range.start,
                            line: self.lexer.get_inner_state().line,
                            end: range.end
                        });

                    //Recursively parse subtokens
                    for interp_token_result in interp_lexer {
                        let interp_token = interp_token_result?;

                        if let TokenType::String(interp_range, segment_type) = interp_token.token_type() {
                            let token_lexer = Lexer::new(self.program.clone())
                                .with_state(LexerState {
                                    start: interp_range.start,
                                    current: interp_range.start,
                                    line: self.lexer.get_inner_state().line,
                                    end: interp_range.end
                                });

                            let mut token_parser = Self {
                                program: self.program.clone(),
                                lexer: LexerIterWithHistory::new(token_lexer),
                                errors: Vec::new()
                            };

                            let expr = match segment_type {
                                //Recursively parse expression
                                StringType::InterpolatedSegment => {
                                    token_parser.expression()?
                                        .ok_or_else(|| GreyscaleError::CompileErr("Expected an expression".to_string()))
                                },
                                //Handle segment as string
                                _ => token_parser.parse_string(interp_token)
                            }?;

                            segments.push(expr);
                        }
                        else {
                            return Err(GreyscaleError::CompileErr("Unexpected token in interpolated string.".to_string()));
                        }
                    }

                    //If no segments, return an empty string literal, otherwise
                    //return an interpolated string with each concatenated segment
                    if segments.is_empty() {
                        return Ok(ExprNode::Literal(Literal {
                            value: LiteralType::String("".to_string())
                        }));
                    }
                    else {
                        return Ok(ExprNode::InterpolatedString(InterpolatedString 
                        { 
                            segments
                        }));
                    }
                },
                StringType::InterpolatedSegment => {
                    return Err(GreyscaleError::CompileErr("Unexpected token.".to_string()));
                },
                StringType::Escaped => {
                    return Err(GreyscaleError::CompileErr("Escape sequences are not yet supported".to_string()));
                },
            }
        }
        
        Err(GreyscaleError::CompileErr("Expected a string.".to_string()))
    }

    fn is_at_end(&self) -> bool {
        self.lexer.is_at_end()
    }
}