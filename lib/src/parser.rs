use crate::{vm::error::GreyscaleError, util::string::GraphemeString, lexer::{Lexer, LexerIterWithHistory}, token::{Token, token_type::{TokenType, Base, StringType}}};
use ast::{AST, Node};
use ast::expression::{ExprNode};
use ast::statement::{StmtNode};

use self::ast::{expression::{BinaryRHS, Binary, Assignment, Unary, Call, Literal, Identifier}, LiteralType, statement::Expression};

pub mod ast;

pub struct Parser<'a> {
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
    pub fn parse(program: &'a str) -> Result<AST, Vec<GreyscaleError>> {
        let gprogram = GraphemeString::new(program);
        let lexer = Lexer::new(gprogram);

        let mut parser = Self {
            lexer: LexerIterWithHistory::new(lexer),
            errors: Vec::new()
        };

        parser.parse_program()
    }

    fn parse_program(&mut self) -> Result<AST, Vec<GreyscaleError>> {
        let mut statements: Vec<Node> = Vec::new();

        fn inner(parser: &mut Parser<'_>) -> Result<Node, GreyscaleError> {
            let maybe_statement = parser.statement()?
                .ok_or_else(|| GreyscaleError::CompileErr("Expected a statement.".to_string()))?;
            Ok(Node::Statement(Box::new(maybe_statement)))
        }

        while !self.is_at_end() {
            //Clear saved token history at beginning of statement
            self.lexer.clear();

            match inner(self) {
                Ok(node) => {
                    statements.push(node);
                },
                Err(err) => {
                    self.errors.push(err)
                },
            }
        }

        if self.errors.is_empty() {
            Ok(AST::new(statements))
        }
        else {
            Err(self.errors.clone())
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
                    self.lexer.advance();

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
            else if let TokenType::String(range, string_type) = token_type {
                self.lexer.advance();

                let content = self.program().substring(range);

                match string_type {
                    StringType::Literal => {
                        return Ok(Some(ExprNode::Literal(Literal {
                            value: LiteralType::String(content)
                        })));
                    },
                    StringType::Interpolated => {
                        return Err(GreyscaleError::CompileErr("Interpolated strings are not yet supported".to_string()));
                    },
                    StringType::InterpolatedSegment => {
                        return Err(GreyscaleError::CompileErr("Interpolated strings are not yet supported".to_string()));
                    },
                    StringType::Escaped => {
                        return Err(GreyscaleError::CompileErr("Escape sequences are not yet supported".to_string()));
                    },
                }
            }
            else if let TokenType::Number(range, number_base) = token_type {
                self.lexer.advance();

                let content = self.program().substring(range);

                match number_base {
                    Base::Decimal => {
                        return Ok(Some(ExprNode::Literal(Literal {
                            value: LiteralType::Double(content.parse::<f64>().unwrap())
                        })))
                    },
                    Base::Binary => {
                        return Err(GreyscaleError::CompileErr("Binary numbers are not yet supported".to_string()));
                    },
                    Base::Hexadecimal => {
                        return Err(GreyscaleError::CompileErr("Hex numbers are not yet supported".to_string()));
                    },
                }
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

    fn program(&self) -> &GraphemeString<'a> {
        self.lexer.program()
    }

    fn is_at_end(&self) -> bool {
        self.lexer.is_at_end()
    }
}