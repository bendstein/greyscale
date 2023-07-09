use std::rc::Rc;

use crate::parser::ast;
use crate::chunk;
use crate::token::token_type::TokenType;
use crate::value::object::Object;
use crate::vm::error;
use crate::ops;
use crate::value;

use ast::expression as expr;
use ast::statement as stmt;

use ast::AST;
use expr::ExprNode;
use stmt::StmtNode;
use error::GreyscaleError;
use chunk::Chunk;
use value::Value;
use crate::location::Location;

#[allow(dead_code)]
pub struct Compiler<'a> {
    program: Rc<Vec<&'a str>>,
    errors: Vec<GreyscaleError>,
    chunk: Chunk
}

impl<'a> Compiler<'a> {
    pub fn compile_ast(program: Rc<Vec<&'a str>>, ast: AST) -> Result<Chunk, GreyscaleError> {
        let mut compiler = Self {
            program: Rc::clone(&program),
            errors: Vec::new(),
            chunk: Chunk::default()
        };

        for statement in ast.statements {
            match statement {
                crate::parser::ast::Node::Expression(n) => compiler.expr(*n),
                crate::parser::ast::Node::Statement(n) => compiler.stmt(*n),
            }
        }

        if compiler.errors.is_empty() {
            Ok(compiler.chunk)
        }
        else {
            Err(GreyscaleError::AggregateErr(compiler.errors))
        }
    }

    pub fn compile_expression(program: Rc<Vec<&'a str>>, expression: ExprNode) -> Result<Chunk, GreyscaleError> {
        let mut compiler = Self {
            program: Rc::clone(&program),
            errors: Vec::new(),
            chunk: Chunk::default()
        };

        compiler.expr(expression);

        if compiler.errors.is_empty() {
            Ok(compiler.chunk)
        }
        else {
            Err(GreyscaleError::AggregateErr(compiler.errors))
        }
    }

    fn expr(&mut self, expr: ExprNode) {
        match expr {
            ExprNode::Binary(binary, loc) => {
                self.expr_binary(binary, loc);
            },
            ExprNode::Unary(unary, loc) => {
                self.expr_unary(unary, loc);
            },
            ExprNode::Assignment(assign, loc) => {
                self.expr_assign(assign, loc);
            },
            ExprNode::Literal(literal, loc) => {
                self.expr_literal(literal, loc);
            },
            ExprNode::InterpolatedString(interp, loc) => {
                self.expr_interpolated_string(interp, loc);
            },
            ExprNode::Identifier(id, loc) => {
                self.expr_id(id, loc);
            },
            ExprNode::Function(_, loc) => {
                self.errors.push(GreyscaleError::CompileErr("Function expression compilation not yet implemented.".to_string(), loc));
            },
            ExprNode::Call(_, loc) => {
                self.errors.push(GreyscaleError::CompileErr("Call expression compilation not yet implemented.".to_string(), loc));
            },
        }
    }

    fn stmt(&mut self, stmt: StmtNode) {
        match stmt {
            StmtNode::Block(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Block statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::Conditional(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Conditional statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::Keyword(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Keyword statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::Declaration(declaration, loc, _) => {
                self.stmt_declaration(declaration, loc);
            },
            StmtNode::Expression(expression, _, end_loc) => {
                self.stmt_expression(expression, end_loc);
            },
            StmtNode::Print(print, loc, _) => {
                self.stmt_print(print, loc);
            },
            StmtNode::For(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("For statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::While(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("While statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::Loop(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Loop statement compilation not yet implemented.".to_string(), loc));
            },
            StmtNode::Return(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Return statement compilation not yet implemented.".to_string(), loc));
            },
        }
    }

    fn push_const(&mut self, code: u8, code_long: u8, value: Value, location: Location) -> usize {
        let const_count = self.chunk.count_consts();

        if const_count < u8::MAX as usize {
            let index = self.chunk.add_const(value) as u8;
            self.chunk.write(code, location.line);
            self.chunk.write(index, location.line);
            index as usize
        }
        else if const_count < u16::MAX as usize {
            let index = self.chunk.add_const(value) as u16;
            self.chunk.write(code_long, location.line);
            self.chunk.write_u16(index, location.line);
            index as usize
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} constants.", u16::MAX), location));
            u16::MAX as usize
        }
    }

}

//Expressions
impl<'a> Compiler<'a> {

    fn expr_literal(&mut self, lit: expr::Literal, location: Location) {
        match lit.value {
            ast::LiteralType::Void => {
                let value = Value::Void;
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            ast::LiteralType::Null => {
                let value = Value::Null;
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            ast::LiteralType::Boolean(b) => {
                let value = Value::Bool(b);
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            ast::LiteralType::String(s) => {
                let value = Value::Object(Rc::new(Object::String(s)));
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            ast::LiteralType::Double(n) => {
                let value = Value::Double(n);
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            ast::LiteralType::Integer(n) => {
                let value = Value::Int(n);
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
        }
    }

    fn expr_unary(&mut self, expr: expr::Unary, location: Location) {
        //Write operand
        self.expr(*expr.expr);

        //Write unary operators
        for op_token in expr.ops {
            let token_type = op_token.token_type();
            match token_type {
                TokenType::Minus => self.chunk.write(ops::OP_NEGATE, location.line),
                TokenType::Tilde => self.chunk.write(ops::OP_BITWISE_NOT, location.line),
                TokenType::Bang => self.chunk.write(ops::OP_LOGICAL_NOT, location.line),
                _ => {
                    self.errors.push(GreyscaleError::CompileErr(format!("Invalid unary operator '{}'.", 
                        token_type.as_string()), location));
                }
            }
        }
    }

    fn expr_binary(&mut self, expr: expr::Binary, location: Location) {
        //Write first operand
        self.expr(*expr.left);

        //For each following part, write the operand followed by the operator
        for operation in expr.right {
            self.expr(*operation.right);
            
            let token_type = operation.op.token_type();

            match token_type {
                TokenType::Plus => self.chunk.write(ops::OP_ADD, location.line),
                TokenType::Minus => self.chunk.write(ops::OP_SUBTRACT, location.line),
                TokenType::Star => self.chunk.write(ops::OP_MULTIPLY, location.line),
                TokenType::Slash => self.chunk.write(ops::OP_DIVIDE, location.line),
                TokenType::Percent => self.chunk.write(ops::OP_MODULUS, location.line),
                TokenType::PipePipe => self.chunk.write(ops::OP_LOGICAL_OR, location.line),
                TokenType::CaretCaret => self.chunk.write(ops::OP_LOGICAL_XOR, location.line),
                TokenType::AmpAmp => self.chunk.write(ops::OP_LOGICAL_AND, location.line),
                TokenType::Pipe => self.chunk.write(ops::OP_BITWISE_OR, location.line),
                TokenType::Caret => self.chunk.write(ops::OP_BITWISE_XOR, location.line),
                TokenType::Amp => self.chunk.write(ops::OP_BITWISE_AND, location.line),
                TokenType::LessLess => self.chunk.write(ops::OP_BITWISE_LSHIFT, location.line),
                TokenType::GreaterGreater => self.chunk.write(ops::OP_BITWISE_RSHIFT, location.line),
                TokenType::EqualEqual => self.chunk.write(ops::OP_EQUAL, location.line),
                TokenType::BangEqual => self.chunk.write(ops::OP_NOT_EQUAL, location.line),
                TokenType::Greater => self.chunk.write(ops::OP_GREATER, location.line),
                TokenType::GreaterEqual => self.chunk.write(ops::OP_GREATER_EQUAL, location.line),
                TokenType::Less => self.chunk.write(ops::OP_LESS, location.line),
                TokenType::LessEqual => self.chunk.write(ops::OP_LESS_EQUAL, location.line),
                _ => {
                    self.errors.push(GreyscaleError::CompileErr(format!("Invalid binary operator '{}'.", 
                        token_type.as_string()), location));
                }
            }
        }
    }

    fn expr_interpolated_string(&mut self, interp: expr::InterpolatedString, location: Location) {
        //If empty, push empty string literal
        if interp.segments.is_empty() {
            self.expr_literal(expr::Literal 
            { 
                value: ast::LiteralType::String(String::from(""))
            }, location);
        }
        else {
            let mut first: bool = true;

            //Concatenate segments
            for segment in interp.segments {
                let segment_location = segment.location();

                self.expr(segment);

                //Don't push concat operation after first
                if first {
                    first = false;
                }
                else {
                    self.chunk.write(ops::OP_CONCAT, segment_location.line);
                }
            }
        }
    }

    fn expr_id(&mut self, expr: expr::Identifier, location: Location) {
        //Get identifier
        let id_token = expr.id;
        let id_token_type = id_token.token_type();

        if let TokenType::Identifier(range) = id_token_type {
            let content = (&self.program)[range.clone()].join("");

            //Add id to constants table
            let id_value = Value::Object(Rc::new(Object::String(content)));
            self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, id_value, location);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                id_token_type.as_string()), location));
        }
    }

    fn expr_assign(&mut self, expr: expr::Assignment, location: Location) {
        //Get identifier
        let id_token = expr.id;
        let id_token_type = id_token.token_type();

        let id = if let TokenType::Identifier(range) = id_token_type {
            (&self.program)[range.clone()].join("")
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                id_token_type.as_string()), location));

                //TODO
            return;
        };

        let id_rc = Rc::new(Object::String(id));

        let assignment_token_type = expr.assignment_type.token_type();

        let maybe_infix_op = match assignment_token_type {
            TokenType::Equal => None,
            TokenType::PlusEqual => Some(ops::OP_ADD),
            TokenType::MinusEqual => Some(ops::OP_SUBTRACT),
            TokenType::StarEqual => Some(ops::OP_MULTIPLY),
            TokenType::SlashEqual => Some(ops::OP_DIVIDE),
            TokenType::PercentEqual => Some(ops::OP_MODULUS),
            TokenType::CaretEqual => Some(ops::OP_BITWISE_XOR),
            TokenType::CaretCaretEqual => Some(ops::OP_LOGICAL_XOR),
            TokenType::AmpEqual => Some(ops::OP_BITWISE_AND),
            TokenType::AmpAmpEqual => Some(ops::OP_LOGICAL_AND),
            TokenType::PipeEqual => Some(ops::OP_BITWISE_OR),
            TokenType::PipePipeEqual =>Some(ops::OP_LOGICAL_OR),
            TokenType::LessLessEqual => Some(ops::OP_BITWISE_LSHIFT),
            TokenType::GreaterGreaterEqual => Some(ops::OP_BITWISE_RSHIFT),
            _ => {
                self.errors.push(GreyscaleError::CompileErr(format!("Invalid assignment operator '{}'.", 
                    assignment_token_type.as_string()), location));
                    //TODO
                    return;
            }
        };

        //If this is a combined assignment, assign {current value} {op} {assigned expression}
        if let Some(infix_op) = maybe_infix_op {
            //Push identifier
            self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);

            //Push assigned expression
            self.expr(*expr.assignment);

            //Push infix op
            self.chunk.write(infix_op, location.line);
        }
        //Otherwise, just push assigned expression
        else {
            //Push assigned expression
            self.expr(*expr.assignment);
        }

        //Push assignment operator
        self.push_const(ops::OP_SET_GLOBAL, ops::OP_SET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
    }
}

//Statements
impl<'a> Compiler<'a> {
    fn stmt_expression(&mut self, stmt: stmt::Expression, end_loc: Location) {
        //Compile expression
        self.expr(*stmt.expression);

        //Push pop
        self.chunk.write(ops::OP_POP, end_loc.line);
    }

    fn stmt_print(&mut self, stmt: stmt::Print, loc: Location) {
        //Compile expression
        self.expr(*stmt.expression);

        //Push print
        self.chunk.write(ops::OP_PRINT, loc.line);
    }

    fn stmt_declaration(&mut self, stmt: stmt::Declaration, loc: Location) {
        //If there is an assignment, push it
        if let Some(assignment) = stmt.assignment {
            //Compile expression
            self.expr(*assignment);
        }
        //Otherwise, push null
        else {
            self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, Value::Null, loc);
        }

        //Get identifier
        let id_token = stmt.id;
        let id_token_type = id_token.token_type();

        if let TokenType::Identifier(range) = id_token_type {
            let content = (&self.program)[range.clone()].join("");

            //Add id to constants table and get its index
            let id_value = Value::Object(Rc::new(Object::String(content)));
            self.push_const(ops::OP_DEF_GLOBAL, ops::OP_DEF_GLOBAL_LONG, id_value, loc);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                id_token_type.as_string()), loc));
        }
    }
}