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
            ExprNode::Binary(binary) => {
                self.expr_binary(binary);
            },
            ExprNode::Unary(unary) => {
                self.expr_unary(unary);
            },
            ExprNode::Assignment(_) => {
                self.errors.push(GreyscaleError::CompileErr("Assignment expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Literal(literal) => {
                self.expr_literal(literal);
            },
            ExprNode::InterpolatedString(interp) => {
                self.expr_interpolated_string(interp);
            },
            ExprNode::Identifier(_) => {
                self.errors.push(GreyscaleError::CompileErr("Identifier expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Function(_) => {
                self.errors.push(GreyscaleError::CompileErr("Function expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Call(_) => {
                self.errors.push(GreyscaleError::CompileErr("Call expression compilation not yet implemented.".to_string()));
            },
        }
    }

    fn stmt(&mut self, stmt: StmtNode) {
        match stmt {
            StmtNode::Block(_) => {
                self.errors.push(GreyscaleError::CompileErr("Block statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Conditional(_) => {
                self.errors.push(GreyscaleError::CompileErr("Conditional statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Keyword(_) => {
                self.errors.push(GreyscaleError::CompileErr("Keyword statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Declaration(_) => {
                self.errors.push(GreyscaleError::CompileErr("Declaration statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Expression(expression) => {
                self.stmt_expression(expression);
            },
            StmtNode::For(_) => {
                self.errors.push(GreyscaleError::CompileErr("For statement compilation not yet implemented.".to_string()));
            },
            StmtNode::While(_) => {
                self.errors.push(GreyscaleError::CompileErr("While statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Loop(_) => {
                self.errors.push(GreyscaleError::CompileErr("Loop statement compilation not yet implemented.".to_string()));
            },
            StmtNode::Return(_) => {
                self.errors.push(GreyscaleError::CompileErr("Return statement compilation not yet implemented.".to_string()));
            },
        }
    }

    fn push_const(&mut self, value: Value) {
        let const_count = self.chunk.count_consts();

        if const_count <= u8::MAX as usize {
            let index = self.chunk.add_const(value) as u8;
            self.chunk.write(ops::OP_CONSTANT);
            self.chunk.write(index);
        }
        else if const_count <= u16::MAX as usize {
            let index = self.chunk.add_const(value) as u16;
            self.chunk.write(ops::OP_CONSTANT_LONG);
            self.chunk.write_u16(index);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} constants.", u16::MAX)));
        }
    }

}

//Expressions
impl<'a> Compiler<'a> {

    fn expr_literal(&mut self, lit: expr::Literal) {
        match lit.value {
            ast::LiteralType::Void => {
                let value = Value::Void;
                self.push_const(value);
            },
            ast::LiteralType::Null => {
                let value = Value::Null;
                self.push_const(value);
            },
            ast::LiteralType::Boolean(b) => {
                let value = Value::Bool(b);
                self.push_const(value);
            },
            ast::LiteralType::String(s) => {
                let value = Value::Object(Rc::new(Object::String(s)));
                self.push_const(value);
            },
            ast::LiteralType::Double(n) => {
                let value = Value::Double(n);
                self.push_const(value);
            },
            ast::LiteralType::Integer(n) => {
                let value = Value::Int(n);
                self.push_const(value);
            },
        }
    }

    fn expr_unary(&mut self, expr: expr::Unary) {
        //Write operand
        self.expr(*expr.expr);

        //Write unary operators
        for op_token in expr.ops {
            let token_type = op_token.token_type();
            match token_type {
                TokenType::Minus => self.chunk.write(ops::OP_NEGATE),
                TokenType::Tilde => self.chunk.write(ops::OP_BITWISE_NOT),
                TokenType::Bang => self.chunk.write(ops::OP_LOGICAL_NOT),
                _ => {
                    self.errors.push(GreyscaleError::CompileErr(format!("Invalid unary operator '{}'.", 
                        token_type.as_string())));
                }
            }
        }
    }

    fn expr_binary(&mut self, expr: expr::Binary) {
        //Write first operand
        self.expr(*expr.left);

        //For each following part, write the operand followed by the operator
        for operation in expr.right {
            self.expr(*operation.right);
            
            let token_type = operation.op.token_type();

            match token_type {
                TokenType::Plus => self.chunk.write(ops::OP_ADD),
                TokenType::Minus => self.chunk.write(ops::OP_SUBTRACT),
                TokenType::Star => self.chunk.write(ops::OP_MULTIPLY),
                TokenType::Slash => self.chunk.write(ops::OP_DIVIDE),
                TokenType::Percent => self.chunk.write(ops::OP_MODULUS),
                TokenType::PipePipe => self.chunk.write(ops::OP_LOGICAL_OR),
                TokenType::CaretCaret => self.chunk.write(ops::OP_LOGICAL_XOR),
                TokenType::AmpAmp => self.chunk.write(ops::OP_LOGICAL_AND),
                TokenType::Pipe => self.chunk.write(ops::OP_BITWISE_OR),
                TokenType::Caret => self.chunk.write(ops::OP_BITWISE_XOR),
                TokenType::Amp => self.chunk.write(ops::OP_BITWISE_AND),
                TokenType::LessLess => self.chunk.write(ops::OP_BITWISE_LSHIFT),
                TokenType::GreaterGreater => self.chunk.write(ops::OP_BITWISE_RSHIFT),
                TokenType::EqualEqual => self.chunk.write(ops::OP_EQUAL),
                TokenType::BangEqual => self.chunk.write(ops::OP_NOT_EQUAL),
                TokenType::Greater => self.chunk.write(ops::OP_GREATER),
                TokenType::GreaterEqual => self.chunk.write(ops::OP_GREATER_EQUAL),
                TokenType::Less => self.chunk.write(ops::OP_LESS),
                TokenType::LessEqual => self.chunk.write(ops::OP_LESS_EQUAL),
                _ => {
                    self.errors.push(GreyscaleError::CompileErr(format!("Invalid unary operator '{}'.", 
                        token_type.as_string())));
                }
            }
        }
    }

    fn expr_interpolated_string(&mut self, interp: expr::InterpolatedString) {
        //If empty, push empty string literal
        if interp.segments.is_empty() {
            self.expr_literal(expr::Literal 
            { 
                value: ast::LiteralType::String(String::from(""))
            });
        }
        else {
            let mut first: bool = true;

            //Concatenate segments
            for segment in interp.segments {
                self.expr(segment);

                //Don't push concat operation after first
                if first {
                    first = false;
                }
                else {
                    self.chunk.write(ops::OP_CONCAT);
                }
            }
        }
    }
}

//Statements
impl<'a> Compiler<'a> {
    fn stmt_expression(&mut self, stmt: stmt::Expression) {
        //Compile expression
        self.expr(*stmt.expression);
    }
}