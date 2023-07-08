use std::rc::Rc;

use crate::{parser::ast::{AST, statement::StmtNode, expression::{ExprNode, Literal}}, chunk::Chunk, vm::error::GreyscaleError, ops, value::Value};

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
            ExprNode::Binary(_) => {
                self.errors.push(GreyscaleError::CompileErr("Binary expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Unary(_) => {
                self.errors.push(GreyscaleError::CompileErr("Unary expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Assignment(_) => {
                self.errors.push(GreyscaleError::CompileErr("Assignment expression compilation not yet implemented.".to_string()));
            },
            ExprNode::Literal(literal) => {
                self.literal(literal);
            },
            ExprNode::InterpolatedString(_) => {
                self.errors.push(GreyscaleError::CompileErr("InterpolatedString expression compilation not yet implemented.".to_string()));
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
            StmtNode::Expression(_) => {
                self.errors.push(GreyscaleError::CompileErr("Expression statement compilation not yet implemented.".to_string()));
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

    fn literal(&mut self, lit: Literal) {
        match lit.value {
            crate::parser::ast::LiteralType::Void => {
                self.errors.push(GreyscaleError::CompileErr("Void literal compilation not yet implemented.".to_string()));
            },
            crate::parser::ast::LiteralType::Null => {
                self.errors.push(GreyscaleError::CompileErr("Null literal compilation not yet implemented.".to_string()));
            },
            crate::parser::ast::LiteralType::Boolean(b) => {
                self.errors.push(GreyscaleError::CompileErr("Boolean literal compilation not yet implemented.".to_string()));
            },
            crate::parser::ast::LiteralType::String(s) => {
                self.errors.push(GreyscaleError::CompileErr("String literal compilation not yet implemented.".to_string()));
            },
            crate::parser::ast::LiteralType::Double(n) => {
                let const_count = self.chunk.count_consts();
                let value = Value::Double(n);

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
            },
            crate::parser::ast::LiteralType::Integer(n) => {
                let const_count = self.chunk.count_consts();
                let value = Value::Double(n as f64);

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
            },
        }
    }

}