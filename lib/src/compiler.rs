use std::rc::Rc;

use crate::chunk::Chunk;
use crate::parser::ast;
use crate::parser::ast::LiteralType;
use crate::parser::ast::expression::Literal;
use crate::token::token_type::TokenType;
use crate::value::Values;
use crate::value::object;
use crate::value::object::Function;
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
use value::Value;
use crate::location::Location;

/**
 * Types of structures that can be affected by break/continue/etc
 */
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ControllableType {
    #[default]
    Loop,
    Switch,
    Catch,
    Top
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub struct Controllable {
    pub ctype: ControllableType,
    pub depth: usize,
    pub patch_start: usize,
    pub patch_end: usize
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct Upvalue {
    pub index: usize,
    pub is_local: bool,
    pub name: String
}

pub struct Compiler<'a> {
    program: Rc<Vec<&'a str>>,
    errors: Vec<GreyscaleError>,
    target: Function,
    constants: Values,
    locals: Vec<Vec<String>>,
    upvals: Vec<Upvalue>,
    enclosing_locals: Option<Vec<Vec<String>>>,
    parent_enclosing_locals: Option<Vec<Vec<String>>>,
    enclosing_upvals: Vec<Upvalue>,
    controllables: Vec<Controllable>
}

impl<'a> Compiler<'a> {
    pub fn compile_ast(program: Rc<Vec<&'a str>>, ast: AST) -> Result<Function, GreyscaleError> {
        let mut compiler = Self {
            program: Rc::clone(&program),
            errors: Vec::new(),
            target: Function::default(),
            constants: Values::default(),
            locals: Vec::new(),
            upvals: Vec::new(),
            enclosing_locals: None,
            parent_enclosing_locals: None,
            enclosing_upvals: Vec::new(),
            controllables: Vec::new()
        };

        //Claim locals slot 0 for internal use
        compiler.locals.push(vec![String::new()]);

        let mut last_location = Location::new(0, 0);

        for statement in ast.statements {
            match statement {
                crate::parser::ast::Node::Expression(n) => {
                    last_location = n.location();
                    compiler.expr(*n)
                },
                crate::parser::ast::Node::Statement(n) => {
                    last_location = n.end_location();
                    compiler.stmt(*n)
                },
            }
        }

        //Push return void
        compiler.expr_literal(Literal {
            value: LiteralType::Void
        }, last_location);
        
        //Push return keyword
        compiler.target.chunk.write(ops::OP_RETURN, last_location.line);

        if compiler.errors.is_empty() {
            Ok(compiler.target)
        }
        else {
            Err(GreyscaleError::AggregateErr(compiler.errors))
        }
    }

    pub fn compile_expression(program: Rc<Vec<&'a str>>, expression: ExprNode) -> Result<Function, GreyscaleError> {
        let mut compiler = Self {
            program: Rc::clone(&program),
            errors: Vec::new(),
            target: Function::default(),
            constants: Values::default(),
            locals: Vec::new(),
            upvals: Vec::new(),
            parent_enclosing_locals: None,
            enclosing_locals: None,
            enclosing_upvals: Vec::new(),
            controllables: Vec::new()
        };
        
        //Claim locals slot 0 for internal use
        compiler.locals.push(vec![String::new()]);

        compiler.expr(expression);

        if compiler.errors.is_empty() {
            Ok(compiler.target)
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
            ExprNode::Function(func, loc) => {
                self.expr_function(func, loc);
            },
            ExprNode::Call(call, _) => {
                self.expr_call(call);
            },
        }
    }

    fn stmt(&mut self, stmt: StmtNode) {
        match stmt {
            StmtNode::Block(block, _, end_loc) => {
                self.stmt_block(block, end_loc);
            },
            StmtNode::Conditional(conditional, loc, _) => {
                self.stmt_conditional(conditional, loc);
            },
            StmtNode::Keyword(stmt, loc, _) => {
                self.stmt_keyword(stmt, loc);
            },
            StmtNode::Declaration(declaration, loc, _) => {
                self.stmt_declaration(declaration, loc);
            },
            StmtNode::Expression(expression, _, end_loc) => {
                self.stmt_expression(expression, end_loc);
            },
            StmtNode::For(stmt, loc, end_loc) => {
                self.stmt_for(stmt, loc, end_loc);
            },
            StmtNode::While(stmt, loc, _) => {
                self.stmt_while(stmt, loc);
            },
            StmtNode::Loop(stmt, loc, _) => {
                self.stmt_loop(stmt, loc);
            },
            StmtNode::Return(stmt, loc, _) => {
                self.stmt_return(stmt, loc);
            },
        }
    }

    fn push_const(&mut self, code: u8, code_long: u8, value: Value, location: Location) -> usize {
        //If constant exists, get its index
        let index = if let Some(i) = self.constants.index_of(&value) {
            i
        }
        //Otherwise, if there is space for a new constant, add it to the chunk, and the compiler's collection of constants
        else if self.target.chunk.count_consts() < u16::MAX as usize  {
            self.constants.write(value.clone());
            self.target.chunk.add_const(value)
        }
        //Otherwise, out of range
        else {
            u16::MAX as usize
        };

        if index < u8::MAX as usize {
            self.target.chunk.write(code, location.line);
            self.target.chunk.write(index as u8, location.line);
        }
        else if index < u16::MAX as usize {
            self.target.chunk.write(code_long, location.line);
            self.target.chunk.write_u16(index as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} constants.", u16::MAX), location));
        }

        index
    }

    fn push_local(&mut self, code: u8, code_long: u8, slot: usize, location: Location) {
        if slot < u8::MAX as usize {
            self.target.chunk.write(code, location.line);
            self.target.chunk.write(slot as u8, location.line);
        }
        else if slot < u16::MAX as usize {
            self.target.chunk.write(code_long, location.line);
            self.target.chunk.write_u16(slot as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} locals.", u16::MAX), location));
        }
    }

    fn push_jump(&mut self, code: u8, location: Location) -> usize {
        self.target.chunk.write(code, location.line);
        //Get the address we're going to write the offset to so we can patch it later
        let count = self.target.chunk.count();
        self.target.chunk.write_u16(u16::MAX, location.line);
        count
    }

    fn patch_jump(&mut self, offset: usize, location: Location) {
        let jump_to = self.target.chunk.count().saturating_sub(offset + 2);

        if jump_to <= u16::MAX as usize {
            self.target.chunk.patch_u16(offset, jump_to as u16);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot jump over {} lines.", u16::MAX), location));
        }
    }

    fn push_loop(&mut self, loop_start: usize, location: Location) {
        self.target.chunk.write(ops::OP_LOOP, location.line);

        let offset = (self.target.chunk.count() + 2).saturating_sub(loop_start);

        if offset <= u16::MAX as usize {
            self.target.chunk.write_u16(offset as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot jump over {} lines.", u16::MAX), location));
        }
    }

    fn resolve_local(locals: &[Vec<String>], name: &str) -> Option<usize> {
        for i in (0..locals.len()).rev() {
            let scope = &locals[i];

            for j in (0..scope.len()).rev() {
                let elem = &scope[j];

                if elem == name {
                    let mut position = j;

                    for item in locals.iter().take(i) {
                        position += item.len();
                    }

                    return Some(position);
                }
            }
        }
        
        None
    }

    fn resolve_upval(&mut self, name: &str) -> Option<usize> {
        if let Some(enclosing) = &self.enclosing_locals {
            let index = Self::resolve_local(enclosing, name);

            if let Some(local) = index {
                //Add the upvalue and return its index
                return Some(self.add_upval(name.to_string(), local, true));
            }
        }

        //Check parent's context
        if let Some(enclosing) = &self.parent_enclosing_locals {
            let index = Self::resolve_local(enclosing, name);

            if let Some(local) = index {
                //Add the upvalue and return its index
                return Some(self.add_upval(name.to_string(), local, false));
            }
        }

        None
    }

    fn add_upval(&mut self, name: String, index: usize, is_local: bool) -> usize {
        //Convert the current target to a closure
        self.target.convert_to_closure();

        //Make sure current upval doesn't already exist
        for upval in &self.upvals {
            if upval.index == index && upval.is_local == is_local {
                return upval.index;
            }
        }

        let mut on_parent = false;

        //Make sure current upval doesn't exist on paren
        for upval in &self.enclosing_upvals {
            if upval.index == index {
                on_parent = true;
                break;
            }
        }

        //Create upval and increment target captured count
        self.upvals.push(Upvalue {
            index,
            is_local: !on_parent && is_local,
            name
        });

        self.target.add_upval()
    }

}

//Expressions
impl<'a> Compiler<'a> {
    fn expr_literal(&mut self, lit: expr::Literal, location: Location) {
        match lit.value {
            LiteralType::Void => {
                let value = Value::Void;
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            LiteralType::Null => {
                let value = Value::Null;
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            LiteralType::Boolean(b) => {
                let value = Value::Bool(b);
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            LiteralType::String(s) => {
                let value = Value::Object(Rc::new(Object::String(s)));
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            LiteralType::Double(n) => {
                let value = Value::Double(n);
                self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, location);
            },
            LiteralType::Integer(n) => {
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
                TokenType::Minus => self.target.chunk.write(ops::OP_NEGATE, location.line),
                TokenType::Tilde => self.target.chunk.write(ops::OP_BITWISE_NOT, location.line),
                TokenType::Bang => self.target.chunk.write(ops::OP_LOGICAL_NOT, location.line),
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

        //For each following part, write the operand and operator
        for operation in expr.right {
            let token_type = operation.op.token_type();

            //Handle short circuiting ops
            let short_circuit_patch_loc = match token_type {
                TokenType::PipePipe => Some(self.short_circuit(ops::OP_JUMP_IF_TRUE, location)),
                TokenType::AmpAmp => Some(self.short_circuit(ops::OP_JUMP_IF_FALSE, location)),
                _ => None
            };

            self.expr(*operation.right);

            //If short circuiting operator, patch jump location to here to prevent eval of the rhs
            if let Some(patch_at) = short_circuit_patch_loc {
                self.patch_jump(patch_at, location);
            }
            
            match token_type {
                TokenType::PipePipe => {}, //Short circuiting ops already handled
                TokenType::AmpAmp => {},
                TokenType::Plus => self.target.chunk.write(ops::OP_ADD, location.line),
                TokenType::Minus => self.target.chunk.write(ops::OP_SUBTRACT, location.line),
                TokenType::Star => self.target.chunk.write(ops::OP_MULTIPLY, location.line),
                TokenType::Slash => self.target.chunk.write(ops::OP_DIVIDE, location.line),
                TokenType::Percent => self.target.chunk.write(ops::OP_MODULUS, location.line),
                TokenType::CaretCaret => self.target.chunk.write(ops::OP_LOGICAL_XOR, location.line),
                TokenType::Pipe => self.target.chunk.write(ops::OP_BITWISE_OR, location.line),
                TokenType::Caret => self.target.chunk.write(ops::OP_BITWISE_XOR, location.line),
                TokenType::Amp => self.target.chunk.write(ops::OP_BITWISE_AND, location.line),
                TokenType::LessLess => self.target.chunk.write(ops::OP_BITWISE_LSHIFT, location.line),
                TokenType::GreaterGreater => self.target.chunk.write(ops::OP_BITWISE_RSHIFT, location.line),
                TokenType::EqualEqual => self.target.chunk.write(ops::OP_EQUAL, location.line),
                TokenType::BangEqual => self.target.chunk.write(ops::OP_NOT_EQUAL, location.line),
                TokenType::Greater => self.target.chunk.write(ops::OP_GREATER, location.line),
                TokenType::GreaterEqual => self.target.chunk.write(ops::OP_GREATER_EQUAL, location.line),
                TokenType::Less => self.target.chunk.write(ops::OP_LESS, location.line),
                TokenType::LessEqual => self.target.chunk.write(ops::OP_LESS_EQUAL, location.line),
                _ => {
                    self.errors.push(GreyscaleError::CompileErr(format!("Invalid binary operator '{}'.", 
                        token_type.as_string()), location));
                }
            }
        }
    }

    fn short_circuit(&mut self, short_circuit_op: u8, location: Location) -> usize {
        let short_circuit_loc = self.push_jump(short_circuit_op, location);

        //If didn't short circuit, pop value from stack so next part can be evaluated
        self.target.chunk.write(ops::OP_POP, location.line);

        //Return patch location
        short_circuit_loc
    }

    fn expr_interpolated_string(&mut self, interp: expr::InterpolatedString, location: Location) {
        //If empty, push empty string literal
        if interp.segments.is_empty() {
            self.expr_literal(expr::Literal 
            { 
                value: LiteralType::String(String::from(""))
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
                    self.target.chunk.write(ops::OP_CONCAT, segment_location.line);
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

            //If id is a local
            if let Some(index) = Self::resolve_local(&self.locals, &content) {
                self.push_local(ops::OP_GET_LOCAL, ops::OP_GET_LOCAL_LONG, index, location);
            }
            //If id is an upvalue
            else if let Some(index) = self.resolve_upval(&content) {
                self.push_local(ops::OP_GET_UPVAL, ops::OP_GET_UPVAL_LONG, index, location);
            }
            //If it's global or undefined
            else {
                //Add id to constants table
                let id_value = Value::Object(Rc::new(Object::String(content)));
                self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, id_value, location);
            }
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                id_token_type.as_string()), location));
        }
    }

    fn expr_function(&mut self, expr: expr::Function, loc: Location) {
        let name = match expr.name {
            None => "".to_string(),
            Some(t) => {
                let token_type = t.token_type();
                        
                if let TokenType::Identifier(range) = &token_type {
                    (&self.program)[range.clone()].join("")
                }
                else {
                    "".to_string()
                }
            }
        };

        let mut func_compiler = Compiler {
            program: Rc::clone(&self.program),
            errors: Vec::new(),
            target: Function {
                arity: expr.args.len() as u8,
                chunk: Chunk::default(),
                func_type: object::FunctionType::Function(if name.is_empty() {
                    "anon".to_string()
                } else {
                    name.clone()
                }),
            },
            constants: self.constants.clone(),
            locals: vec![ vec![ name ] ],
            upvals: Vec::new(),
            parent_enclosing_locals: if let Some(enclosing_locals) = &self.parent_enclosing_locals {
                let mut enclosing = enclosing_locals.clone();
                enclosing.extend(self.locals.clone());
                Some(enclosing)
            }
            else {
                Some(self.locals.clone())
            },
            enclosing_locals: Some(self.locals.clone()),
            enclosing_upvals: self.upvals.clone(),
            controllables: Vec::new(),
        };

        //Push local scope
        func_compiler.locals.push(Vec::new());

        //Push argument names as locals
        for arg in &expr.args {
            let id_token_type = arg.token_type();

            let content = if let TokenType::Identifier(range) = arg.token_type() {
                (&self.program)[range.clone()].join("")
            }
            else {
                func_compiler.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                    id_token_type.as_string()), loc));
                    return;
            };

            //Value is on top of the stack, push local to current scope in compiler
            func_compiler.locals.last_mut().unwrap().push(content);
        }

        //Compile function body
        match expr.body {
            expr::FunctionType::Block(b) => {
                let loc = b.end_location();
                func_compiler.stmt(*b);

                //Push return void for if function doesn't return
                func_compiler.expr_literal(Literal {
                    value: LiteralType::Void
                }, loc);

                func_compiler.target.chunk.write(ops::OP_RETURN, loc.line);
            },
            expr::FunctionType::Inline(i) => {
                let loc = i.location();
                func_compiler.expr(*i);

                //Push return
                func_compiler.target.chunk.write(ops::OP_RETURN, loc.line);
            },
        }

        //Write function
        let is_closure = func_compiler.target.func_type.is_closure();

        //For any non-local upvalues, add them to this as well
        let inner_upvals: Vec<&Upvalue> = func_compiler.upvals.iter()
            .filter(|u| !u.is_local)
            .collect();

        if !inner_upvals.is_empty() {
            for uv in inner_upvals {
                self.resolve_upval(&uv.name); 
            }
        }

        let value = Value::Object(Rc::new(Object::Function(func_compiler.target)));

        //If function is closure, push as closure
        if is_closure {

            self.push_const(ops::OP_CLOSURE, ops::OP_CLOSURE_LONG, value, loc);

            //Emit upvals
            for upval in func_compiler.upvals {
                if upval.index < u8::MAX as usize {
                    self.target.chunk.write(u8::from(upval.is_local), loc.line); //1 or 0
                    self.target.chunk.write(upval.index as u8, loc.line);
                }
                else if upval.index < u16::MAX as usize {
                    self.target.chunk.write(u8::from(upval.is_local) + 2, loc.line); //3 or 2
                    self.target.chunk.write_u16(upval.index as u16, loc.line);
                }
                else {
                    self.errors.push(GreyscaleError::CompileErr(format!("Closure cannot capture more than {} variables.", u16::MAX), loc));
                }
            }
        }
        //Otherwise, push as constant
        else {
            self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, value, loc);
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
            return;
        };

        let id_index = Self::resolve_local(&self.locals, &id);
        let upval_index = if id_index.is_none() {
            self.resolve_upval(&id) 
        } else {
            None
        };

        let id_rc = Rc::new(Object::String(id));

        let assignment_token_type = expr.assignment_type.token_type();

        let maybe_infix_op = match assignment_token_type {
            TokenType::Equal => None,
            TokenType::PipePipeEqual => None, //Short circuit ops handled elsewhere
            TokenType::AmpAmpEqual => None,
            TokenType::PlusEqual => Some(ops::OP_ADD),
            TokenType::MinusEqual => Some(ops::OP_SUBTRACT),
            TokenType::StarEqual => Some(ops::OP_MULTIPLY),
            TokenType::SlashEqual => Some(ops::OP_DIVIDE),
            TokenType::PercentEqual => Some(ops::OP_MODULUS),
            TokenType::CaretEqual => Some(ops::OP_BITWISE_XOR),
            TokenType::CaretCaretEqual => Some(ops::OP_LOGICAL_XOR),
            TokenType::AmpEqual => Some(ops::OP_BITWISE_AND),
            TokenType::PipeEqual => Some(ops::OP_BITWISE_OR),
            TokenType::LessLessEqual => Some(ops::OP_BITWISE_LSHIFT),
            TokenType::GreaterGreaterEqual => Some(ops::OP_BITWISE_RSHIFT),
            _ => {
                self.errors.push(GreyscaleError::CompileErr(format!("Invalid assignment operator '{}'.", 
                    assignment_token_type.as_string()), location));
                return;
            }
        };

        //If this is a combined assignment, assign {current value} {op} {assigned expression}
        if let Some(infix_op) = maybe_infix_op {

            //If id is a local
            if let Some(index) = id_index {
                //Push local slot
                self.push_local(ops::OP_GET_LOCAL, ops::OP_GET_LOCAL_LONG, index, location);
            }
            //If id is an upvalue
            else if let Some(index) = upval_index {
                //Push upval
                self.push_local(ops::OP_GET_UPVAL, ops::OP_GET_UPVAL_LONG, index, location);
            }
            //If it's a global or undefined
            else {
                //Push identifier
                self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
            }

            //Push assigned expression
            self.expr(*expr.assignment);

            //Push infix op
            self.target.chunk.write(infix_op, location.line);
        }
        //Handle short circuting operators
        else if matches!(assignment_token_type, TokenType::AmpAmpEqual | TokenType::PipePipeEqual) {

            //LHS
            //If id is a local
            if let Some(index) = id_index {
                //Push local slot
                self.push_local(ops::OP_GET_LOCAL, ops::OP_GET_LOCAL_LONG, index, location);
            }
            //If id is an upvalue
            else if let Some(index) = upval_index {
                //Push upval
                self.push_local(ops::OP_GET_UPVAL, ops::OP_GET_UPVAL_LONG, index, location);
            }
            //If it's a global or undefined
            else {
                //Push identifier
                self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
            }

            //Short-circuiting op
            let patch_at = if let TokenType::PipePipeEqual = assignment_token_type {
                self.short_circuit(ops::OP_JUMP_IF_TRUE, location)
            }
            else {
                self.short_circuit(ops::OP_JUMP_IF_FALSE, location)
            };

            //Push assigned expression
            self.expr(*expr.assignment);

            //Patch jump location to here to prevent eval of the rhs
            self.patch_jump(patch_at, location);
        }
        //Otherwise, just push assigned expression
        else {
            //Push assigned expression
            self.expr(*expr.assignment);
        }

        //If id is a local
        if let Some(index) = id_index {
            //Push local slot
            self.push_local(ops::OP_SET_LOCAL, ops::OP_SET_LOCAL_LONG, index, location);
        }
        //If id is a local
        else if let Some(index) = upval_index {
            //Push upval
            self.push_local(ops::OP_SET_UPVAL, ops::OP_SET_UPVAL_LONG, index, location);
        }
        //If it's a global or undefined
        else {
            //Push assignment operator
            self.push_const(ops::OP_SET_GLOBAL, ops::OP_SET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
        }
    }

    fn expr_call(&mut self, expr: expr::Call) {
        let mut current_loc = expr.callable.location();

        //Write operand
        self.expr(*expr.callable);

        //Write calls
        for call in expr.calls {
            if let Some(first_arg) = &call.first() {
                current_loc = first_arg.location();
            }

            let call_len = call.len() as u8;

            //Write args
            for arg in call {
                self.expr(arg);
            }

            //Write call operator
            self.target.chunk.write(ops::OP_CALL, current_loc.line);

            //Write call length
            self.target.chunk.write(call_len, current_loc.line);
        }
    }
}

//Statements
impl<'a> Compiler<'a> {
    fn stmt_block(&mut self, block: stmt::Block, end_loc: Location) {
        let initial_scope_depth = self.locals.len();

        //Push local scope
        self.locals.push(Vec::new());

        //Compile each statement in block
        for stmt in block.statements{
            self.stmt(stmt);
        }

        //Pop local scope
        while !self.locals.is_empty() && self.locals.len() > initial_scope_depth {
            let len = self.locals.last().unwrap().len();

            if len < u8::MAX as usize {
                self.target.chunk.write(ops::OP_POP_N, end_loc.line);
                self.target.chunk.write(len as u8, end_loc.line);
            }
            else {
                self.target.chunk.write(ops::OP_POP_N_LONG, end_loc.line);
                self.target.chunk.write_u16(len as u16, end_loc.line);
            }
            
            self.locals.pop();
        }
    }

    fn stmt_expression(&mut self, stmt: stmt::Expression, end_loc: Location) {
        //Compile expression
        self.expr(*stmt.expression);

        //Push pop
        self.target.chunk.write(ops::OP_POP, end_loc.line);
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

            //If nothing on local scope stack (slot 0 is reserved), this is global
            if self.locals.len() <= 1 {
                //Add id to constants table and get its index
                let id_value = Value::Object(Rc::new(Object::String(content)));
                self.push_const(ops::OP_DEF_GLOBAL, ops::OP_DEF_GLOBAL_LONG, id_value, loc);
            }
            //Otherwise, it is local
            else {
                //Value is on top of the stack, push local to current scope in compiler
                self.locals.last_mut().unwrap().push(content);
            }
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Invalid identifier '{}'.", 
                id_token_type.as_string()), loc));
        }
    }

    fn stmt_conditional(&mut self, stmt: stmt::Conditional, loc: Location) {
        let mut patch_locations: Vec<usize> = Vec::new();

        for branch in stmt.branches {
            //Compile condition
            self.expr(branch.condition);

            //If false, jump to next condition
            let patch_location = self.push_jump(ops::OP_JUMP_IF_FALSE, loc);

            //If true, pop condition
            self.target.chunk.write(ops::OP_POP, loc.line);

            //Compile statement
            self.stmt(branch.body);

            //Push unconditional jump to go to the end of the conditional after the body
            patch_locations.push(self.push_jump(ops::OP_JUMP, loc));

            //Patch jump addr after condition to jump here
            self.patch_jump(patch_location, loc);

            //If false, pop condition here
            self.target.chunk.write(ops::OP_POP, loc.line);
        }

        if let Some(else_block) = stmt.branch_else {
            //Compile statement
            self.stmt(*else_block);
        }

        //For the end of each branch, jump here
        for patch_at in patch_locations {
            self.patch_jump(patch_at, loc);
        }
    }

    fn stmt_loop(&mut self, stmt: stmt::Loop, loc: Location) {
        let loop_start = self.target.chunk.count();

        //Push true as condition, and jump to end if false, to allow support for break statement
        self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, Value::Bool(true), loc);

        let after_condition = self.target.chunk.count();

        //Push to stack for break/continue
        self.controllables.push(Controllable {
            ctype: ControllableType::Loop,
            depth: self.locals.len(),
            patch_start: loop_start,
            patch_end: after_condition
        });

        //If false, jump to end
        let patch_loc = self.push_jump(ops::OP_JUMP_IF_FALSE, loc);

        //If true, pop condition result here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Compile body
        self.stmt(*stmt.body);

        //Loop back to start
        self.push_loop(loop_start, loc);

        //Patch exit location
        self.patch_jump(patch_loc, loc);

        //If false, pop condition result here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Pop from stack for break/continue
        self.controllables.pop();
    }

    fn stmt_while(&mut self, stmt: stmt::While, loc: Location) {
        let loop_start = self.target.chunk.count();

        //Compile expression
        self.expr(*stmt.condition);

        let after_condition = self.target.chunk.count();

        //Push to stack for break/continue
        self.controllables.push(Controllable {
            ctype: ControllableType::Loop,
            depth: self.locals.len(),
            patch_start: loop_start,
            patch_end: after_condition
        });

        //If false, jump to end
        let patch_loc = self.push_jump(ops::OP_JUMP_IF_FALSE, loc);

        //If true, pop condition result here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Compile body
        self.stmt(*stmt.body);

        //Loop back to start
        self.push_loop(loop_start, loc);

        //Patch exit location
        self.patch_jump(patch_loc, loc);

        //If false, pop condition result here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Pop from stack for break/continue
        self.controllables.pop();
    }

    fn stmt_for(&mut self, stmt: stmt::For, loc: Location, end_loc: Location) {
        let initial_scope_depth = self.locals.len();

        //Push local scope
        self.locals.push(Vec::new());

        //Compile declaration
        let maybe_decl_patch = if let Some(dec) = stmt.declaration {
            self.stmt(*dec);

            if stmt.action.is_some() {
                //Jump to after action
                Some(self.push_jump(ops::OP_JUMP, loc))
            }
            else {
                None
            }
        }
        else {
            None
        };

        //Get loop location
        let loop_start = self.target.chunk.count();

        let has_action = stmt.action.is_some();

        //Compile action
        if let Some(act) = stmt.action {
            self.expr(*act);

            //Pop action value from stack
            self.target.chunk.write(ops::OP_POP, loc.line);
        }

        //Jump here after declaration to prevent execution of action before first time
        if let Some(decl_patch) = maybe_decl_patch {
            if has_action {
                self.patch_jump(decl_patch, loc);
            }
        }

        //Compile condition
        if let Some(cond) = stmt.condition {
            self.expr(*cond);
        }
        else {
            //If no condition, push true as condition to allow for breaking w/o patching locations
            self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, Value::Bool(true), loc);
        }

        let after_condition = self.target.chunk.count();

        //Push to stack for break/continue
        self.controllables.push(Controllable {
            ctype: ControllableType::Loop,
            depth: self.locals.len(),
            patch_start: loop_start,
            patch_end: after_condition
        });

        let patch_loc = self.push_jump(ops::OP_JUMP_IF_FALSE, loc);

        //If true, pop condition here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Compile body
        self.stmt(*stmt.body);

        //Loop back to start
        self.push_loop(loop_start, loc);

        //Patch exit location
        self.patch_jump(patch_loc, loc);

        //If false, pop condition result here
        self.target.chunk.write(ops::OP_POP, loc.line);

        //Pop from stack for break/continue
        self.controllables.pop();

        //Pop local scope
        while !self.locals.is_empty() && self.locals.len() > initial_scope_depth {
            let len = self.locals.last().unwrap().len();

            if len < u8::MAX as usize {
                self.target.chunk.write(ops::OP_POP_N, end_loc.line);
                self.target.chunk.write(len as u8, end_loc.line);
            }
            else {
                self.target.chunk.write(ops::OP_POP_N_LONG, end_loc.line);
                self.target.chunk.write_u16(len as u16, end_loc.line);
            }
            
            self.locals.pop();
        }
    }

    fn stmt_keyword(&mut self, stmt: stmt::Keyword, loc: Location) {
        let token_type = stmt.keyword.token_type();

        match token_type {
            TokenType::Continue | TokenType::Break => {
                let mut found: Option<Controllable> = None;

                for i in (0..self.controllables.len()).rev() {
                    let ctrl_top = self.controllables[i];

                    if let ControllableType::Loop = ctrl_top.ctype {
                        found = Some(ctrl_top);
                        break;
                    }
                }

                if found.is_none() {
                    self.errors.push(GreyscaleError::CompileErr(format!("Token {} is not valid here.", token_type.as_string()), loc));
                    return;
                }

                let found = found.unwrap();

                //On continue or break, pop local scope
                while !self.locals.is_empty() && self.locals.len() > found.depth {
                    let len = self.locals.last().unwrap().len();

                    if len < u8::MAX as usize {
                        self.target.chunk.write(ops::OP_POP_N, loc.line);
                        self.target.chunk.write(len as u8, loc.line);
                    }
                    else {
                        self.target.chunk.write(ops::OP_POP_N_LONG, loc.line);
                        self.target.chunk.write_u16(len as u16, loc.line);
                    }
                    
                    self.locals.pop();
                }

                //On continue, jump to before condition
                if let TokenType::Continue = token_type {
                    self.push_loop(found.patch_start, loc);
                }
                //On break, push false to stack and jump to after condition
                else {
                    self.push_const(ops::OP_CONSTANT, ops::OP_CONSTANT_LONG, Value::Bool(false), loc);
                    self.push_loop(found.patch_end, loc);
                }
            },
            _ => {
                self.errors.push(GreyscaleError::CompileErr("Invalid keyword token.".to_string(), loc));
            }
        }
    }

    fn stmt_return(&mut self, stmt: stmt::Return, loc: Location) {
        if self.target.func_type.is_top_level() {
            self.errors.push(GreyscaleError::CompileErr(format!("Token {} is not valid here.", TokenType::Return.as_string()), loc));
            return;
        }

        //If returning an expression, push it, otherwise void
        if let Some(expr) = stmt.expression {
            self.expr(*expr);
        }
        else {
            self.expr_literal(Literal {
                value: LiteralType::Void
            }, loc);
        }

        //Push return keyword
        self.target.chunk.write(ops::OP_RETURN, loc.line);
    }
}