use std::rc::Rc;

use crate::parser::ast;
use crate::chunk;
use crate::token::token_type::TokenType;
use crate::value::Values;
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
    chunk: Chunk,
    constants: Values,
    locals: Vec<Vec<String>>
}

impl<'a> Compiler<'a> {
    pub fn compile_ast(program: Rc<Vec<&'a str>>, ast: AST) -> Result<Chunk, GreyscaleError> {
        let mut compiler = Self {
            program: Rc::clone(&program),
            errors: Vec::new(),
            chunk: Chunk::default(),
            constants: Values::default(),
            locals: Vec::new()
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
            chunk: Chunk::default(),
            constants: Values::default(),
            locals: Vec::new()
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
            StmtNode::Block(block, _, end_loc) => {
                self.stmt_block(block, end_loc);
            },
            StmtNode::Conditional(conditional, loc, _) => {
                self.stmt_conditional(conditional, loc);
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
            StmtNode::For(stmt, loc, end_loc) => {
                self.stmt_for(stmt, loc, end_loc);
            },
            StmtNode::While(stmt, loc, _) => {
                self.stmt_while(stmt, loc);
            },
            StmtNode::Loop(stmt, loc, _) => {
                self.stmt_loop(stmt, loc);
            },
            StmtNode::Return(_, loc, _) => {
                self.errors.push(GreyscaleError::CompileErr("Return statement compilation not yet implemented.".to_string(), loc));
            },
        }
    }

    fn push_const(&mut self, code: u8, code_long: u8, value: Value, location: Location) -> usize {
        //If constant exists, get its index
        let index = if let Some(i) = self.constants.index_of(&value) {
            i
        }
        //Otherwise, if there is space for a new constant, add it to the chunk, and the compiler's collection of constants
        else if self.chunk.count_consts() < u16::MAX as usize  {
            self.constants.write(value.clone());
            self.chunk.add_const(value)
        }
        //Otherwise, out of range
        else {
            u16::MAX as usize
        };

        if index < u8::MAX as usize {
            self.chunk.write(code, location.line);
            self.chunk.write(index as u8, location.line);
        }
        else if index < u16::MAX as usize {
            self.chunk.write(code_long, location.line);
            self.chunk.write_u16(index as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} constants.", u16::MAX), location));
        }

        index
    }

    fn push_local(&mut self, code: u8, code_long: u8, slot: usize, location: Location) {
        if slot < u8::MAX as usize {
            self.chunk.write(code, location.line);
            self.chunk.write(slot as u8, location.line);
        }
        else if slot < u16::MAX as usize {
            self.chunk.write(code_long, location.line);
            self.chunk.write_u16(slot as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot exceed {} locals.", u16::MAX), location));
        }
    }

    fn push_jump(&mut self, code: u8, location: Location) -> usize {
        self.chunk.write(code, location.line);
        //Get the address we're going to write the offset to so we can patch it later
        let count = self.chunk.count();
        self.chunk.write_u16(u16::MAX, location.line);
        count
    }

    fn patch_jump(&mut self, offset: usize, location: Location) {
        let jump_to = self.chunk.count().saturating_sub(offset + 2);

        if jump_to <= u16::MAX as usize {
            self.chunk.patch_u16(offset, jump_to as u16);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot jump over {} lines.", u16::MAX), location));
        }
    }

    fn push_loop(&mut self, loop_start: usize, location: Location) {
        self.chunk.write(ops::OP_LOOP, location.line);

        let offset = (self.chunk.count() + 2).saturating_sub(loop_start);

        if offset <= u16::MAX as usize {
            self.chunk.write_u16(offset as u16, location.line);
        }
        else {
            self.errors.push(GreyscaleError::CompileErr(format!("Cannot jump over {} lines.", u16::MAX), location));
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for i in (0..self.locals.len()).rev() {
            let scope = &self.locals[i];

            for j in (0..scope.len()).rev() {
                let elem = &scope[j];

                if elem == name {
                    let mut position = scope.len() - j - 1;

                    for x in 0..i {
                        position += self.locals[x].len();
                    }

                    return Some(position);
                }
            }
        }

        None
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
                TokenType::Plus => self.chunk.write(ops::OP_ADD, location.line),
                TokenType::Minus => self.chunk.write(ops::OP_SUBTRACT, location.line),
                TokenType::Star => self.chunk.write(ops::OP_MULTIPLY, location.line),
                TokenType::Slash => self.chunk.write(ops::OP_DIVIDE, location.line),
                TokenType::Percent => self.chunk.write(ops::OP_MODULUS, location.line),
                TokenType::CaretCaret => self.chunk.write(ops::OP_LOGICAL_XOR, location.line),
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

    fn short_circuit(&mut self, short_circuit_op: u8, location: Location) -> usize {
        let short_circuit_loc = self.push_jump(short_circuit_op, location);

        //If didn't short circuit, pop value from stack so next part can be evaluated
        self.chunk.write(ops::OP_POP, location.line);

        //Return patch location
        short_circuit_loc
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

            //If id is a local
            if let Some(index) = self.resolve_local(&content) {
                self.push_local(ops::OP_GET_LOCAL, ops::OP_GET_LOCAL_LONG, index, location);
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

        let id_index = self.resolve_local(&id);

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
            //If it's a global or undefined
            else {
                //Push identifier
                self.push_const(ops::OP_GET_GLOBAL, ops::OP_GET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
            }

            //Push assigned expression
            self.expr(*expr.assignment);

            //Push infix op
            self.chunk.write(infix_op, location.line);
        }
        //Handle short circuting operators
        else if matches!(assignment_token_type, TokenType::AmpAmpEqual | TokenType::PipePipeEqual) {

            //LHS
            //If id is a local
            if let Some(index) = id_index {
                //Push local slot
                self.push_local(ops::OP_GET_LOCAL, ops::OP_GET_LOCAL_LONG, index, location);
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
        //If it's a global or undefined
        else {
            //Push assignment operator
            self.push_const(ops::OP_SET_GLOBAL, ops::OP_SET_GLOBAL_LONG, Value::Object(Rc::clone(&id_rc)), location);
        }
    }
}

//Statements
impl<'a> Compiler<'a> {
    fn stmt_block(&mut self, block: stmt::Block, end_loc: Location) {
        //Push local scope
        self.locals.push(Vec::new());

        //Compile each statement in block
        for stmt in block.statements{
            self.stmt(stmt);
        }

        //Pop local scope
        let len = self.locals.last().unwrap().len();

        if len < u8::MAX as usize {
            self.chunk.write(ops::OP_POP_N, end_loc.line);
            self.chunk.write(len as u8, end_loc.line);
        }
        else {
            self.chunk.write(ops::OP_POP_N_LONG, end_loc.line);
            self.chunk.write_u16(len as u16, end_loc.line);
        }
        
        self.locals.pop();
    }

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

            //If nothing on local scope stack, this is global
            if self.locals.is_empty() {
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
            self.chunk.write(ops::OP_POP, loc.line);

            //Compile statement
            self.stmt(branch.body);

            //Push unconditional jump to go to the end of the conditional after the body
            patch_locations.push(self.push_jump(ops::OP_JUMP, loc));

            //Patch jump addr after condition to jump here
            self.patch_jump(patch_location, loc);

            //If false, pop condition here
            self.chunk.write(ops::OP_POP, loc.line);
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
        let loop_start = self.chunk.count();

        //Compile body
        self.stmt(*stmt.body);

        //Loop back to start
        self.push_loop(loop_start, loc);
    }

    fn stmt_while(&mut self, stmt: stmt::While, loc: Location) {
        let loop_start = self.chunk.count();

        //Compile expression
        self.expr(*stmt.condition);

        //If false, jump to end
        let patch_loc = self.push_jump(ops::OP_JUMP_IF_FALSE, loc);

        //If true, pop condition result here
        self.chunk.write(ops::OP_POP, loc.line);

        //Compile body
        self.stmt(*stmt.body);

        //Loop back to start
        self.push_loop(loop_start, loc);

        //Patch exit location
        self.patch_jump(patch_loc, loc);

        //If false, pop condition result here
        self.chunk.write(ops::OP_POP, loc.line);
    }

    fn stmt_for(&mut self, stmt: stmt::For, loc: Location, end_loc: Location) {
        //Push local scope
        self.locals.push(Vec::new());

        //Compile declaration
        if let Some(dec) = stmt.declaration {
            self.stmt(*dec);
        }

        //Get loop location
        let loop_start = self.chunk.count();

        //Compile condition
        let maybe_patch_loc = if let Some(cond) = stmt.condition {
            self.expr(*cond);
            let patch_loc = Some(self.push_jump(ops::OP_JUMP_IF_FALSE, loc));

            //If true, pop condition here
            self.chunk.write(ops::OP_POP, loc.line);
            patch_loc
        }
        else {
            None
        };

        //Compile body
        self.stmt(*stmt.body);

        //Compile action
        if let Some(act) = stmt.action {
            self.expr(*act);

            //Pop action value from stack
            self.chunk.write(ops::OP_POP, loc.line);
        }

        //Loop back to start
        self.push_loop(loop_start, loc);

        //Patch exit location
        if let Some(patch_loc) = maybe_patch_loc {
            self.patch_jump(patch_loc, loc);

            //If false, pop condition result here
            self.chunk.write(ops::OP_POP, loc.line);
        }

        //Pop local scope
        let len = self.locals.last().unwrap().len();

        if len < u8::MAX as usize {
            self.chunk.write(ops::OP_POP_N, end_loc.line);
            self.chunk.write(len as u8, end_loc.line);
        }
        else {
            self.chunk.write(ops::OP_POP_N_LONG, end_loc.line);
            self.chunk.write_u16(len as u16, end_loc.line);
        }
        
        self.locals.pop();
    }
}