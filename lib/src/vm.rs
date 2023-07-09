use std::{rc::Rc, collections::HashMap};

use crate::{chunk::{Chunk, disassemble::FormattableInstr}, ops::Op, value::{Value, object::Object}, constants, location::Location};

use self::{error::GreyscaleError, settings::VMSettings};

pub mod error;
pub mod settings;

type GreyscaleResult = std::result::Result<(), GreyscaleError>;

#[derive(Default, Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    settings: VMSettings
}

impl VirtualMachine {
    pub fn new(chunk: Chunk) -> Self {
        Self::new_with_settings(chunk, VMSettings::default())
    }

    pub fn new_with_settings(chunk: Chunk, settings: VMSettings) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            settings
        }
    }

    pub fn move_next(&mut self) -> Option<u8> {
        if self.is_at_end() {
            None
        }
        else {
            self.ip += 1;
            Some(self.chunk[self.ip - 1])
        }
    }

    pub fn execute(&mut self) -> GreyscaleResult {
        
        while let Some(opcode) = self.move_next() {

            let instr = Op::from(opcode);

            self.trace();

            match instr {
                //Declarations And Variables ------------------------------------------------------------
                Op::Constant => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?.clone();
                        self.push_value(value)?;
                    }
                    else {           
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::ConstantLong => {
                    if let Some(addr1) = self.move_next() {
                        if let Some(addr2) = self.move_next() {
                            let addr = (addr2 as u16) + ((addr1 as u16) << 8);
                            let value = self.read_const(addr as usize)?.clone();
                            self.push_value(value)?;
                        }
                        else {
                            return Err(self.make_error("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::DefineGlobal => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?.clone();
                        
                        if !value.is_object_string() {
                            return Err(self.make_error("Expected the name of an identifier.".to_string()));
                        }

                        let id = value.unwrap_object_string();

                        let assign = self.pop_value()
                            .ok_or_else(|| self.make_error(format!("Expected a value to assign to {id}.")))?;

                        self.globals.insert(id, assign);
                    }
                    else {           
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::DefineGlobalLong => {
                    if let Some(addr1) = self.move_next() {
                        if let Some(addr2) = self.move_next() {
                            let addr = (addr2 as u16) + ((addr1 as u16) << 8);
                            let value = self.read_const(addr as usize)?.clone();
                            
                            if !value.is_object_string() {
                                return Err(self.make_error("Expected the name of an identifier.".to_string()));
                            }

                            let id = value.unwrap_object_string();

                            let assign = self.pop_value()
                                .ok_or_else(|| self.make_error(format!("Expected a value to assign to {id}.")))?;
    
                            self.globals.insert(id, assign);
                        }
                        else {
                            return Err(self.make_error("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::GetGlobal => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?.clone();
                        
                        if !value.is_object_string() {
                            return Err(self.make_error("Expected the name of an identifier.".to_string()));
                        }

                        let id = value.unwrap_object_string();

                        if let Some(global) = self.globals.get(&id) {
                            self.push_value(global.clone())?;
                        }
                        else {
                            return Err(self.make_error(format!("Undefined global {id}.")));
                        }
                    }
                    else {           
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::GetGlobalLong => {
                    if let Some(addr1) = self.move_next() {
                        if let Some(addr2) = self.move_next() {
                            let addr = (addr2 as u16) + ((addr1 as u16) << 8);
                            let value = self.read_const(addr as usize)?.clone();
                            
                            if !value.is_object_string() {
                                return Err(self.make_error("Expected the name of an identifier.".to_string()));
                            }

                            let id = value.unwrap_object_string();

                            if let Some(global) = self.globals.get(&id) {
                                self.push_value(global.clone())?;
                            }
                            else {
                                return Err(self.make_error(format!("Undefined global {id}.")));
                            }
                        }
                        else {
                            return Err(self.make_error("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::SetGlobal => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?.clone();
                        
                        if !value.is_object_string() {
                            return Err(self.make_error("Expected the name of an identifier.".to_string()));
                        }

                        let id = value.unwrap_object_string();

                        if !self.globals.contains_key(&id) {
                            return Err(self.make_error(format!("Cannot assign value to undefined global {id}.")));
                        }

                        //Assignment is an expression that returns the assigned value, so don't pop from stack
                        let assign = self.peek_value()
                            .ok_or_else(|| self.make_error(format!("Expected a value to assign to {id}.")))?;

                        self.globals.insert(id, assign.clone());

                    }
                    else {           
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },
                Op::SetGlobalLong => {
                    if let Some(addr1) = self.move_next() {
                        if let Some(addr2) = self.move_next() {
                            let addr = (addr2 as u16) + ((addr1 as u16) << 8);
                            let value = self.read_const(addr as usize)?.clone();
                            
                            if !value.is_object_string() {
                                return Err(self.make_error("Expected the name of an identifier.".to_string()));
                            }

                            let id = value.unwrap_object_string();

                            if !self.globals.contains_key(&id) {
                                return Err(self.make_error(format!("Cannot assign value to undefined global {id}.")));
                            }

                            //Assignment is an expression that returns the assigned value, so don't pop from stack
                            let assign = self.peek_value()
                                .ok_or_else(|| self.make_error(format!("Expected a value to assign to {id}.")))?;
    
                            self.globals.insert(id, assign.clone());
                        }
                        else {
                            return Err(self.make_error("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(self.make_error("Expected the address of a constant.".to_string()));
                    }
                },

                //Keywords  ------------------------------------------------------------------------
                Op::Return => {
                    //println!("{}", self.pop_value().unwrap_or_default());
                    return Ok(());
                },
                Op::Print => {
                    if constants::TRACE {
                        println!("\n------VM Output------\n");
                    }

                    println!("{}", self.pop_value().unwrap_or_default());

                    if constants::TRACE {
                        println!("\n----End VM Output----\n");
                    }
                },
                //Internal
                Op::Pop => {
                    //(For REPL) If pop is the last instruction, and ignore final pop is enabled, don't pop
                    if self.is_at_end() && self.settings.ignore_final_pop {

                    }
                    else {
                        let _ = self.pop_value();                        
                    }
                },

                //Unary operators  -----------------------------------------------------------------
                //Arithmetic
                Op::Negate => self.unary_op(&instr)?,
                //Logical
                Op::LogicalNot => self.unary_op(&instr)?,
                //Bitwise
                Op::BitwiseNot => self.unary_op(&instr)?,


                //Binary operators  ----------------------------------------------------------------
                //Arithmetic
                Op::Add => self.binary_op(&instr)?,
                Op::Subtract => self.binary_op(&instr)?,
                Op::Multiply => self.binary_op(&instr)?,
                Op::Divide => self.binary_op(&instr)?,
                Op::Modulus => self.binary_op(&instr)?,
                //Logical
                Op::LogicalAnd => self.binary_op(&instr)?,
                Op::LogicalOr => self.binary_op(&instr)?,
                Op::LogicalXor => self.binary_op(&instr)?,
                //Bitwise
                Op::BitwiseAnd => self.binary_op(&instr)?,
                Op::BitwiseOr => self.binary_op(&instr)?,
                Op::BitwiseXor => self.binary_op(&instr)?,
                Op::BitwiseLShift => self.binary_op(&instr)?,
                Op::BitwiseRShift => self.binary_op(&instr)?,
                //Comparison
                Op::Equal => self.binary_op(&instr)?,
                Op::NotEqual => self.binary_op(&instr)?,
                Op::Greater => self.binary_op(&instr)?,
                Op::Less => self.binary_op(&instr)?,
                Op::GreaterEqual => self.binary_op(&instr)?,
                Op::LessEqual => self.binary_op(&instr)?,
                //Internal
                Op::Concat => self.binary_op(&instr)?,

                //Other
                Op::Unknown(n) => {
                    return Err(self.make_error(format!("Invalid instruction '{n}'.")));
                },
            };

            if constants::TRACE {
                print!("STACK --> ");
                self.stack_trace();
                println!();
            }
        }

        Ok(())
    }

    pub fn peek_value(&self) -> Option<&Value> {
        self.stack.last()
    }

    fn read_const(&self, n: usize) -> Result<&Value, GreyscaleError> {
        if let Some(constant) = self.chunk.try_get_const(n) {
            Ok(constant)
        }
        else {
            Err(self.make_error(format!("Failed to find constant at '{n}'.")))
        }
    }

    fn unary_op(&mut self, op: &Op) -> GreyscaleResult {
        match op {
            Op::Negate => {
                if let Some(val) = self.pop_value() {
                    match val {
                        Value::Double(n) => {
                            self.push_value(Value::Double(-n))?;
                            Ok(()) 
                        },
                        Value::Int(n) => {
                            self.push_value(Value::Int(-n))?;
                            Ok(())
                        },
                        _ => Err(self.make_error(format!("Cannot apply unary operator {op} to argument.")))
                    }
                }
                else {
                    Err(self.make_error("Expected an argument.".to_string()))
                }
            },
            Op::LogicalNot => {
                if let Some(val) = self.pop_value() {
                    match val {
                        Value::Bool(b) => {
                            self.push_value(Value::Bool(!b))?;
                            Ok(()) 
                        },
                        _ => Err(self.make_error(format!("Cannot apply unary operator {op} to argument.")))
                    }
                }
                else {
                    Err(self.make_error("Expected an argument.".to_string()))
                }
            },
            Op::BitwiseNot => {
                if let Some(val) = self.pop_value() {
                    match val {
                        Value::Int(n) => {
                            self.push_value(Value::Int(!n))?;
                            Ok(()) 
                        },
                        _ => Err(self.make_error(format!("Cannot apply unary operator {op} to argument.")))
                    }
                }
                else {
                    Err(self.make_error("Expected an argument.".to_string()))
                }
            },
            _ => Err(self.make_error(format!("Invalid unary operator {op}")))
        }
    }

    fn binary_op(&mut self, op: &Op) -> GreyscaleResult {

        let mut next_arg = || {
            if let Some(val) = self.pop_value() {
                Ok(val)
            }
            else {
                Err(self.make_error("Expected an argument.".to_string()))
            }
        };

        let val_b = next_arg()?;
        let val_a = next_arg()?;

        match op {
            Op::Add => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a + b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double((a as f64) + b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Double(a + (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double(a + b))?;
                        return Ok(());
                    }
                }
                else if val_a.is_object_string() && val_b.is_object_string() {
                    let a_string = val_a.string();
                    let b_string = val_b.string();
                    let concat = format!("{a_string}{b_string}");
                    self.push_value(Value::Object(Rc::new(Object::String(concat))))?;
                    return Ok(());
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Subtract => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a - b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double((a as f64) - b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Double(a - (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double(a - b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Multiply => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a * b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double((a as f64) * b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Double(a * (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double(a * b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Divide => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a / b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double((a as f64) / b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Double(a / (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double(a / b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Modulus => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a % b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double((a as f64) % b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Double(a % (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Double(a % b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::LogicalAnd => {
                if let Value::Bool(a) = val_a {
                    //Short circuit
                    if !a {
                        self.push_value(Value::Bool(false))?;
                        return Ok(());
                    }

                    if let Value::Bool(b) = val_b {
                        self.push_value(Value::Bool(b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::LogicalOr => {
                if let Value::Bool(a) = val_a {
                    //Short circuit
                    if a {
                        self.push_value(Value::Bool(true))?;
                        return Ok(());
                    }

                    if let Value::Bool(b) = val_b {
                        self.push_value(Value::Bool(b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::LogicalXor => {
                if let Value::Bool(a) = val_a {
                    if let Value::Bool(b) = val_b {
                        self.push_value(Value::Bool(a ^ b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::BitwiseAnd => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a & b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::BitwiseOr => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a | b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::BitwiseXor => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a ^ b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::BitwiseLShift => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a << b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::BitwiseRShift => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Int(a >> b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Equal => {
                if Value::Null == val_a || Value::Null == val_b {
                    self.push_value(Value::Bool(Value::Null == val_a && Value::Null == val_b))?;
                    return Ok(());
                }
                else if Value::Void == val_a || Value::Void == val_b {
                    self.push_value(Value::Bool(false))?;
                    return Ok(());
                }
                else if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a == b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) == b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a == (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a == b))?;
                        return Ok(());
                    }
                }
                else if let Value::Bool(a) = val_a {
                    if let Value::Bool(b) = val_b {
                        self.push_value(Value::Bool(a == b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::NotEqual => {
                if Value::Null == val_a || Value::Null == val_b {
                    self.push_value(Value::Bool((Value::Null == val_a) ^ (Value::Null == val_b)))?;
                    return Ok(());
                }
                else if Value::Void == val_a || Value::Void == val_b {
                    self.push_value(Value::Bool(true))?;
                    return Ok(());
                }
                else if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a != b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) != b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a != (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a != b))?;
                        return Ok(());
                    }
                }
                else if let Value::Bool(a) = val_a {
                    if let Value::Bool(b) = val_b {
                        self.push_value(Value::Bool(a != b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Greater => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a > b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) > b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a > (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a > b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Less => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a < b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) < b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a < (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a < b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::GreaterEqual => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a >= b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) >= b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a >= (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a >= b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },            
            Op::LessEqual => {
                if let Value::Int(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a <= b))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool((a as f64) <= b))?;
                        return Ok(());
                    }
                }
                else if let Value::Double(a) = val_a {
                    if let Value::Int(b) = val_b {
                        self.push_value(Value::Bool(a <= (b as f64)))?;
                        return Ok(());
                    }
                    else if let Value::Double(b) = val_b {
                        self.push_value(Value::Bool(a <= b))?;
                        return Ok(());
                    }
                }

                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            Op::Concat => {
                //If either value is a string, concatenate them
                if val_a.is_object_string() || val_b.is_object_string() {
                    let a_string = val_a.string();
                    let b_string = val_b.string();
                    let concat = format!("{a_string}{b_string}");
                    self.push_value(Value::Object(Rc::new(Object::String(concat))))?;
                    return Ok(());
                }
                
                Err(self.make_error("Operation is not valid for given types.".to_string()))
            },
            _ => Err(self.make_error(format!("Invalid binary operator {op}")))
        }
    }

    fn push_value(&mut self, value: Value) -> GreyscaleResult {
        if self.stack.len() == constants::MAX_STACK {
            return Err(self.make_error("Stack overflow".to_string()));
        }

        self.stack.push(value);

        Ok(())
    }

    fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    fn is_at_end(&self) -> bool {
        self.ip >= self.chunk.count()
    }

    fn get_line(&self) -> usize {
        self.chunk.metadata.get_line(self.ip)
    }

    fn make_error(&self, message: String) -> GreyscaleError {
        GreyscaleError::RuntimeErr(message, Location {
            column: 0,
            line: self.get_line()
        })
    }

    // fn reset_stack(&mut self) {
    //     self.stack.clear();
    // }

    fn trace(&self) {
        if constants::TRACE {
            print!("\nINSTR: {}", FormattableInstr::new(&self.chunk, self.ip - 1));
            print!("STACK: ");
            self.stack_trace();
        }
    }

    pub fn stack_trace(&self) {
        if constants::TRACE {
            for value in &self.stack {
                print!("[ {value} ]");
            }
            println!();
        }
    }

}