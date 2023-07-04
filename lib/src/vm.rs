use crate::{chunk::{Chunk, disassemble::FormattableInstr}, ops::Op, value::Value, constants};

use self::error::GreyscaleError;

pub mod error;

type GreyscaleResult = std::result::Result<(), GreyscaleError>;

#[derive(Default, Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>
}

impl VirtualMachine {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::new()
        }
    }

    pub fn move_next(&mut self) -> Option<u8> {
        if self.ip >= self.chunk.count() {
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
                //Constant declarations
                Op::Constant => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?;
                        self.push_value(*value)?;
                    }
                    else {
                        return Err(GreyscaleError::RuntimeErr("Expected the address of a constant.".to_string()));
                    }
                },
                Op::ConstantLong => {
                    if let Some(addr1) = self.move_next() {
                        if let Some(addr2) = self.move_next() {
                            let addr = (addr2 as u16) + ((addr1 as u16) << 8);
                            let value = self.read_const(addr as usize)?;
                            self.push_value(*value)?;
                        }
                        else {
                            return Err(GreyscaleError::RuntimeErr("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(GreyscaleError::RuntimeErr("Expected the address of a constant.".to_string()));
                    }
                },

                //Keywords
                Op::Return => {
                    println!("{}", self.pop_value().unwrap_or_default());
                    return Ok(());
                },

                //Unary operators
                Op::Negate => self.unary_op(&instr)?,

                //Binary operators
                Op::Add => self.binary_op(&instr)?,
                Op::Subtract => self.binary_op(&instr)?,
                Op::Multiply => self.binary_op(&instr)?,
                Op::Divide => self.binary_op(&instr)?,
                Op::Modulus => self.binary_op(&instr)?,

                //Other
                Op::Unknown(n) => {
                    return Err(GreyscaleError::RuntimeErr(format!("Invalid instruction '{n}'.")));
                },
            };
        }

        Ok(())
    }

    fn read_const(&self, n: usize) -> Result<&Value, GreyscaleError> {
        if let Some(constant) = self.chunk.try_get_const(n) {
            Ok(constant)
        }
        else {
            Err(GreyscaleError::RuntimeErr(format!("Failed to find constant at '{n}'.")))
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
                        }
                    }
                }
                else {
                    Err(GreyscaleError::RuntimeErr("Expected an argument.".to_string()))
                }
            },
            _ => Err(GreyscaleError::RuntimeErr(format!("Invalid unary operator {op}")))
        }
    }

    fn binary_op(&mut self, op: &Op) -> GreyscaleResult {

        let mut next_arg = || {
            if let Some(val) = self.pop_value() {
                Ok(val)
            }
            else {
                Err(GreyscaleError::RuntimeErr("Expected an argument.".to_string()))
            }
        };

        if matches!(op, Op::Add | Op::Subtract | Op::Multiply | Op::Divide | Op::Modulus) {
            let val_a = next_arg()?;
            let val_b = next_arg()?;

            match val_a {
                Value::Double(a) => {
                    match val_b {
                        Value::Double(b) => {
                            match op {
                                Op::Add => self.push_value(Value::Double(a + b))?,
                                Op::Subtract => self.push_value(Value::Double(a - b))?,
                                Op::Multiply => self.push_value(Value::Double(a * b))?,
                                Op::Divide => self.push_value(Value::Double(a / b))?,
                                Op::Modulus => self.push_value(Value::Double(a % b))?,

                                //Will never be reached
                                _ => {}
                            }
                        },
                    }
                }
            }

            Ok(())
        }
        else {
            Err(GreyscaleError::RuntimeErr(format!("Invalid binary operator {op}")))
        }
    }

    fn push_value(&mut self, value: Value) -> GreyscaleResult {
        if self.stack.len() == constants::MAX_STACK {
            return Err(GreyscaleError::RuntimeErr("Stack overflow".to_string()));
        }

        self.stack.push(value);

        Ok(())
    }

    fn pop_value(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    // fn peek_value(&self) -> Option<&Value> {
    //     self.stack.last()
    // }

    // fn reset_stack(&mut self) {
    //     self.stack.clear();
    // }

    fn trace(&self) {
        if constants::TRACE {
            print!("\nINSTR: {}", FormattableInstr::new(&self.chunk, self.ip - 1));
            print!("STACK: ");
            self.stack_trace();
            println!();
        }
    }

    fn stack_trace(&self) {
        if constants::TRACE && !&self.stack.is_empty() {
            for value in &self.stack {
                print!("[ {value} ]");
            }
            println!();
        }
    }

}