use std::fmt::format;

use crate::{chunk::{Chunk, disassemble::FormattableInstr}, ops::Op, value::Value};

use self::error::GreyscaleError;

pub mod error;

type GreyscaleResult = std::result::Result<(), GreyscaleError>;

#[derive(Default, Debug)]
pub struct VirtualMachine {
    chunk: Chunk,
    ip: usize
}

impl VirtualMachine {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0
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

    pub fn run(&mut self) -> GreyscaleResult {
        
        while let Some(opcode) = self.move_next() {

            let instr = Op::from(opcode);

            match instr {
                Op::Return => {},
                Op::Constant => {},
                Op::ConstantLong => {},
                Op::Unknown(_) => { println!() },
            }

            self.trace();

            match instr {
                Op::Return => {
                    return Ok(());
                },
                Op::Constant => {
                    if let Some(addr) = self.move_next() {
                        let value = self.read_const(addr as usize)?;
                        println!("{value}");
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
                            println!("{value}");
                        }
                        else {
                            return Err(GreyscaleError::RuntimeErr("Expected the 16-bit address of a constant.".to_string()));
                        }
                    }
                    else {
                        return Err(GreyscaleError::RuntimeErr("Expected the address of a constant.".to_string()));
                    }
                },
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

    fn trace(&self) {
        if crate::TRACE {
            println!("{}", FormattableInstr::new(&self.chunk, self.ip - 1));
        }
    }

}