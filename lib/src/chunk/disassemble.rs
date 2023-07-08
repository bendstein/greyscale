use std::fmt::{Write, Display};

use crate::ops::Op;

use super::Chunk;

type Result = std::result::Result<usize, std::fmt::Error>;

impl Chunk {

    pub fn disassemble(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("== {} ==\n", self.name.as_ref().unwrap_or(&String::default())))?;
        
        let mut offset = 0_usize;
        
        while offset < self.code.len() {
            offset = self.disassemble_instr(offset, f)?;
        }
        
        Ok(())
    }
    
    fn disassemble_instr(&self, offset: usize, f: &mut std::fmt::Formatter<'_>) -> Result {
        f.write_fmt(format_args!("{:04X?}  ", offset))?;
        
        let line_count = self.metadata.line_count();

        let line_pad = if line_count == 0 {
            1
        }
        else {
            f32::floor(f32::log10(line_count as f32)) as usize + 1
        };

        let prev_line = if offset == 0 {
            0
        } else {
            self.metadata.get_line(offset - 1)
        };

        let curr_line = self.metadata.get_line(offset);

        if offset > 0 && prev_line == curr_line {
            f.write_fmt(format_args!("{: >line_pad$}  ", "|"))?;
        }
        else {
            f.write_fmt(format_args!("{:0>line_pad$}  ", curr_line))?;
        }

        let instr = self[offset];
        let op = Op::from(instr);

        match op {
            //Constant declarations -------------------------------------------
            Op::Constant => self.disassemble_instr_const(offset, f),
            Op::ConstantLong => self.disassemble_instr_const_long(offset, f),


            //Keywords --------------------------------------------------------
            Op::Return => self.disassemble_instr_simple(op, offset, f),


            //Unary operators -------------------------------------------------
            //Arithmetic
            Op::Negate => self.disassemble_instr_simple(op, offset, f),
            //Logical
            Op::LogicalNot => self.disassemble_instr_simple(op, offset, f),
            //Bitwise
            Op::BitwiseNot => self.disassemble_instr_simple(op, offset, f),


            //Binary operators ------------------------------------------------
            //Arithmetic
            Op::Add => self.disassemble_instr_simple(op, offset, f),
            Op::Subtract => self.disassemble_instr_simple(op, offset, f),
            Op::Multiply => self.disassemble_instr_simple(op, offset, f),
            Op::Divide => self.disassemble_instr_simple(op, offset, f),
            Op::Modulus => self.disassemble_instr_simple(op, offset, f),
            //Logical
            Op::LogicalAnd => self.disassemble_instr_simple(op, offset, f),
            Op::LogicalOr => self.disassemble_instr_simple(op, offset, f),
            Op::LogicalXor => self.disassemble_instr_simple(op, offset, f),
            //Bitwise
            Op::BitwiseAnd => self.disassemble_instr_simple(op, offset, f),
            Op::BitwiseOr => self.disassemble_instr_simple(op, offset, f),
            Op::BitwiseXor => self.disassemble_instr_simple(op, offset, f),
            Op::BitwiseLShift => self.disassemble_instr_simple(op, offset, f),
            Op::BitwiseRShift => self.disassemble_instr_simple(op, offset, f),
            //Comparison
            Op::Equal => self.disassemble_instr_simple(op, offset, f),
            Op::NotEqual => self.disassemble_instr_simple(op, offset, f),
            Op::Greater => self.disassemble_instr_simple(op, offset, f),
            Op::Less => self.disassemble_instr_simple(op, offset, f),
            Op::GreaterEqual => self.disassemble_instr_simple(op, offset, f),
            Op::LessEqual => self.disassemble_instr_simple(op, offset, f),
            //Internal
            Op::Concat => self.disassemble_instr_simple(op, offset, f),

            //Other -----------------------------------------------------------
            Op::Unknown(_) => self.disassemble_instr_unknown(instr, offset, f)
        }
    }
    
    fn disassemble_instr_simple(&self, op: Op, offset: usize, f: &mut std::fmt::Formatter<'_>) -> Result {
        f.write_fmt(format_args!("{}\n", op.name_padded()))?;
        Ok(offset + 1)
    }
    
    fn disassemble_instr_const(&self, offset: usize, f: &mut std::fmt::Formatter<'_>) -> Result {
        f.write_fmt(format_args!("{}  {:04X?}  ", Op::Constant.name_padded(), self[offset + 1]))?;
        self.write_value(self[offset + 1] as usize, f)?;
        f.write_char('\n')?;
        Ok(offset + 2)
    }

    fn disassemble_instr_const_long(&self, offset: usize, f: &mut std::fmt::Formatter<'_>) -> Result {
        //Combine the next 2 arguments to get the full index
        let a1 = self[offset + 1];
        let a2 = self[offset + 2];

        let combined = ((a1 as u16) << 8) + (a2 as u16);

        f.write_fmt(format_args!("{}  {:04X?}  ", Op::ConstantLong.name_padded(), combined))?;

        self.write_value(combined as usize, f)?;
        f.write_char('\n')?;
        Ok(offset + 3)
    }

    fn write_value(&self, n: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        if n >= self.constants.count() {
            f.write_fmt(format_args!("<undefined>"))?;
        }
        else {
            f.write_fmt(format_args!("{}", self.constants[n]))?;
        }

        Ok(())
    }

    fn disassemble_instr_unknown(&self, instr: u8, offset: usize, f: &mut std::fmt::Formatter<'_>) -> Result {
        f.write_fmt(format_args!("{}\n", Op::Unknown(instr).name_padded()))?;
        Ok(offset + 1)
    }
    
}

pub struct FormattableInstr<'a> {
    chunk: &'a Chunk,
    offset: usize
}

impl<'a> FormattableInstr<'a> {
    pub fn new(chunk: &'a Chunk, offset: usize) -> Self {
        Self {
            chunk,
            offset
        }
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.offset = offset;
        self
    }
}

impl<'a> Display for FormattableInstr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.chunk.disassemble_instr(self.offset, f)?;
        Ok(())
    }
}