use std::fmt::{Display};

use crate::ops::Op;

use super::Chunk;

type Result = std::result::Result<usize, std::fmt::Error>;

impl Chunk {

    pub fn disassemble(&self) -> String {
        let mut s = String::new();

        s.push_str(&format!("== {} ==\n", self.name.as_ref().unwrap_or(&String::default())));

        let mut offset = 0_usize;
        
        while offset < self.code.len() {
            offset = self.disassemble_instr(offset, &mut s);
        }
        
        s
    }
    
    pub fn disassemble_instr(&self, offset: usize, s: &mut String) -> usize {
        s.push_str(&format!("{:04X?}  ", offset));
        
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
            s.push_str(&format!("{: >line_pad$}  ", "|"));
        }
        else {
            s.push_str(&format!("{:0>line_pad$}  ", curr_line));
        }

        let instr = self[offset];
        let op = Op::from(instr);

        match op {
            //Declarations And Variables -------------------------------------------
            //Constants
            Op::Constant => self.disassemble_instr_const(op, offset, s),
            Op::ConstantLong => self.disassemble_instr_const_long(op, offset, s),
            //Globals
            Op::DefineGlobal => self.disassemble_instr_const(op, offset, s),
            Op::DefineGlobalLong => self.disassemble_instr_const_long(op, offset, s),
            Op::GetGlobal => self.disassemble_instr_const(op, offset, s),
            Op::GetGlobalLong => self.disassemble_instr_const_long(op, offset, s),
            Op::SetGlobal => self.disassemble_instr_const(op, offset, s),
            Op::SetGlobalLong => self.disassemble_instr_const_long(op, offset, s),
            //Locals
            Op::GetLocal => self.disassemble_instr_w_arg(op, offset, s),
            Op::GetLocalLong => self.disassemble_instr_w_arg_long(op, offset, s),
            Op::SetLocal => self.disassemble_instr_w_arg(op, offset, s),
            Op::SetLocalLong => self.disassemble_instr_w_arg_long(op, offset, s),


            //Keywords --------------------------------------------------------
            Op::Return => self.disassemble_instr_simple(op, offset, s),
            Op::Print => self.disassemble_instr_simple(op, offset, s),
            //Internal
            Op::Pop => self.disassemble_instr_simple(op, offset, s),
            Op::PopN => self.disassemble_instr_w_arg(op, offset, s),
            Op::PopNLong => self.disassemble_instr_w_arg_long(op, offset, s),
            Op::Jump => self.disassemble_jump(op, 1, offset, s),
            Op::JumpIfFalse => self.disassemble_jump(op, 1, offset, s),
            Op::JumpIfTrue => self.disassemble_jump(op, 1, offset, s),
            Op::Loop => self.disassemble_jump(op, -1, offset, s),


            //Unary operators -------------------------------------------------
            //Arithmetic
            Op::Negate => self.disassemble_instr_simple(op, offset, s),
            //Logical
            Op::LogicalNot => self.disassemble_instr_simple(op, offset, s),
            //Bitwise
            Op::BitwiseNot => self.disassemble_instr_simple(op, offset, s),


            //Binary operators ------------------------------------------------
            //Arithmetic
            Op::Add => self.disassemble_instr_simple(op, offset, s),
            Op::Subtract => self.disassemble_instr_simple(op, offset, s),
            Op::Multiply => self.disassemble_instr_simple(op, offset, s),
            Op::Divide => self.disassemble_instr_simple(op, offset, s),
            Op::Modulus => self.disassemble_instr_simple(op, offset, s),
            //Logical
            Op::LogicalXor => self.disassemble_instr_simple(op, offset, s),
            //Bitwise
            Op::BitwiseAnd => self.disassemble_instr_simple(op, offset, s),
            Op::BitwiseOr => self.disassemble_instr_simple(op, offset, s),
            Op::BitwiseXor => self.disassemble_instr_simple(op, offset, s),
            Op::BitwiseLShift => self.disassemble_instr_simple(op, offset, s),
            Op::BitwiseRShift => self.disassemble_instr_simple(op, offset, s),
            //Comparison
            Op::Equal => self.disassemble_instr_simple(op, offset, s),
            Op::NotEqual => self.disassemble_instr_simple(op, offset, s),
            Op::Greater => self.disassemble_instr_simple(op, offset, s),
            Op::Less => self.disassemble_instr_simple(op, offset, s),
            Op::GreaterEqual => self.disassemble_instr_simple(op, offset, s),
            Op::LessEqual => self.disassemble_instr_simple(op, offset, s),
            //Internal
            Op::Concat => self.disassemble_instr_simple(op, offset, s),

            //Other -----------------------------------------------------------
            Op::Unknown(_) => self.disassemble_instr_unknown(instr, offset, s)
        }
    }
    
    fn disassemble_instr_simple(&self, op: Op, offset: usize, s: &mut String) -> usize {
        s.push_str(&format!("{}\n", op.name_padded()));
        offset + 1
    }
    
    fn disassemble_instr_const(&self, op: Op, offset: usize, s: &mut String) -> usize {
        s.push_str(&format!("{}  {:04X?}  ", op.name_padded(), self[offset + 1]));
        self.write_value(self[offset + 1] as usize, s);
        s.push('\n');
        offset + 2
    }

    fn disassemble_instr_const_long(&self, op: Op, offset: usize, s: &mut String) -> usize {
        //Combine the next 2 arguments to get the full index
        let a1 = self[offset + 1];
        let a2 = self[offset + 2];

        let combined = ((a1 as u16) << 8) + (a2 as u16);

        s.push_str(&format!("{}  {:04X?}  ", op.name_padded(), combined));

        self.write_value(combined as usize, s);
        s.push('\n');
        offset + 3
    }

    fn disassemble_instr_w_arg(&self, op: Op, offset: usize, s: &mut String) -> usize {
        s.push_str(&format!("{}  {:04X?}", op.name_padded(), self[offset + 1]));
        s.push('\n');
        offset + 2
    }

    fn disassemble_instr_w_arg_long(&self, op: Op, offset: usize, s: &mut String) -> usize {
        //Combine the next 2 arguments to get the full index
        let a1 = self[offset + 1];
        let a2 = self[offset + 2];

        let combined = ((a1 as u16) << 8) + (a2 as u16);

        s.push_str(&format!("{}  {:04X?}", op.name_padded(), combined));
        s.push('\n');
        offset + 3
    }

    fn disassemble_jump(&self, op: Op, sign: isize, offset: usize, s: &mut String) -> usize {
        //Combine the next 2 arguments to get the full index
        let a1 = self[offset + 1];
        let a2 = self[offset + 2];

        let combined = ((a1 as u16) << 8) + (a2 as u16);

        //Instead of writing the jump amount, write the location it will jump to
        s.push_str(&format!("{}  {:04X?}", op.name_padded(), 
            (offset + 3_usize).saturating_add_signed(combined as isize * sign)));
        s.push('\n');
        offset + 3
    }

    fn write_value(&self, n: usize, s: &mut String) {

        if n >= self.constants.count() {
            s.push_str(&format!("<undefined>"));
        }
        else {
            s.push_str(&format!("{}", self.constants[n]));
        }
    }

    fn disassemble_instr_unknown(&self, instr: u8, offset: usize, s: &mut String) -> usize {
        s.push_str(&format!("{}\n", Op::Unknown(instr).name_padded()));
        offset + 1
    }
    
}

// pub struct FormattableInstr<'a> {
//     chunk: &'a Chunk,
//     offset: usize
// }

// impl<'a> FormattableInstr<'a> {
//     pub fn new(chunk: &'a Chunk, offset: usize) -> Self {
//         Self {
//             chunk,
//             offset
//         }
//     }

//     pub fn with_offset(mut self, offset: usize) -> Self {
//         self.offset = offset;
//         self
//     }
// }

// impl<'a> Display for FormattableInstr<'a> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         self.chunk.disassemble_instr(self.offset, s);
//         Ok(())
//     }
// }