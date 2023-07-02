use std::{collections::HashMap};

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Op {
    #[default]
    Return,
    Constant,
    ConstantLong,
    Unknown(u8)
}

impl Op {
    pub fn name(&self) -> String {
        String::from(*OP_NAMES.get(self).unwrap_or(&""))
    }

    pub fn name_padded(&self) -> String {
        let name = self.name();
        let width = *INSTR_LEN_MAX;
        format!("{:width$}", name)
    }
}

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        *OPS_FWD.get(&value).unwrap_or(&Op::Unknown(value))
    }
}

impl From<Op> for u8 {
    fn from(value: Op) -> Self {
        if let Op::Unknown(n) = value {
            n
        }
        else {
            *OPS_REV.get(&value).unwrap_or(&0_u8)
        }
    }
}

pub const OP_CONSTANT: u8 = 0;
pub const OP_CONSTANT_LONG: u8 = 1;
pub const OP_RETURN: u8 = 2;

lazy_static! {
    static ref OPS_PAIRS: Vec<(u8, Op)> = vec![
        (OP_CONSTANT, Op::Constant),
        (OP_CONSTANT_LONG, Op::ConstantLong),
        (OP_RETURN, Op::Return),
    ];  
    
    static ref OP_NAMES: HashMap<Op, &'static str> = [
        (Op::Constant, "OP_CONSTANT"),
        (Op::ConstantLong, "OP_CONSTANT_LONG"),
        (Op::Return, "OP_RETURN"),
    ].into_iter().collect();

    static ref OPS_FWD: HashMap<u8, Op> = OPS_PAIRS.iter()
        .copied()
        .collect();

    static ref OPS_REV: HashMap<Op, u8> = OPS_PAIRS.iter()
        .map(|p| (p.1, p.0))
        .collect();

    pub static ref INSTR_LEN_MAX: usize = OP_NAMES.values()
        .map(|i| i.len())
        .max()
        .unwrap_or(0_usize);
}