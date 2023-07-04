use std::{collections::HashMap, fmt::Display};

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Op {
    //Constant declarations
    Constant,
    ConstantLong,

    //Keywords
    #[default]
    Return,

    //Unary operators
    Negate,

    //Binary operators
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,

    //Other
    Unknown(u8)
}

impl Op {
    pub fn name(&self) -> String {
        match self {
            Op::Unknown(n) => format!("??{n}"),
            _ => String::from(*OP_NAMES.get(self).unwrap_or(&""))
        }
    }

    pub fn name_padded(&self) -> String {
        let name = self.name();
        let width = *INSTR_LEN_MAX;
        format!("{:width$}", name)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.name()))
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

//Constant declarations
pub const OP_CONSTANT: u8       = 0;
pub const OP_CONSTANT_LONG: u8  = 1;

//Keywords
pub const OP_RETURN: u8         = 2;

//Unary operators
pub const OP_NEGATE: u8         = 40;

//Binary operators
pub const OP_ADD: u8            = 50;
pub const OP_SUBTRACT: u8       = 51;
pub const OP_MULTIPLY: u8       = 52;
pub const OP_DIVIDE: u8         = 53;
pub const OP_MODULUS: u8        = 54;


lazy_static! {
    static ref OPS_PAIRS: Vec<(u8, Op)> = vec![
        //Constant declarations
        (OP_CONSTANT, Op::Constant),
        (OP_CONSTANT_LONG, Op::ConstantLong),

        //Keywords
        (OP_RETURN, Op::Return),

        //Unary operators
        (OP_NEGATE, Op::Negate),

        //Binary operators
        (OP_ADD, Op::Add),
        (OP_SUBTRACT, Op::Subtract),
        (OP_MULTIPLY, Op::Multiply),
        (OP_DIVIDE, Op::Divide),
        (OP_MODULUS, Op::Modulus),
    ];  
    
    static ref OP_NAMES: HashMap<Op, &'static str> = [
        //Constant declarations
        (Op::Constant, "OP_CONSTANT"),
        (Op::ConstantLong, "OP_CONSTANT_LONG"),

        //Keywords
        (Op::Return, "OP_RETURN"),

        //Unary Operators
        (Op::Negate, "OP_NEGATE"),

        //Binary operators
        (Op::Add, "OP_ADD"),
        (Op::Subtract, "OB_SUBTRACT"),
        (Op::Multiply, "MULTIPLY"),
        (Op::Divide, "OP_DIVIDE"),
        (Op::Modulus, "OP_MODULUS"),

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