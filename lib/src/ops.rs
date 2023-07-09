use std::{collections::HashMap, fmt::Display};

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
pub enum Op {
    //Declarations And Variables ---
    //Constants
    Constant,
    ConstantLong,
    //Globals
    DefineGlobal,
    DefineGlobalLong,
    GetGlobal,
    GetGlobalLong,
    SetGlobal,
    SetGlobalLong,
    //Locals
    GetLocal,
    GetLocalLong,
    SetLocal,
    SetLocalLong,


    //Keywords -----------------------
    #[default]
    Return,
    Print,
    //Internal
    Pop,
    PopN,
    PopNLong,


    //Unary operators ----------------
    //Arithmetic
    Negate,
    //Logical
    LogicalNot,
    //Bitwise
    BitwiseNot,

    //Binary operators ---------------
    //Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    //Logical
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    //Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLShift,
    BitwiseRShift,
    //Comparison
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    //Internal
    Concat,


    //Other --------------------------
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

//Declarations And Variables -------
//Constants
pub const OP_CONSTANT: u8       = 0;
pub const OP_CONSTANT_LONG: u8  = 1;
//Globals
pub const OP_DEF_GLOBAL: u8     = 2;
pub const OP_DEF_GLOBAL_LONG: u8= 3;
pub const OP_GET_GLOBAL: u8     = 4;
pub const OP_GET_GLOBAL_LONG: u8= 5;
pub const OP_SET_GLOBAL: u8     = 6;
pub const OP_SET_GLOBAL_LONG: u8= 7;
//Locals
pub const OP_GET_LOCAL: u8      = 8;
pub const OP_GET_LOCAL_LONG: u8 = 9;
pub const OP_SET_LOCAL: u8      = 10;
pub const OP_SET_LOCAL_LONG: u8 = 11;


//Keywords ---------------------------
pub const OP_RETURN: u8         = 30;
pub const OP_PRINT: u8          = 31;
//Internal
pub const OP_POP: u8            = 32;
pub const OP_POP_N: u8          = 33;
pub const OP_POP_N_LONG: u8     = 34;


//Unary operators --------------------
//Arithmetic
pub const OP_NEGATE: u8         = 50;
//Logical
pub const OP_LOGICAL_NOT: u8    = 51;
//Bitwise
pub const OP_BITWISE_NOT: u8    = 52;


//Binary operators -------------------
//Arithmetic
pub const OP_ADD: u8            = 60;
pub const OP_SUBTRACT: u8       = 61;
pub const OP_MULTIPLY: u8       = 62;
pub const OP_DIVIDE: u8         = 63;
pub const OP_MODULUS: u8        = 64;
//Logical
pub const OP_LOGICAL_AND: u8    = 65;
pub const OP_LOGICAL_OR: u8     = 66;
pub const OP_LOGICAL_XOR: u8    = 67;
//Bitwise
pub const OP_BITWISE_AND: u8    = 68;
pub const OP_BITWISE_OR: u8     = 69;
pub const OP_BITWISE_XOR: u8    = 70;
pub const OP_BITWISE_LSHIFT: u8 = 71;
pub const OP_BITWISE_RSHIFT: u8 = 72;
//Comparison
pub const OP_EQUAL: u8          = 80;
pub const OP_NOT_EQUAL: u8      = 81;
pub const OP_GREATER: u8        = 82;
pub const OP_LESS: u8           = 83;
pub const OP_GREATER_EQUAL: u8  = 84;
pub const OP_LESS_EQUAL: u8     = 85;
//Internal
pub const OP_CONCAT: u8         = 86;


lazy_static! {
    static ref OPS_PAIRS: Vec<(u8, Op)> = vec![
        //Declarations And Variables ---------
        //Constants
        (OP_CONSTANT, Op::Constant),
        (OP_CONSTANT_LONG, Op::ConstantLong),
        //Globals
        (OP_DEF_GLOBAL, Op::DefineGlobal),
        (OP_DEF_GLOBAL_LONG, Op::DefineGlobalLong),
        (OP_GET_GLOBAL, Op::GetGlobal),
        (OP_GET_GLOBAL_LONG, Op::GetGlobalLong),
        (OP_SET_GLOBAL, Op::SetGlobal),
        (OP_SET_GLOBAL_LONG, Op::SetGlobalLong),
        //Locals
        (OP_GET_LOCAL, Op::GetLocal),
        (OP_GET_LOCAL_LONG, Op::GetLocalLong),
        (OP_SET_LOCAL, Op::SetLocal),
        (OP_SET_LOCAL_LONG, Op::SetLocalLong),


        //Keywords -----------------------------
        (OP_RETURN, Op::Return),
        (OP_PRINT, Op::Print),
        //Internal
        (OP_POP, Op::Pop),
        (OP_POP_N, Op::PopN),
        (OP_POP_N_LONG, Op::PopNLong),


        //Unary operators ----------------------
        //Arithmetic
        (OP_NEGATE, Op::Negate),     
        //Logical
        (OP_LOGICAL_NOT, Op::LogicalNot),
        //Bitwise
        (OP_BITWISE_NOT, Op::BitwiseNot),


        //Binary operators ---------------------
        //Arithmetic
        (OP_ADD, Op::Add),
        (OP_SUBTRACT, Op::Subtract),
        (OP_MULTIPLY, Op::Multiply),
        (OP_DIVIDE, Op::Divide),
        (OP_MODULUS, Op::Modulus),
        //Logical
        (OP_LOGICAL_AND, Op::LogicalAnd),
        (OP_LOGICAL_OR, Op::LogicalOr),
        (OP_LOGICAL_XOR, Op::LogicalXor),
        //Bitwise
        (OP_BITWISE_AND, Op::BitwiseAnd),
        (OP_BITWISE_OR, Op::BitwiseOr),
        (OP_BITWISE_XOR, Op::BitwiseXor),
        (OP_BITWISE_LSHIFT, Op::BitwiseLShift),
        (OP_BITWISE_RSHIFT, Op::BitwiseRShift),
        //Comparison
        (OP_EQUAL, Op::Equal),
        (OP_NOT_EQUAL, Op::NotEqual),
        (OP_GREATER, Op::Greater),
        (OP_LESS, Op::Less),
        (OP_GREATER_EQUAL, Op::GreaterEqual),
        (OP_LESS_EQUAL, Op::LessEqual),
        //Internal
        (OP_CONCAT, Op::Concat)
    ];  
    
    static ref OP_NAMES: HashMap<Op, &'static str> = [
        //Declarations And Variables ---------
        //Constants
        (Op::Constant, "OP_CONSTANT"),
        (Op::ConstantLong, "OP_CONSTANT_LONG"),
        //Globals
        (Op::DefineGlobal, "OP_DEFINE_GLOBAL"),
        (Op::DefineGlobalLong, "OP_DEFINE_GLOBAL_LONG"),
        (Op::GetGlobal, "OP_GET_GLOBAL"),
        (Op::GetGlobalLong, "OP_GET_GLOBAL_LONG"),
        (Op::SetGlobal, "OP_SET_GLOBAL"),
        (Op::SetGlobalLong, "OP_SET_GLOBAL_LONG"),
        //Locals
        (Op::GetLocal, "OP_GET_LOCAL"),
        (Op::GetLocalLong, "OP_GET_LOCAL_LONG"),
        (Op::SetLocal, "OP_SET_LOCAL"),
        (Op::SetLocalLong, "OP_SET_LOCAL_LONG"),


        //Keywords -----------------------------
        (Op::Return, "OP_RETURN"),
        (Op::Print, "OP_PRINT"),
        //Internal
        (Op::Pop, "OP_POP"),
        (Op::PopN, "OP_POP_N"),
        (Op::PopNLong, "OP_POP_N_LONG"),


        //Unary operators ----------------------
        //Arithmetic
        (Op::Negate, "OP_NEGATE"),     
        //Logical
        (Op::LogicalNot, "OP_LOGICAL_NOT"),
        //Bitwise
        (Op::BitwiseNot, "OP_BITWISE_NOT"),


        //Binary operators ---------------------
        //Arithmetic
        (Op::Add, "OP_ADD"),
        (Op::Subtract, "OP_SUBTRACT"),
        (Op::Multiply, "OP_MULTIPLY"),
        (Op::Divide, "OP_DIVIDE"),
        (Op::Modulus, "OP_MODULUS"),
        //Logical
        (Op::LogicalAnd, "OP_LOGICAL_AND"),
        (Op::LogicalOr, "OP_LOGICAL_OR"),
        (Op::LogicalXor, "OP_LOGICAL_XOR"),
        //Bitwise
        (Op::BitwiseAnd, "OP_BITWISE_AND"),
        (Op::BitwiseOr, "OP_BITWISE_OR"),
        (Op::BitwiseXor, "OP_BITWISE_XOR"),
        (Op::BitwiseLShift, "OP_BITWISE_LSHIFT"),
        (Op::BitwiseRShift, "OP_BITWISE_RSHIFT"),
        //Comparison
        (Op::Equal, "OP_EQUAL"),
        (Op::NotEqual, "OP_NOT_EQUAL"),
        (Op::Greater, "OP_GREATER"),
        (Op::Less, "OP_LESS"),
        (Op::GreaterEqual, "OP_GREATER_EQUAL"),
        (Op::LessEqual, "OP_LESS_EQUAL"),
        //Internal
        (Op::Concat, "OP_CONCAT")
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