use crate::value::object::Function;

#[derive(Debug, Default, PartialEq, PartialOrd, Clone)]
pub struct CallFrame {
    pub function: Function,
    pub ip: usize,
    pub stack_offset: usize
}