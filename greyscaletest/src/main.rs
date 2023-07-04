use greyscale::{chunk::*, value::Value, vm::VirtualMachine, ops::Op};

fn main() {
    let mut c = Chunk::default().with_name(Some("Test Chunk".to_string()));

    let constant = c.add_const(Value::Double(15_f64));

    c.write(Op::Constant.into());
    c.write(constant as u8);

    let constant = c.add_const(Value::Double(13.4_f64));

    c.write(Op::Constant.into());
    c.write(constant as u8);

    c.write(Op::Divide.into());

    c.write(Op::Negate.into());

    c.write(Op::Return.into());
    
    let mut vm = VirtualMachine::new(c);

    match vm.execute() {
        Ok(()) => {
            
        },
        Err(e) => {
            match e {
                greyscale::vm::error::GreyscaleError::CompileErr(ce) => {
                    println!("A compile error occurred: {ce}");
                },
                greyscale::vm::error::GreyscaleError::RuntimeErr(re) => {
                    println!("A runtime error occurred: {re}");
                },
            }
        },
    }
}