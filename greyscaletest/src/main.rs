use greyscale::{chunk::*, vm::VirtualMachine};

fn main() {
    let mut c = Chunk::default().with_name(Some("Test Chunk".to_string()));

    c.add_const(greyscale::value::Value::Double(15_f64));
    c.add_const(greyscale::value::Value::Double(24_f64));

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(1);
    c.write_u16(1);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(0);
    c.write(0);

    c.write(2);
    
    c.metadata.new_line(2);
    c.metadata.new_line(4);
    c.metadata.new_line(8);
    c.metadata.new_line(10);
    c.metadata.new_line(12);
    c.metadata.new_line(14);
    c.metadata.new_line(16);
    c.metadata.new_line(18);
    c.metadata.new_line(20);
    c.metadata.new_line(22);

    println!("{c}");
    
    let mut vm = VirtualMachine::new(c);

    match vm.run() {
        Ok(()) => {
            println!("Ok");
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