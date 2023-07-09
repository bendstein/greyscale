use std::{fs::File, io::Read};

use greyscale::{vm::{VirtualMachine, error::GreyscaleError}, chunk::Chunk};

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args()
        .skip(1)
        .collect();

    if args.is_empty() {
        return Err("Expected a file path.".to_string());
    }

    let filepath = &args[0];

    let handle_io_err = |err: std::io::Error| {
        match err.kind() {
            std::io::ErrorKind::NotFound => format!("Failed to find file at {filepath}"),
            std::io::ErrorKind::PermissionDenied => format!("You do not have access to read the file at {filepath}"),
            std::io::ErrorKind::OutOfMemory => format!("Not enough memory to read the file at {filepath}"),
            _ => format!("Failed to read file at {filepath}"),
        }
    };

    let mut file = File::open(&args[0])
        .map_err(handle_io_err)?;

    let mut program: String = String::new();
    let _ = file.read_to_string(&mut program)
        .map_err(handle_io_err)?;

    let c: Chunk = Chunk::default();

    let mut vm = VirtualMachine::new(c);

    fn handle_err(err: GreyscaleError) -> String {
        match err {
            GreyscaleError::CompileErr(ce, loc) => {
                format!("A compile error occurred: [Ln: {}, Col: {}] {ce}", loc.line, loc.column)
            },
            GreyscaleError::RuntimeErr(re, loc) => {
                format!("A runtime error occurred: {re} at: line {}", loc.line)
            },
            GreyscaleError::AggregateErr(inner) => {
                let mut messages: Vec<String> = Vec::new();

                for i in inner {
                    let message = handle_err(i);
                    messages.push(message);
                }

                format!("One or more errors occurred:\n    {}", messages.join("\n    "))
            }
        }
    }

    vm.execute()
        .map_err(handle_err)
}