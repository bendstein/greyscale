use std::{fs::File, io::Read, time::{Duration, Instant}, rc::Rc};

use greyscale::{vm::{VirtualMachine, error::GreyscaleError}, constants, parser::{Parser, settings::ParserSettings}, compiler::Compiler, chunk::Chunk};
use unicode_segmentation::UnicodeSegmentation;

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

    let mut compiled = if filepath.to_uppercase().ends_with(".BIN") {
        let mut bytes: Vec<u8> = Vec::new();

        let _ = file.read_to_end(&mut bytes)
            .map_err(handle_io_err)?;

        Chunk::decode_from_bytes(&bytes)
    }
    else {
        let mut program: String = String::new();
        let _ = file.read_to_string(&mut program)
            .map_err(handle_io_err)?;

        if (constants::TRACE & constants::TRACE_OUTPUT_INPUT) == constants::TRACE_OUTPUT_INPUT {
            println!("Program: {program}");
        }
    
        let compile_start = Instant::now();
    
        let graphemes = program.graphemes(true).collect::<Vec<&str>>();
        let rc_graphemes: Rc<Vec<&str>> = Rc::from(graphemes);
        
        let parse_result = Parser::parse_with_settings(Rc::clone(&rc_graphemes), ParserSettings {
            allow_implicit_final_semicolon: true
        });
    
        if let Err(parse_err) = &parse_result {
            return Err(handle_err(parse_err.clone()));
        }
    
        let parsed = parse_result.unwrap();
    
        if (constants::TRACE & constants::TRACE_OUTPUT_PARSE_TREE) == constants::TRACE_OUTPUT_PARSE_TREE {
            let formatted_parsed = parsed.debug_string(Rc::clone(&rc_graphemes));
            println!("{formatted_parsed}");
        }
    
        let compile_result = Compiler::compile_ast(Rc::clone(&rc_graphemes), parsed);
    
        if let Err(compile_err) = &compile_result {
            return Err(handle_err(compile_err.clone()));
        }
    
        let compiled = compile_result.unwrap();
    
        if (constants::TRACE & constants::TRACE_BENCHMARK) == constants::TRACE_BENCHMARK {
            println!("Finished compiling in {}ms", compile_start.elapsed().as_millis());
        }

        compiled.chunk
    };

    if (constants::TRACE & constants::TRACE_OUTPUT_COMPILED) == constants::TRACE_OUTPUT_COMPILED {
        compiled.name = Some(String::from("Trace"));
        println!("{compiled}");
    }

    let mut vm = VirtualMachine::new(compiled)
        .map_err(handle_err)?;

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

    if args.contains(&"/debug".to_string()) {
        vm_step_through(&mut vm, !args.contains(&"/auto".to_string()))
            .map_err(|e| {
                let message = handle_err(e);
                format!("{message}\n{}", vm.get_call_stack_trace())
            })
    }
    else {
        vm.execute()
            .map_err(|e| {
                let message = handle_err(e);
                format!("{message}\n{}", vm.get_call_stack_trace())
            })
    }
}

fn vm_step_through(vm: &mut VirtualMachine, manual: bool) -> Result<(), GreyscaleError> { 
    while !vm.is_at_end() {
        write_vm_state(vm)?;

        if manual {
            let mut temp = String::new();
            let _ = std::io::stdin().read_line(&mut temp);
        }
        else {
            std::thread::sleep(Duration::from_millis(750));
        }
        
        vm.step()?;
    }

    Ok(())
}

fn write_vm_state(vm: &VirtualMachine) -> Result<(), GreyscaleError> {
    print!("\x1B[2J\x1B[1;1H");
    let ip = vm.ip();

    println!("=============================================");
    let chunk = vm.chunk()?;

    let mut offset = 0_usize;

    while offset < chunk.count() {
        let current_offset = offset;
        let mut instr = String::new();
        offset = chunk.disassemble_instr(offset, &mut instr);

        instr = instr.replace(['\r', '\n'], "");

        instr = format!("{:<41}", instr);

        //If on this instruction, draw indicator
        if current_offset <= ip && ip < offset {
            instr.push_str(" <----");
        }

        println!("{}", instr);
    }

    println!("Stack: {}", vm.get_stack_trace());

    Ok(())
}