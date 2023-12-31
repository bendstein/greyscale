use std::{fs::File, io::{Read, Write}, time::{Duration, Instant, SystemTime}, rc::Rc};

use greyscale::{vm::{VirtualMachine, error::GreyscaleError}, constants, parser::{Parser, settings::ParserSettings}, compiler::Compiler, chunk::Chunk, value::object::Native};
use unicode_segmentation::UnicodeSegmentation;

fn main() -> Result<(), String> {
    //Set to use virtual terminal so that control characters work on windows
    _ = colored::control::set_virtual_terminal(true);

    let args: Vec<String> = std::env::args()
        .skip(1)
        .collect();

    //If piped input, read program from stdin, otherwise, read from file
    let input: Vec<u8> = if atty::is(atty::Stream::Stdin) {
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

        let mut bytes: Vec<u8> = Vec::new();

        let _ = file.read_to_end(&mut bytes)
            .map_err(handle_io_err)?;

        Ok(bytes)
    }
    else {
        let handle_io_err = |err: std::io::Error| {
            match err.kind() {
                std::io::ErrorKind::NotFound => String::from("Failed to find the requested resource."),
                std::io::ErrorKind::PermissionDenied => String::from("You do not have access to read the requested resource."),
                std::io::ErrorKind::OutOfMemory => String::from("Not enough memory to read the requested resource."),
                _ => String::from("Failed to read the requested resource."),
            }
        };

        let bytes: Result<Vec<u8>, std::io::Error> = std::io::stdin()
            .lock()
            .bytes()
            .collect();

        bytes.map_err(handle_io_err)
    }?;

    //Check if bytes begins with GRYSCL signature
    let mut compiled = if Chunk::verify_signature(&input) {
        Chunk::decode_from_bytes(&input)
    }
    else {
        let program = std::str::from_utf8(&input)
            .map_err(|err| err.to_string())?;

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
    
        let compile_result = Compiler::default().compile_ast(Rc::clone(&rc_graphemes), parsed);
    
        if let Err(compile_err) = &compile_result {
            return Err(handle_err(compile_err.clone()));
        }
    
        let compiled = compile_result.unwrap();
    
        if (constants::TRACE & constants::TRACE_BENCHMARK) == constants::TRACE_BENCHMARK {
            println!("Finished compiling in {}ms", compile_start.elapsed().as_millis());
        }

        Ok(compiled.chunk)
    }?;

    if args.contains(&"/writebin".to_string()) {
        let encoded = compiled.encode_as_bytes();

        File::create("test2.bin")
            .expect("Failed to create bin")
            .write_all(&encoded)
            .expect("Failed to write bin");
    }

    if (constants::TRACE & constants::TRACE_OUTPUT_COMPILED) == constants::TRACE_OUTPUT_COMPILED {
        compiled.name = Some(String::from("Trace"));
        println!("{compiled}");
    }

    let mut vm = VirtualMachine::new(compiled)
        .map_err(handle_err)?
        .register_natives(vec![
            Native {
                arity: 0,
                name: "time".to_string(),
                function: |_, _| {
                    let millis = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap_or_default().as_millis();
                    Ok(greyscale::value::Value::Int(millis.min(i64::MAX as u128) as i64))
                }
            },
            Native {
                arity: 1,
                name: "sleep".to_string(),
                function: |args, line| {
                    let duration = args.first().unwrap();
                    
                    if let greyscale::value::Value::Int(n) = duration {
                        std::thread::sleep(Duration::from_millis(*n as u64));
                        Ok(greyscale::value::Value::Void)
                    }
                    else {
                        Err(GreyscaleError::RuntimeErr("Expected an integer".to_string(), greyscale::location::Location { column: 0, line }))
                    }
                }
            },
            Native {
                arity: 1,
                name: "panic".to_string(),
                function: |args, _| {
                    let message = args.first().unwrap();

                    panic!("{}", message);
                }
            },
            Native {
                arity: 1,
                name: "print".to_string(),
                function: |args, _| {
                    let message = args.first().unwrap();
                    print!("{}", message);
                    Ok(greyscale::value::Value::Void)
                }
            },
            Native {
                arity: 1,
                name: "println".to_string(),
                function: |args, _| {
                    let message = args.first().unwrap();
                    println!("{}", message);
                    Ok(greyscale::value::Value::Void)
                }
            },
            Native {
                arity: 0,
                name: "readln".to_string(),
                function: |_, line| {
                    let mut input: String = String::new();
                    if let Err(err) = std::io::stdin().read_line(&mut input) {
                        let message = match err.kind() {
                            std::io::ErrorKind::NotFound => "An I/O error occurred: Not Found.".to_string(),
                            std::io::ErrorKind::PermissionDenied => "An I/O error occurred: Permission Denied.".to_string(),
                            std::io::ErrorKind::TimedOut => "An I/O error occurred: Operation Timed Out.".to_string(),
                            std::io::ErrorKind::UnexpectedEof => "An I/O error occurred: Unexpected End Of Input.".to_string(),
                            std::io::ErrorKind::OutOfMemory => "An I/O error occurred: Out of Memory.".to_string(),
                            _ => "An I/O error occurred.".to_string(),
                        };

                        return Err(GreyscaleError::RuntimeErr(message, greyscale::location::Location { column: 0, line }));
                    }

                    Ok(greyscale::value::Value::Object(Rc::new(greyscale::value::object::Object::String(input.trim().to_string()))))
                }
            },
            Native {
                arity: 0,
                name: "clear".to_string(),
                function: |_, _| {
                    print!("\x1B[2J\x1B[1;1H");
                    Ok(greyscale::value::Value::Void)
                }
            },
            Native {
                arity: 1,
                name: "parseInt".to_string(),
                function: |args, line| {
                    let s = args.first().unwrap();

                    if s.is_object_string() {
                        let s = s.unwrap_object_string();

                        match s.parse::<i64>() {
                            Ok(v) => Ok(greyscale::value::Value::Int(v)),
                            Err(_) => Err(GreyscaleError::RuntimeErr(format!("{s} is not in a valid format to be parsed as an integer."), 
                                greyscale::location::Location { column: 0, line }))
                        }
                    }
                    else {
                        Err(GreyscaleError::RuntimeErr("Expected a string".to_string(), greyscale::location::Location { column: 0, line }))
                    }
                }
            },
            Native {
                arity: 1,
                name: "parseDouble".to_string(),
                function: |args, line| {
                    let s = args.first().unwrap();

                    if s.is_object_string() {
                        let s = s.unwrap_object_string();

                        match s.parse::<f64>() {
                            Ok(v) => Ok(greyscale::value::Value::Double(v)),
                            Err(_) => Err(GreyscaleError::RuntimeErr(format!("{s} is not in a valid format to be parsed as a double."), 
                                greyscale::location::Location { column: 0, line }))
                        }
                    }
                    else {
                        Err(GreyscaleError::RuntimeErr("Expected a string".to_string(), greyscale::location::Location { column: 0, line }))
                    }
                }
            }
        ]);

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