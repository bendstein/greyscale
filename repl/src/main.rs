use std::{io::Write, time::{Duration, SystemTime}, rc::Rc};

use greyscale::{vm::{VirtualMachine, error::GreyscaleError, settings::VMSettings}, parser::{Parser, settings::ParserSettings, ast::AST}, compiler::{Compiler, CompilerState}, value::object::Native};
use unicode_segmentation::UnicodeSegmentation;

const START_MSG: &str = "Welcome to the Greyscale REPL. Enter the expression to evaluate:";
const PROMPT: &str = "> ";

fn main() {
    let args: Vec<String> = std::env::args()
        .skip(1)
        .collect();

    //Initialize compiler state
    let mut compiler_state = CompilerState::default();

    //Initialize VM
    let mut vm = VirtualMachine::default_with_settings(VMSettings {
        ignore_final_pop: true
    }).register_natives(vec![
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
        },
        Native {
            arity: 0,
            name: "exit".to_string(),
            function: |_, _| {
                std::process::exit(0);
            }
        }
    ]);

    println!("\x1B[2J\x1B[1;1H{START_MSG}");

    loop {
        //Display prompt
        print!("{PROMPT}");

        match std::io::stdout().flush() {
            Ok(_) => (),
            Err(err) => {
                eprintln!("Failed to flush stdout: {err}");
                continue;
            }
        }

        //Read next line
        let mut input = String::new();

        match std::io::stdin().read_line(&mut input) {
            Ok(_) => (),
            Err(err) => {
                eprintln!("Failed to read input: {err}");
                continue;
            }
        }

        input = String::from(input.trim_end());

        //Break input into graphemes and lex/parse it
        let graphemes = input.graphemes(true).collect::<Vec<&str>>();
        let rc_graphemes: Rc<Vec<&str>> = Rc::from(graphemes);
        
        let parse_result = Parser::parse_with_settings(Rc::clone(&rc_graphemes), ParserSettings {
            allow_implicit_final_semicolon: true
        });
    
        if let Err(parse_err) = &parse_result {
            eprintln!("{}", handle_err(parse_err.clone()));
            continue;
        }
    
        let parsed = parse_result.unwrap();
        let ast_len = parsed.statements.len();

        let prev_vm_state = vm.get_state();
        let mut prev_compiler_state = compiler_state.clone();
        let mut success = true;

        //Compile and execute each statement
        for (index, statement) in parsed.statements.into_iter().enumerate() {
            //Create an AST with only this statement
            let stmt_ast = AST::new(vec![statement]);

            //Compile the statement into bytecode
            let mut compiler = Compiler::default().with_state(prev_compiler_state.clone());
            let compile_result = compiler.compile_ast_notreturn(Rc::clone(&rc_graphemes), stmt_ast);

            if let Err(compile_err) = &compile_result {
                eprintln!("{}", handle_err(compile_err.clone()));
                break;
            }

            let compiled = compile_result.unwrap().chunk;

            //Assign chunk to VM
            _ = vm.swap_chunk(compiled);

            let vm_result = if args.contains(&"/debug".to_string()) {
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
            };
    
            if let Err(msg) = vm_result {
                eprintln!("{msg}");
                success = false;
                break;
            }
    
            if index == ast_len - 1 {
                if let Some(rv) = vm.pop_value() {
                    if !rv.is_void() {
                        println!("{rv}");
                    }
                }

                vm.flush_stack();
            }
    
            //Record state of compiler on success
            prev_compiler_state = compiler.get_state();
        }

        //Record state of compiler on success
        if success {
            compiler_state = prev_compiler_state;
        }
        //Rollback VM state on failure
        else {
            vm = vm.with_state(prev_vm_state);
        }
    }
}

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