use std::{fs::File, io::{Read, Write}, time::Instant, rc::Rc};

use greyscale::{{vm::error::GreyscaleError}, constants, parser::{Parser, settings::ParserSettings}, compiler::Compiler};
use unicode_segmentation::UnicodeSegmentation;

fn main() -> Result<(), String> {
    let args: Vec<String> = std::env::args()
        .skip(1)
        .collect();

    if args.len() < 2 {
        return Err("Expected a file path and output path.".to_string());
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

    let mut compiled = compile_result.unwrap();

    if (constants::TRACE & constants::TRACE_BENCHMARK) == constants::TRACE_BENCHMARK {
        println!("Finished compiling in {}ms", compile_start.elapsed().as_millis());
    }

    if (constants::TRACE & constants::TRACE_OUTPUT_COMPILED) == constants::TRACE_OUTPUT_COMPILED {
        compiled.name = Some(String::from("Trace"));
        println!("{compiled}");
    }

    let bytes = compiled.encode_as_bytes();

    let output_path = &args[1];

    let mut file = File::create(output_path)
        .map_err(handle_io_err)?;

    file.write_all(&bytes)
        .map_err(handle_io_err)?;

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

    Ok(())
}