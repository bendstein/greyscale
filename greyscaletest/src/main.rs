extern crate unicode_segmentation;

use std::rc::Rc;
use std::time::Instant;
use greyscale::compiler::Compiler;
use greyscale::parser::settings::ParserSettings;
use greyscale::vm::settings::VMSettings;
use greyscale::{vm, constants};
use unicode_segmentation::UnicodeSegmentation;

use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program: &str = "
    for let i = 0; i < 5; i += 1 {
        for let j = 0; j < i; j += 1 {
            print `{i} + {j} = {i + j}`;
        }
    }
    ";

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
        handle_err(parse_err);
        return;
    }

    let parsed = parse_result.unwrap();

    if (constants::TRACE & constants::TRACE_OUTPUT_PARSE_TREE) == constants::TRACE_OUTPUT_PARSE_TREE {
        let formatted_parsed = parsed.debug_string(Rc::clone(&rc_graphemes));
        println!("{formatted_parsed}");
    }

    let compile_result = Compiler::default().compile_ast(Rc::clone(&rc_graphemes), parsed);

    if let Err(compile_err) = &compile_result {
        handle_err(compile_err);
        return;
    }

    let mut compiled = compile_result.unwrap().chunk;

    if (constants::TRACE & constants::TRACE_BENCHMARK) == constants::TRACE_BENCHMARK {
        println!("Finished compiling in {}ms", compile_start.elapsed().as_millis());
    }

    if (constants::TRACE & constants::TRACE_OUTPUT_COMPILED) == constants::TRACE_OUTPUT_COMPILED {
        compiled.name = Some(String::from("Trace"));
        println!("{compiled}");
    }

    let exec_start = Instant::now();

    let vm = vm::VirtualMachine::new_with_settings(compiled, VMSettings {
        ignore_final_pop: false
    });

    if let Err(error) = vm {
        handle_err(&error);
        return;
    }

    let mut vm = vm.unwrap();

    let execute_result = vm.execute();

    if let Err(execute_err) = &execute_result {
        handle_err(execute_err);
        return;
    }

    if let Some(r) = vm.peek_value() {
        println!("{r}");
    }

    if (constants::TRACE & constants::TRACE_BENCHMARK) == constants::TRACE_BENCHMARK {
        println!("Finished execution in {}ms", exec_start.elapsed().as_millis());
    }

    if (constants::TRACE & constants::TRACE_VM) == constants::TRACE_VM {
        println!("\n--End State--\n");
        print!("STACK: ");
        vm.stack_trace();
    }

}

fn handle_err(err: &GreyscaleError) {
    match err {
        GreyscaleError::CompileErr(m, location) => eprintln!("[Ln: {}, Col: {}] {m}", location.line, location.column),
        GreyscaleError::RuntimeErr(m, location) => eprintln!("{m} at: line {}", location.line),
        GreyscaleError::AggregateErr(inner) => {
            for i in inner {
                handle_err(i)
            }
        }
    }
}
