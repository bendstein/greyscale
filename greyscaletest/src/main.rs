extern crate unicode_segmentation;

use std::rc::Rc;
use greyscale::compiler::Compiler;
use greyscale::parser::settings::ParserSettings;
use greyscale::vm::settings::VMSettings;
use greyscale::{vm, constants};
use unicode_segmentation::UnicodeSegmentation;

use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program = "
        let x = 5;
        let y = 5.0;
        let z = 5.00;
        let a = 5.0000000000001;
        let a = 5.00000000000010000;
        print `{x}{y}{z}{a}`;
    ";

    if constants::TRACE {
        println!("Program: {program}");
    }

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

    let compile_result = Compiler::compile_ast(Rc::clone(&rc_graphemes), parsed);

    if let Err(compile_err) = &compile_result {
        handle_err(compile_err);
        return;
    }

    let mut compiled = compile_result.unwrap();

    if constants::TRACE {
        compiled.name = Some(String::from("Trace"));
        println!("{compiled}");
    }

    let mut vm = vm::VirtualMachine::new_with_settings(compiled, VMSettings {
        ignore_final_pop: true
    });

    let execute_result = vm.execute();

    if let Err(execute_err) = &execute_result {
        handle_err(execute_err);
        return;
    }

    if let Some(r) = vm.peek_value() {
        println!("{r}");
    }

    if constants::TRACE {
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
