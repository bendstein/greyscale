extern crate unicode_segmentation;

use std::rc::Rc;
use greyscale::compiler::Compiler;
use greyscale::parser::settings::ParserSettings;
use greyscale::{vm, constants};
use unicode_segmentation::UnicodeSegmentation;

use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program = "
        print 5 + 6;
        print 8 * 9;
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

    //println!("{:#?}", parsed);

    let compile_result = Compiler::compile_ast(Rc::clone(&rc_graphemes), parsed);

    if let Err(compile_err) = &compile_result {
        handle_err(compile_err);
        return;
    }

    let mut compiled = compile_result.unwrap();

    compiled.name = Some(String::from("Test Expr"));

    println!("{compiled}");

    let mut vm = vm::VirtualMachine::new(compiled);

    let execute_result = vm.execute();

    if let Err(execute_err) = &execute_result {
        handle_err(execute_err);
        return;
    }

    if let Some(r) = vm.peek_value() {
        println!("{r}");
    }

    println!("OK");
}

fn handle_err(err: &GreyscaleError) {
    match err {
        GreyscaleError::CompileErr(m) => eprintln!("{m}"),
        GreyscaleError::RuntimeErr(m) => eprintln!("{m}"),
        GreyscaleError::AggregateErr(inner) => {
            for i in inner {
                handle_err(i)
            }
        }
    }
}
