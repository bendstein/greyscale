extern crate unicode_segmentation;

use std::rc::Rc;
use greyscale::compiler::Compiler;
use unicode_segmentation::UnicodeSegmentation;

use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program = "0b11.11";

    let graphemes = program.graphemes(true).collect::<Vec<&str>>();
    let rc_graphemes: Rc<Vec<&str>> = Rc::from(graphemes);
    
    //let parse_result = Parser::parse(Rc::clone(&rc_graphemes));
    let parse_result = Parser::parse_expression(Rc::clone(&rc_graphemes));

    if let Err(parse_err) = &parse_result {
        handle_err(parse_err);
        return;
    }

    let parsed = parse_result.unwrap();

    println!("{:#?}", parsed);

    //let compile_result = Compiler::compile_ast(Rc::clone(&rc_graphemes), parsed);
    // let compile_result = Compiler::compile_expression(Rc::clone(&rc_graphemes), parsed);

    // if let Err(compile_err) = &compile_result {
    //     handle_err(compile_err);
    //     return;
    // }

    // let mut compiled = compile_result.unwrap();

    // compiled.name = Some(String::from("Test Expr"));

    // println!("{compiled}");
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
