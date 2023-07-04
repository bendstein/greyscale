use greyscale::{util::string::GraphemeString, lexer::Lexer};
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program_str = "";
    let program = GraphemeString::new(program_str);
    let lexer = Lexer::new(&program);

    for token_result in lexer {
        match token_result {
            Ok(token) => {
                let content = token.get_value(&program);
                println!("{}: ({}/{}..{}): {}", token.token_type().as_string(), token.line(), token.range().start, token.range().end, content);
            },
            Err(err) => {
                match err {
                    GreyscaleError::CompileErr(e) => {
                        eprintln!("Compiler error: {e}")
                    },
                    GreyscaleError::RuntimeErr(e) => {
                        eprintln!("Runtime error: {e}")
                    }
                }
            }
        }
    }

    println!("EOF");
}