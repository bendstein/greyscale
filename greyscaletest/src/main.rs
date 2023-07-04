use greyscale::{util::string::GraphemeString, lexer::Lexer};
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program_str = "";
    let program = GraphemeString::new(program_str);
    let lexer = Lexer::new(&program);

    for token_result in lexer {
        match token_result {
            Ok(token) => {
                println!("{}: ({}/{}..{}): {}", token.token_type().as_string(), token.line(), token.range().start, token.range().end, token.token_type().as_program_string(&program));

                // if let TokenType::String(range, stype) = token.token_type() {
                //     if stype == &StringType::Interpolated {
                //         //Print out interpolated parts

                //         let interp_lexer = InterpStringLexer::new(&program).move_range(range.start, range.end, token.line());

                //         for interp_result in interp_lexer {
                //             match interp_result {
                //                 Ok(interp_token) => {
                //                     println!("    {}: ({}/{}..{}): {}", interp_token.token_type().as_string(), interp_token.line(), interp_token.range().start, interp_token.range().end, interp_token.token_type().as_program_string(&program));
                //                 },
                //                 Err(interp_err) => {
                //                     match interp_err {
                //                         GreyscaleError::CompileErr(e) => {
                //                             eprintln!("Compiler error in interpolated string: {e}")
                //                         },
                //                         GreyscaleError::RuntimeErr(e) => {
                //                             eprintln!("Runtime error in interpolated string: {e}")
                //                         }
                //                     }
                //                 }
                //             }
                //         }
                //     }
                // }
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