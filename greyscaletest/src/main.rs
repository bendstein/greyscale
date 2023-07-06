use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program = "0x15F;";
    
    let parse_result = Parser::parse(program);

    if let Err(parse_err) = &parse_result {
        for err in parse_err {
            eprintln!("{}", match err {
                GreyscaleError::CompileErr(m) => m,
                GreyscaleError::RuntimeErr(m) => m,
            });
        }
        
        return;
    }

    let _parsed = parse_result.unwrap();

    println!("{:#?}", _parsed);
}