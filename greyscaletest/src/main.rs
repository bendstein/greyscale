use greyscale::parser::Parser;
use greyscale::vm::error::GreyscaleError;

fn main() {
    let program = "`abce {gef} oranges {'the' + `quick {'brown ' + 'fox'}`}`;";
    
    let parse_result = Parser::parse(program);

    if let Err(parse_err) = &parse_result {

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

        handle_err(parse_err);
        
        return;
    }

    let _parsed = parse_result.unwrap();

    println!("{:#?}", _parsed);
}