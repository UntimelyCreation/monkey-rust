use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{ast::AstNode, evaluator::eval, lexer::Lexer, object::Environment, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start_repl() {
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        match io::stdout().flush() {
            Ok(_) => match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    let lexer = Lexer::new(input.as_str());
                    let mut parser = Parser::new(lexer);
                    if let Some(program) = parser.parse_program() {
                        if let Some(evaluated) = eval(AstNode::Program(program), env.clone()) {
                            println!("{}", evaluated.inspect());
                        }
                    }
                }
                Err(error) => println!("ERROR: {error}"),
            },
            Err(error) => println!("ERROR: {error}"),
        };
    }
}
