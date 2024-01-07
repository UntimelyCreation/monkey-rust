use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{evaluator::eval, object::Environment, parser::parse};

const PROMPT: &str = ">> ";

pub fn start_repl() {
    let env = Rc::new(RefCell::new(Environment::new()));

    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        match io::stdout().flush() {
            Ok(_) => match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if let Some(program) = parse(&input) {
                        if let Some(evaluated) = eval(program, env.clone()) {
                            println!("{}", evaluated);
                        }
                    }
                }
                Err(error) => println!("ERROR: {error}"),
            },
            Err(error) => println!("ERROR: {error}"),
        };
    }
}
