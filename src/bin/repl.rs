use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use monkey_rust::{
    evaluator::environment::Environment,
    evaluator::eval,
    parser::{eprint_parse_errors, parse},
};

const PROMPT: &str = ">> ";

pub fn main() {
    let env = Rc::new(RefCell::new(Environment::new()));

    println!("Welcome to the Monkey programming language!");
    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        match io::stdout().flush() {
            Ok(_) => match io::stdin().read_line(&mut input) {
                Ok(_) => match parse(&input) {
                    Ok(program) => {
                        let evaluated = eval(program, env.clone());
                        println!("{}", evaluated);
                    }
                    Err(errs) => eprint_parse_errors(&errs),
                },
                Err(error) => eprintln!("ERROR: {error}"),
            },
            Err(error) => eprintln!("ERROR: {error}"),
        };
    }
}
