use std::{
    cell::RefCell,
    io::{self, Write},
    rc::Rc,
};

use crate::{
    evaluator::eval,
    object::Environment,
    parser::{eprint_parse_errors, parse},
};

const PROMPT: &str = ">> ";

pub fn start_repl() {
    let env = Rc::new(RefCell::new(Environment::new()));

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
