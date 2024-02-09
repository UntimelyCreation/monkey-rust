use std::{
    error::Error,
    io::{self, Write},
};

use monkey_rust::{
    compiler::Compiler,
    parser::{eprint_parse_errors, parse},
    vm::Vm,
};

const PROMPT: &str = ">> ";

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut compiler = Compiler::new();
    let mut vm = Vm::new();

    println!("Welcome to the Monkey programming language!");
    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        match parse(&input) {
            Ok(program) => match compiler.compile(&program) {
                Ok(bytecode) => {
                    vm.update(bytecode);
                    match vm.run() {
                        Ok(_) => {
                            println!("{}", vm.last_popped());
                        }
                        Err(error) => eprintln!("vm error: {error}"),
                    }
                }
                Err(error) => eprintln!("compile error: {error}"),
            },
            Err(errs) => eprint_parse_errors(&errs),
        };
    }
}
