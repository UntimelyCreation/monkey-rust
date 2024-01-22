use std::io::{self, Write};

use monkey_rust::{
    compiler::Compiler,
    parser::{eprint_parse_errors, parse},
    vm::Vm,
};

const PROMPT: &str = ">> ";

pub fn main() {
    println!("Welcome to the Monkey programming language!");
    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        match io::stdout().flush() {
            Ok(_) => match io::stdin().read_line(&mut input) {
                Ok(_) => match parse(&input) {
                    Ok(program) => {
                        let mut compiler = Compiler::new();
                        match compiler.compile(&program) {
                            Ok(bytecode) => {
                                let mut vm = Vm::from_bytecode(bytecode);
                                match vm.run() {
                                    Ok(_) => {
                                        println!("{}", vm.last_popped());
                                    }
                                    Err(error) => eprintln!("runtime error: {error}"),
                                }
                            }
                            Err(error) => eprintln!("compile error: {error}"),
                        }
                    }
                    Err(errs) => eprint_parse_errors(&errs),
                },
                Err(error) => eprintln!("ERROR: {error}"),
            },
            Err(error) => eprintln!("ERROR: {error}"),
        };
    }
}
