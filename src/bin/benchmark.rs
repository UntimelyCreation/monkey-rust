use core::panic;
use std::{cell::RefCell, env, rc::Rc, time::Instant};

use monkey_rust::{
    compiler::Compiler,
    evaluator::{environment::Environment, eval},
    parser::{eprint_parse_errors, parse},
    vm::Vm,
};

fn main() {
    let args: Vec<String> = env::args().collect();

    let engine = &args[1];

    let input = "let fibonacci = fn(x) { if (x == 0) { return 0; } else { if (x == 1) { return 1; } else { fibonacci(x - 1) + fibonacci(x - 2); } } }; fibonacci(30);";

    match parse(input) {
        Ok(program) => match engine.as_str() {
            "eval" => {
                let environment = Rc::new(RefCell::new(Environment::new()));

                let start = Instant::now();

                let result = eval(program, environment);

                let duration = start.elapsed().as_millis();

                println!(
                    "engine = {}, result = {}, duration = {}",
                    engine, result, duration
                );
            }
            "vm" => {
                let mut compiler = Compiler::new();
                match compiler.compile(&program) {
                    Ok(bytecode) => {
                        let mut vm = Vm::from_bytecode(bytecode);

                        let start = Instant::now();

                        match vm.run() {
                            Ok(_) => {
                                let duration = start.elapsed().as_millis();

                                let result = vm.last_popped();

                                println!(
                                    "engine = {}, result = {}, duration = {}",
                                    engine, result, duration
                                );
                            }
                            Err(error) => eprintln!("vm error: {error}"),
                        }
                    }
                    Err(error) => eprintln!("compile error: {error}"),
                };
            }
            _ => panic!("Engine argument should be 'eval' or 'vm'"),
        },
        Err(errs) => eprint_parse_errors(&errs),
    };
}
