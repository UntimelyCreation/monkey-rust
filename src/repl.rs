use std::io::{self, Write};

use crate::lexer::Lexer;

const PROMPT: &str = ">> ";

pub fn start_repl() {
    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input.as_str());
        let tokens = lexer.tokenize();

        for token in tokens {
            println!("{:?}", token);
        }
    }
}
