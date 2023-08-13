use std::io::{self, Write};

use crate::{lexer::Lexer, parser::Parser};

const PROMPT: &str = ">> ";

pub fn start_repl() {
    loop {
        let mut input = String::new();

        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).unwrap();

        let lexer = Lexer::new(input.as_str());
        let mut parser = Parser::new(lexer);
        if let Some(program) = parser.parse_program() {
            println!("{}", program.to_string());
        }
    }
}
