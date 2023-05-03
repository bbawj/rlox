mod expr;
mod parser;
mod scanner;
mod stmt;
mod token;

use std::{
    fs::File,
    io::{self, BufReader, Read},
    str::FromStr,
};

use parser::Parser;
use scanner::Scanner;

#[derive(Debug)]
pub enum RloxError {
    RuntimeError,
    SyntaxError(String),
    // InternalError(Box<dyn std::error::Error>),
}

// impl From<<f64 as FromStr>::Err> for RloxError {
//     fn from(value: <f64 as FromStr>::Err) -> Self {
//         RloxError::InternalError(value)
//     }
// }

pub struct Rlox {}

impl Rlox {
    pub fn run_file(path: &str) -> Result<(), io::Error> {
        let file = File::open(path)?;
        let mut reader = BufReader::new(file);
        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;
        let mut scanner = Scanner::new(&contents);
        scanner.scan_token();

        Ok(())
    }

    pub fn run_prompt() -> io::Result<()> {
        loop {
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            let mut scanner = Scanner::new(&input);
            scanner.scan_token();
            let mut parser = Parser::new(scanner.tokens);
            println!("{:?}", parser.parse());
        }
    }
}

fn main() {
    Rlox::run_prompt();
}
