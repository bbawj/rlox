mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod token;

use std::{
    fmt,
    fs::File,
    io::{self, BufReader, Read},
};

use interpreter::{Interpreter, Value};
use parser::Parser;
use resolver::Resolver;
use scanner::Scanner;

#[derive(Debug)]
pub enum RloxError {
    RuntimeError(String),
    SyntaxError(String),
    // InternalError(Box<dyn std::error::Error>),
    ReturnValue(Value),
}

impl fmt::Display for RloxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RloxError::RuntimeError(e) => write!(f, "{}", e),
            RloxError::SyntaxError(e) => write!(f, "{}", e),
            RloxError::ReturnValue(e) => write!(f, "{:?}", e),
        }
    }
}

impl std::error::Error for RloxError {}

// impl From<<f64 as FromStr>::Err> for RloxError {
//     fn from(value: <f64 as FromStr>::Err) -> Self {
//         RloxError::InternalError(value)
//     }
// }

pub struct Rlox {}

impl Rlox {
    pub fn run_file(path: &str) -> Result<(), Box<dyn std::error::Error>> {
        let file = File::open(path)?;
        let mut reader = BufReader::new(file);
        let mut contents = String::new();
        reader.read_to_string(&mut contents)?;
        let mut scanner = Scanner::new(&contents);
        scanner.scan_token().unwrap();

        let mut parser = Parser::new(scanner.tokens);
        let mut resolver = Resolver::new();
        let mut interpreter = Interpreter::new();
        match parser.parse() {
            Ok(mut statements) => {
                // println!("{:?}", &statements);
                match resolver.resolve_stmts(&mut statements) {
                    Ok(_) => {
                        for stmt in statements {
                            interpreter.interpret_stmt(&stmt)?;
                        }
                    }
                    Err(e) => {
                        println!("{}", e);
                    }
                }
            }
            Err(e) => println!("{}", e),
        }
        Ok(())
    }

    pub fn run_prompt() -> io::Result<()> {
        let mut interpreter = Interpreter::new();
        loop {
            let mut input = String::new();
            io::stdin().read_line(&mut input)?;
            let mut scanner = Scanner::new(&input);
            scanner.scan_token().unwrap();
            // println!("{:?}", &scanner.tokens);
            let mut parser = Parser::new(scanner.tokens);
            match parser.parse() {
                Ok(statements) => {
                    for stmt in statements {
                        println!("{:?}", interpreter.interpret_stmt(&stmt));
                    }
                }
                Err(e) => println!("{}", e),
            }
        }
    }

    pub fn syntax_error<T>(line: &usize, message: &str) -> Result<T, RloxError> {
        return Err(RloxError::SyntaxError(format!(
            "[line {}] {}",
            line, message
        )));
    }

    pub fn runtime_error<T>(line: &usize, message: &str) -> Result<T, RloxError> {
        return Err(RloxError::RuntimeError(format!(
            "[line {}] {}",
            line, message
        )));
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() == 1 {
        Rlox::run_prompt()?;
    } else {
        Rlox::run_file(&args[1])?;
    }
    Ok(())
}
