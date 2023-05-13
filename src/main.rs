mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;
mod token;

use std::{
    fmt,
    fs::File,
    io::{self, BufReader, Read},
};

use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;

#[derive(Debug)]
pub enum RloxError {
    RuntimeError(String),
    SyntaxError(String),
    // InternalError(Box<dyn std::error::Error>),
}

impl fmt::Display for RloxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RloxError::RuntimeError(e) => write!(f, "{}", e),
            RloxError::SyntaxError(e) => write!(f, "{}", e),
        }
    }
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
        scanner.scan_token().unwrap();

        let mut parser = Parser::new(scanner.tokens);
        let mut interpreter = Interpreter::new();
        match parser.parse() {
            Ok(statements) => {
                // println!("{:?}", &statements);
                for stmt in statements {
                    println!("{:?}", interpreter.interpret_stmt(&stmt));
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
