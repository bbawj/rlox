use std::collections::HashMap;

use crate::{
    token::{Token, TokenType},
    RloxError,
};

pub struct Scanner<'a> {
    source: &'a str,
    pub tokens: Vec<Token>,
    line: usize,
    keywords: HashMap<String, TokenType>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("and".to_string(), TokenType::And);
        keywords.insert("class".to_string(), TokenType::Class);
        keywords.insert("else".to_string(), TokenType::Else);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("for".to_string(), TokenType::For);
        keywords.insert("fun".to_string(), TokenType::Fun);
        keywords.insert("if".to_string(), TokenType::If);
        keywords.insert("nil".to_string(), TokenType::Nil);
        keywords.insert("or".to_string(), TokenType::Or);
        keywords.insert("print".to_string(), TokenType::Print);
        keywords.insert("return".to_string(), TokenType::Return);
        keywords.insert("super".to_string(), TokenType::Super);
        keywords.insert("this".to_string(), TokenType::This);
        keywords.insert("true".to_string(), TokenType::True);
        keywords.insert("var".to_string(), TokenType::Var);
        keywords.insert("while".to_string(), TokenType::While);

        Self {
            source,
            tokens: Vec::new(),
            line: 0,
            keywords,
        }
    }

    pub fn scan_token(&mut self) -> Result<(), RloxError> {
        let mut iter = self.source.chars().peekable();
        loop {
            let mut text = String::new();
            let c = iter.next();

            if c.is_none() {
                self.add_token(TokenType::Eof, "".to_string());
                return Ok(());
            }

            if let Some(character) = c {
                text.push(character);

                match character {
                    '(' => self.add_token(TokenType::LeftParen, text),
                    ')' => self.add_token(TokenType::RightParen, text),
                    '{' => self.add_token(TokenType::LeftBrace, text),
                    '}' => self.add_token(TokenType::RightBrace, text),
                    ',' => self.add_token(TokenType::Comma, text),
                    '.' => self.add_token(TokenType::Dot, text),
                    '-' => self.add_token(TokenType::Minus, text),
                    '+' => self.add_token(TokenType::Plus, text),
                    ';' => self.add_token(TokenType::Semicolon, text),
                    '*' => self.add_token(TokenType::Star, text),
                    '?' => self.add_token(TokenType::Question, text),
                    ':' => self.add_token(TokenType::Colon, text),
                    '!' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            self.add_token(TokenType::BangEqual, text);
                        } else {
                            self.add_token(TokenType::Bang, text)
                        }
                    }
                    '=' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            self.add_token(TokenType::EqualEqual, text);
                        } else {
                            self.add_token(TokenType::Equal, text)
                        }
                    }
                    '>' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            self.add_token(TokenType::GreaterEqual, text);
                        } else {
                            self.add_token(TokenType::Greater, text)
                        }
                    }
                    '<' => {
                        let next = iter.next_if_eq(&'=');
                        if next.is_some() {
                            text.push(next.unwrap());
                            self.add_token(TokenType::LessEqual, text);
                        } else {
                            self.add_token(TokenType::Less, text)
                        }
                    }
                    '/' => {
                        let next = iter.next_if_eq(&'/');
                        if next.is_some() {
                            while iter.next_if(|&x| x != '\n').is_some() {}
                            //discard the newline
                            iter.next();
                        } else {
                            self.add_token(TokenType::Slash, text)
                        }
                    }
                    ' ' | '\r' | '\t' => (),
                    '\n' => self.line = &self.line + 1,
                    '"' => {
                        loop {
                            // end of file
                            if iter.peek().is_none() {
                                return Err(RloxError::SyntaxError(
                                    "Unterminated string.".to_string(),
                                ));
                            }

                            let next = iter.next_if(|&x| x != '"');

                            match next {
                                Some(ch) => text.push(ch),
                                None => {
                                    self.add_token(TokenType::String, text);
                                    // discard the '"'
                                    iter.next();
                                    break;
                                }
                            }
                        }
                    }
                    _ => {
                        if character.is_digit(10) {
                            while let Some(ch) = iter.next_if(|x| x.is_digit(10)) {
                                text.push(ch);
                            }

                            // fractional
                            let peek = iter.peek();
                            match peek {
                                Some(ch) => {
                                    if ch == &'.' {
                                        // ensure that after '.' is digits
                                        let mut iter_clone = iter.clone();
                                        // discard '.'
                                        iter_clone.next();
                                        let next = iter_clone.peek();
                                        match next {
                                            Some(x) => {
                                                if x.is_digit(10) {
                                                    if let Some(dot) = iter.next() {
                                                        text.push(dot);
                                                    }

                                                    while let Some(ch) =
                                                        iter.next_if(|x| x.is_digit(10))
                                                    {
                                                        text.push(ch);
                                                    }
                                                }
                                            }
                                            None => (),
                                        }
                                    }
                                }
                                None => (),
                            }
                            self.add_token_with_literal(
                                TokenType::Number,
                                text.clone(),
                                crate::token::Literal::Number(
                                    text.parse::<f64>().expect("Failed parsing string to f64."),
                                ),
                            );
                        } else if character.is_alphanumeric() {
                            while let Some(ch) = iter.next_if(|x| x.is_alphanumeric()) {
                                text.push(ch);
                            }

                            let keyword = self.keywords.get(&text.to_string());
                            match keyword {
                                Some(k) => self.add_token_with_literal(
                                    *k,
                                    text.clone(),
                                    crate::token::Literal::String(text.to_string()),
                                ),
                                None => self.add_token(TokenType::Identifier, text),
                            }
                        } else {
                            return Err(RloxError::SyntaxError(
                                "Unexpected character.".to_string(),
                            ));
                        }
                    }
                }
            }
        }
    }

    fn add_token(&mut self, token_type: TokenType, text: String) {
        self.tokens
            .push(Token::new(token_type, text, None, self.line))
    }

    fn add_token_with_literal(
        &mut self,
        token_type: TokenType,
        text: String,
        literal: crate::token::Literal,
    ) {
        self.tokens
            .push(Token::new(token_type, text, Some(literal), self.line))
    }
}
