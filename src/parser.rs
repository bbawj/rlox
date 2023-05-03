use crate::{
    expr::{Binary, Conditional, Expr, Grouping, Literal, Unary},
    stmt::Stmt,
    token::{Token, TokenType},
    RloxError,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Result<Expr, RloxError> {
        // let statements = Vec::new();
        // while self.current < self.tokens.len() {
        //     statements.push(self.declaration())
        // }
        Ok(self.expression()?)
    }

    fn declaration(&self) -> Result<(), RloxError> {
        Ok(())
    }

    fn expression(&mut self) -> Result<Expr, RloxError> {
        Ok(self.conditional()?)
    }

    fn conditional(&mut self) -> Result<Expr, RloxError> {
        let mut expression = self.equality()?;
        if self.matches(TokenType::Question) {
            let then_part = Box::new(self.expression()?);
            if !self.matches(TokenType::Colon) {
                return Err(RloxError::SyntaxError(
                    "Expect ':' after then branch of conditional expression.".to_string(),
                ));
            }
            let else_part = Box::new(self.equality()?);
            expression = Expr::Conditional(Conditional {
                expression: Box::new(expression),
                then_part,
                else_part,
            })
        }
        Ok(expression)
    }

    // equality → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> Result<Expr, RloxError> {
        let mut expression = self.comparison()?;
        while self.matches_one_of(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator = self.previous();
            let right = Box::new(self.comparison()?);
            expression = Expr::Binary(Binary {
                left: Box::new(expression),
                right,
                operator,
            })
        }
        Ok(expression)
    }
    // comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    fn comparison(&mut self) -> Result<Expr, RloxError> {
        let mut expression = self.term()?;
        while self.matches_one_of(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = Box::new(self.term()?);
            expression = Expr::Binary(Binary {
                left: Box::new(expression),
                right,
                operator,
            })
        }
        Ok(expression)
    }

    // term → factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, RloxError> {
        let mut expression = self.factor()?;
        while self.matches_one_of(vec![TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = Box::new(self.factor()?);
            expression = Expr::Binary(Binary {
                left: Box::new(expression),
                right,
                operator,
            });
        }
        Ok(expression)
    }
    //factor → unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, RloxError> {
        let mut expression = self.unary()?;
        while self.matches_one_of(vec![TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = Box::new(self.unary()?);
            expression = Expr::Binary(Binary {
                left: Box::new(expression),
                right,
                operator,
            })
        }
        Ok(expression)
    }
    // unary → ( "!" | "-" ) unary | primary ;
    fn unary(&mut self) -> Result<Expr, RloxError> {
        if self.matches_one_of(vec![TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = Box::new(self.unary()?);
            let expression = Expr::Unary(Unary { right, operator });
            return Ok(expression);
        }
        Ok(self.primary()?)
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, RloxError> {
        if self.matches(TokenType::Number) {
            match self.previous().literal {
                Some(crate::token::Literal::Number(float)) => {
                    return Ok(Expr::Literal(Literal::Number(float)))
                }
                Some(other) => panic!(
                    "Expected float while parsing primary number. Got {:?}",
                    other
                ),
                None => panic!("Expected float while parsing primary number. Got None"),
            }
        }
        if self.matches(TokenType::True) {
            return Ok(Expr::Literal(Literal::True));
        }
        if self.matches(TokenType::False) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.matches(TokenType::Nil) {
            return Ok(Expr::Literal(Literal::False));
        }
        if self.matches(TokenType::String) {
            match self.previous().literal {
                Some(crate::token::Literal::String(string)) => {
                    return Ok(Expr::Literal(Literal::String(string)))
                }
                Some(other) => panic!(
                    "Expected string while parsing primary string. Got {:?}",
                    other
                ),
                None => panic!("Expected string while parsing primary string. Got None"),
            }
        }
        if self.matches(TokenType::LeftParen) {
            let expression = self.expression()?;
            if !self.matches(TokenType::RightParen) {
                return Err(RloxError::SyntaxError(
                    "Expected ')' after expression.".to_string(),
                ));
            }
            return Ok(Expr::Grouping(Grouping {
                expression: Box::new(expression),
            }));
        }
        return Err(RloxError::SyntaxError("Expected expression.".to_string()));
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        let token = &self.tokens[self.current];
        if token.token_type == token_type {
            self.advance();
            return true;
        }
        return false;
    }

    fn matches_one_of(&mut self, token_types: Vec<TokenType>) -> bool {
        let token = &self.tokens[self.current];
        for token_type in token_types {
            if token.token_type == token_type {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }
}
