use crate::{
    expr::{Assignment, Binary, Conditional, Expr, Grouping, Literal, Logical, Unary},
    stmt::Stmt,
    token::{Token, TokenType},
    Rlox, RloxError,
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { current: 0, tokens }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, RloxError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            let statement = self.declaration();
            match statement {
                Ok(stmt) => statements.push(stmt),
                Err(e) => {
                    self.synchronize();
                    return Err(e);
                }
            }
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, RloxError> {
        if self.matches(TokenType::Var) {
            return Ok(self.var_declaration()?);
        }
        self.statement()
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_declaration(&mut self) -> Result<Stmt, RloxError> {
        let name = self.consume(TokenType::Identifier, "Expected identifier.")?;

        let mut initializer: Option<Expr> = None;
        if self.matches(TokenType::Equal) {
            initializer = Some(self.expression()?);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration",
        )?;

        Ok(Stmt::Variable(crate::stmt::Variable { name, initializer }))
    }

    // statement → exprStmt | printStmt ;
    fn statement(&mut self) -> Result<Stmt, RloxError> {
        if self.matches(TokenType::Print) {
            return self.print_stmt();
        }
        if self.matches(TokenType::LeftBrace) {
            return self.block();
        }
        if self.matches(TokenType::If) {
            return self.if_stmt();
        }
        if self.matches(TokenType::While) {
            return self.while_stmt();
        }
        if self.matches(TokenType::For) {
            return self.for_stmt();
        }
        self.expression_stmt()
    }

    fn if_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_stmt = Box::new(self.statement()?);
        let mut else_stmt = None;
        if self.matches(TokenType::Else) {
            else_stmt = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If(crate::stmt::If {
            expr: condition,
            then_stmt,
            else_stmt,
        }))
    }

    fn while_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;
        let body = self.statement()?;

        Ok(Stmt::While(crate::stmt::While {
            condition,
            body: Box::new(body),
        }))
    }

    fn for_stmt(&mut self) -> Result<Stmt, RloxError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        let initializer: Option<Stmt>;
        if self.matches(TokenType::Semicolon) {
            initializer = None;
        } else if self.matches(TokenType::Var) {
            initializer = Some(self.var_declaration()?);
        } else {
            initializer = Some(self.expression_stmt()?);
        }

        let mut condition = None;
        if !self.check(TokenType::Semicolon) {
            condition = Some(self.expression()?);
        }
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let mut increment = None;
        if !self.check(TokenType::RightParen) {
            increment = Some(self.expression()?);
        }
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = match body {
                Stmt::Block(mut statements) => {
                    statements.push(Stmt::ExprStmt(inc));
                    Stmt::Block(statements)
                }
                _ => Stmt::Block(vec![body, Stmt::ExprStmt(inc)]),
            };
        }

        if condition.is_none() {
            condition = Some(Expr::Literal(Literal::True));
        }

        if let Some(con) = condition {
            body = Stmt::While(crate::stmt::While {
                condition: con,
                body: Box::new(body),
            });
        }

        if let Some(init) = initializer {
            body = Stmt::Block(vec![init, body]);
        }

        Ok(body)
    }

    fn print_stmt(&mut self) -> Result<Stmt, RloxError> {
        let value = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::PrintStmt(value))
    }

    fn block(&mut self) -> Result<Stmt, RloxError> {
        let mut statments = Vec::new();
        while !self.is_at_end() && self.current_token().token_type != TokenType::RightBrace {
            statments.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(Stmt::Block(statments))
    }

    // exprStmt → expression ";" ;
    fn expression_stmt(&mut self) -> Result<Stmt, RloxError> {
        let expression = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::ExprStmt(expression))
    }

    fn expression(&mut self) -> Result<Expr, RloxError> {
        let expr = self.assignment()?;
        Ok(expr)
    }

    // assignment → IDENTIFIER "=" assignment | equality ;
    fn assignment(&mut self) -> Result<Expr, RloxError> {
        let left = self.or()?;
        if self.matches(TokenType::Equal) {
            let equals = self.previous_token();
            let value = self.assignment()?;

            match left {
                Expr::Variable(v) => {
                    return Ok(Expr::Assignment(Assignment {
                        name: v.name,
                        value: Box::new(value),
                    }))
                }
                _ => return Rlox::syntax_error(&equals.line, "Invalid assignment target."),
            }
        };
        Ok(left)
    }

    fn or(&mut self) -> Result<Expr, RloxError> {
        let mut expr = self.and()?;
        while self.matches(TokenType::Or) {
            let token = self.previous_token();
            let right = Box::new(self.and()?);
            expr = Expr::Logical(Logical {
                token,
                left: Box::new(expr),
                right,
            });
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, RloxError> {
        let mut expr = self.conditional()?;
        while self.matches(TokenType::And) {
            let token = self.previous_token();
            let right = Box::new(self.conditional()?);
            expr = Expr::Logical(Logical {
                token,
                left: Box::new(expr),
                right,
            });
        }
        Ok(expr)
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
            let operator = self.previous_token();
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
            let operator = self.previous_token();
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
            let operator = self.previous_token();
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
            let operator = self.previous_token();
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
            let operator = self.previous_token();
            let right = Box::new(self.unary()?);
            let expression = Expr::Unary(Unary { right, operator });
            return Ok(expression);
        }
        Ok(self.primary()?)
    }

    // primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
    fn primary(&mut self) -> Result<Expr, RloxError> {
        if self.matches(TokenType::Number) {
            match self.previous_token().literal {
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
            match self.previous_token().literal {
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
        if self.matches(TokenType::Identifier) {
            return Ok(Expr::Variable(crate::expr::Variable {
                name: self.previous_token(),
            }));
        }
        return Rlox::syntax_error(&self.current_token().line, "Expected expression.");
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

    fn consume(&mut self, token_type: TokenType, error_message: &str) -> Result<Token, RloxError> {
        if self.check(token_type) {
            return Ok(self.advance());
        }
        return Rlox::syntax_error(&self.current_token().line, error_message);
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return self.current_token().token_type == token_type;
    }

    fn is_at_end(&self) -> bool {
        if self.current_token().token_type == TokenType::Eof {
            return true;
        }
        return false;
    }

    fn advance(&mut self) -> Token {
        self.current += 1;
        return self.previous_token();
    }

    fn current_token(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous_token(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    fn synchronize(&mut self) {
        while self.current < self.tokens.len() {
            match self.current_token().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => break,
                _ => (),
            }
            self.advance();
        }
    }
}
