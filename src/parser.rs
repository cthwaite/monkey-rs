use crate::ast::{Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            cur_token,
            peek_token,
        }
    }

    pub fn next_token(&mut self) {
        std::mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn current_token_is(&self, tok: Token) -> bool {
        tok == self.cur_token
    }

    pub fn peek_token_is(&self, tok: Token) -> bool {
        tok == self.peek_token
    }

    pub fn expect_peek(&mut self, tok: Token) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            return true;
        }
        false
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while self.cur_token != Token::EOF {
            println!(
                "cur_token: {:?} / peek_token: {:?}",
                self.cur_token, self.peek_token
            );
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        Some(program)
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            // Token::Return => self.parse_return_statement(),
            // Token::If => self.parse_if_statement(),
            _ => None,
        }
    }

    pub fn expect_ident(&mut self) -> Option<Identifier> {
        match &self.peek_token {
            Token::Ident(name) => {
                let ident = Identifier::new(name);
                self.next_token();
                Some(ident)
            }
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        let name = match self.expect_ident() {
            Some(name) => name,
            None => return None,
        };
        if !self.expect_peek(Token::Assign) {
            return None;
        }
        while !self.current_token_is(Token::Semicolon) {
            self.next_token();
        }
        Some(Statement::LetStatement {
            token,
            name,
            value: Expression::Dummy,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_some(), "parse_program() returned None");
        let program = program.unwrap();
        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements: got {}",
            program.statements.len()
        );
        let tests = vec!["x", "y", "foobar"];
        for (expected_identifier, stmt) in tests.iter().zip(program.statements.iter()) {
            match stmt {
                Statement::LetStatement { token, name, .. } => {
                    assert_eq!(token, &Token::Let);
                    assert_eq!(name.0, *expected_identifier);
                }
                _ => assert!(false, "Expected LetStatement, got {:?}", stmt),
            }
        }
    }
}
