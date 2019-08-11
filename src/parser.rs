use crate::ast::{Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use std::fmt::{self, Display};

use crate::token::Token;

#[derive(Clone, Debug)]
pub enum ParserError {
    ExpectedToken { expected: Token, saw: Token },
    ExpectedIdent(Token),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::ExpectedToken { expected, saw } => write!(
                f,
                "Expected next token to be {:?}, got {:?} instead",
                expected, saw
            ),
            ParserError::ExpectedIdent(token) => write!(
                f,
                "Expected next token to be Ident, got {:?} instead",
                token
            ),
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    errors: Vec<ParserError>,
    cur_token: Token,
    peek_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            errors: vec![],
            cur_token,
            peek_token,
        }
    }

    pub fn errors(&self) -> &[ParserError] {
        &self.errors
    }

    pub fn peek_error(&mut self, expected: &Token) {
        self.errors.push(ParserError::ExpectedToken {
            expected: expected.clone(),
            saw: self.peek_token.clone(),
        });
    }

    pub fn next_token(&mut self) {
        std::mem::swap(&mut self.cur_token, &mut self.peek_token);
        self.peek_token = self.lexer.next_token();
    }

    pub fn current_token_is(&self, tok: &Token) -> bool {
        *tok == self.cur_token
    }

    pub fn peek_token_is(&self, tok: &Token) -> bool {
        *tok == self.peek_token
    }

    pub fn expect_peek(&mut self, tok: &Token) -> bool {
        if self.peek_token_is(&tok) {
            self.next_token();
            return true;
        }
        self.peek_error(&tok);
        false
    }

    pub fn expect_ident(&mut self) -> Option<Identifier> {
        match &self.peek_token {
            Token::Ident(name) => {
                let ident = Identifier::new(name);
                self.next_token();
                Some(ident)
            }
            _ => {
                self.errors
                    .push(ParserError::ExpectedIdent(self.peek_token.clone()));
                None
            }
        }
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while self.cur_token != Token::EOF {
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
            Token::Return => self.parse_return_statement(),
            // Token::If => self.parse_if_statement(),
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();

        let name = match self.expect_ident() {
            Some(name) => name,
            None => return None,
        };
        if !self.expect_peek(&Token::Assign) {
            return None;
        }
        while !self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }
        Some(Statement::LetStatement {
            token,
            name,
            value: Expression::Dummy,
        })
    }

    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        let stmt = Some(Statement::ReturnStatement {
            token: self.cur_token.clone(),
            expr: Expression::Dummy,
        });
        self.next_token();
        while !self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }
        stmt
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Construct a parser to parser the input, returning the parser and parsed
    /// Program object.
    fn parser_for_input(input: &str) -> (Parser, Program) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(program.is_some(), "parse_program() returned None");
        (parser, program.unwrap())
    }

    fn check_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        assert!(
            errors.is_empty(),
            "Parser has {} errors: {:?}",
            errors.len(),
            errors
        )
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let (parser, program) = parser_for_input(input);
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements: got {}",
            program.statements.len()
        );
        let expected_names = vec!["x", "y", "foobar"];
        for (expected_identifier, stmt) in expected_names.iter().zip(program.statements.iter()) {
            match stmt {
                Statement::LetStatement { token, name, .. } => {
                    assert_eq!(token, &Token::Let);
                    assert_eq!(name.0, *expected_identifier);
                }
                _ => assert!(false, "Expected LetStatement, got {:?}", stmt),
            }
        }
    }

    #[test]
    fn test_invalid_let_statements() {
        let input = r#"
        let x 5;
        let = 10;
        let 838383;
        "#;

        let (parser, _program) = parser_for_input(input);
        let errors = parser.errors();
        assert_eq!(errors.len(), 3);

        assert!(
            match &errors[0] {
                ParserError::ExpectedToken { expected, saw } => {
                    assert_eq!(expected, &Token::Assign);
                    assert_eq!(saw, &Token::make_int("5"));
                    true
                }
                _ => false,
            },
            "Expected ParserError::ExpectedToken, saw {:?}",
            errors[0]
        );

        assert!(
            match &errors[1] {
                ParserError::ExpectedIdent(saw) => {
                    assert_eq!(saw, &Token::Assign);
                    true
                }
                _ => false,
            },
            "Expected ParserError::ExpectedIdent, saw {:?}",
            errors[1]
        );

        assert!(
            match &errors[2] {
                ParserError::ExpectedIdent(saw) => {
                    assert_eq!(saw, &Token::make_int("838383"));
                    true
                }
                _ => false,
            },
            "Expected ParserError::ExpectedIdent, saw {:?}",
            errors[2]
        );
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
        return 5;
        return 10;
        return 993322;
        "#;

        let (parser, program) = parser_for_input(input);
        check_parser_errors(&parser);

        assert_eq!(
            program.statements.len(),
            3,
            "program.statements does not contain 3 statements: got {}",
            program.statements.len()
        );
        let values = vec!["5", "10", "993322"];
        for (_expected_identifier, stmt) in values.iter().zip(program.statements.iter()) {
            match stmt {
                Statement::ReturnStatement { token, .. } => {
                    assert_eq!(token, &Token::Return);
                }
                _ => assert!(false, "Expected ReturnStatement, got {:?}", stmt),
            }
        }
    }
}
