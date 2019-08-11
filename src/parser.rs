use crate::ast::{Expression, Identifier, Program, Statement};
use crate::lexer::Lexer;
use std::fmt::{self, Display};

use crate::token::Token;

type PrefixParseFn = Fn() -> Expression;
type InfixParseFn = Fn(Expression) -> Expression;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGrater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Clone, Debug)]
pub enum ParserError {
    ExpectedToken { expected: Token, saw: Token },
    ExpectedIdent(Token),
    IntegerParseFailure(String),
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
            ParserError::IntegerParseFailure(expr) => {
                write!(f, "Could not parse {} as integer", expr)
            }
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
        let mut program = Program::default();

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
            _ => self.parse_expression_statement(),
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
        Some(Statement::Let {
            token,
            name,
            value: Expression::Nothing,
        })
    }

    pub fn parse_return_statement(&mut self) -> Option<Statement> {
        let stmt = Some(Statement::Return {
            token: self.cur_token.clone(),
            expr: Expression::Nothing,
        });
        self.next_token();
        while !self.current_token_is(&Token::Semicolon) {
            self.next_token();
        }
        stmt
    }

    pub fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = match self.parse_expression(Precedence::Lowest) {
            Some(expr) => expr,
            None => return None,
        };
        let stmt = Statement::Expression {
            token: self.cur_token.clone(),
            expr,
        };

        if self.peek_token_is(&Token::Semicolon) {
            self.next_token()
        }
        Some(stmt)
    }

    pub fn parse_expression(&mut self, _precendence: Precedence) -> Option<Expression> {
        match &self.cur_token {
            Token::Ident(name) => Some(Expression::Identifier(Identifier::new(name))),
            Token::Int(value_str) => {
                let value: i64 = match value_str.parse::<i64>() {
                    Ok(value) => value,
                    Err(_) => {
                        self.errors
                            .push(ParserError::IntegerParseFailure(value_str.to_owned()));
                        return None;
                    }
                };
                Some(Expression::IntegerLiteral(value))
            }
            _ => None,
        }
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

    /// Assert that a Parser contains no errors.
    fn assert_no_parser_errors(parser: &Parser) {
        let errors = parser.errors();
        assert!(
            errors.is_empty(),
            "Parser has {} errors: {:?}",
            errors.len(),
            errors
        )
    }

    /// Assert that a Program contains a certain number of staements.
    fn assert_program_statements_len(program: &Program, count: usize) {
        assert_eq!(
            program.statements.len(),
            count,
            "program.statements does not contain {} statement{}: got {}",
            count,
            if count >= 1 { "s" } else { "" },
            program.statements.len()
        );
    }

    #[test]
    fn test_let_statements() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "#;

        let (parser, program) = parser_for_input(input);
        assert_no_parser_errors(&parser);
        assert_program_statements_len(&program, 3);
        let expected_names = vec!["x", "y", "foobar"];
        for (expected_identifier, stmt) in expected_names.iter().zip(program.statements.iter()) {
            match stmt {
                Statement::Let { token, name, .. } => {
                    assert_eq!(token, &Token::Let);
                    assert_eq!(name.0, *expected_identifier);
                }
                _ => assert!(false, "Expected Statement::Let, got {:?}", stmt),
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
        assert_no_parser_errors(&parser);
        assert_program_statements_len(&program, 3);
        let values = vec!["5", "10", "993322"];
        for (_expected_identifier, stmt) in values.iter().zip(program.statements.iter()) {
            match stmt {
                Statement::Return { token, .. } => {
                    assert_eq!(token, &Token::Return);
                }
                _ => assert!(false, "Expected ReturnStatement, got {:?}", stmt),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let (parser, program) = parser_for_input(input);
        assert_no_parser_errors(&parser);
        assert_program_statements_len(&program, 1);

        match &program.statements[0] {
            Statement::Expression {
                expr: Expression::Identifier(Identifier(name)),
                ..
            } => {
                assert_eq!(name, "foobar");
            }
            _ => assert!(
                false,
                "Expected Statement::Expression(Identifier), got {:?}",
                program.statements[0]
            ),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let (parser, program) = parser_for_input(input);
        assert_no_parser_errors(&parser);
        assert_program_statements_len(&program, 1);

        match &program.statements[0] {
            Statement::Expression {
                expr: Expression::IntegerLiteral(value),
                ..
            } => {
                assert_eq!(*value, 5);
            }
            _ => assert!(
                false,
                "Expected Statement::Expression(IntegerLiteral), got {:?}",
                program.statements[0]
            ),
        }
    }
}
