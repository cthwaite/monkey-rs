use crate::token::Token;
use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(ident: &str) -> Self {
        Identifier(ident.to_owned())
    }
}
impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Nothing,
}
impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(name) => write!(f, "{}", name),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    LetStatement {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    ReturnStatement {
        token: Token,
        expr: Expression,
    },
}

impl Statement {
    pub fn token_literal(&self) -> &str {
        match self {
            Statement::LetStatement { token, .. } => token.literal(),
            Statement::ReturnStatement { token, .. } => token.literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::LetStatement { token, name, value } => {
                write!(f, "{} {}", token.literal(), name)?;
                match value {
                    Expression::Nothing => (),
                    _ => write!(f, " = {}", value)?,
                }
                write!(f, ";")
            }
            Statement::ReturnStatement { token, expr } => {
                write!(f, "{}", token.literal())?;
                match expr {
                    Expression::Nothing => (),
                    _ => write!(f, " {}", expr)?,
                }
                write!(f, ";")
            }
        }
    }
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}

impl Program {
    pub fn token_literal(&self) -> Option<&str> {
        self.statements
            .first()
            .and_then(|stmt| Some(stmt.token_literal()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_display_let_statement() {
        let stmt = Statement::LetStatement {
            token: Token::Let,
            name: Identifier("x".to_string()),
            value: Expression::Nothing,
        };
        assert_eq!(format!("{}", stmt), "let x;");
    }

    #[test]
    fn test_display_return_statement() {
        let stmt = Statement::ReturnStatement {
            token: Token::Return,
            expr: Expression::Nothing,
        };
        assert_eq!(format!("{}", stmt), "return;");
    }
}
