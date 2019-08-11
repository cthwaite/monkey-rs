use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

impl Identifier {
    pub fn new(ident: &str) -> Self {
        Identifier(ident.to_owned())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Dummy,
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
