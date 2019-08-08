use crate::token::Token;

trait Node {
    fn token_literal(&self) -> String;
}
trait Statement: Node {
    fn statement_node(&self);
}

trait Expression: Node {
    fn expression_node(&self);
}

struct Program {
    statements: Vec<Box<Statement>>,
}

impl Program {
    pub fn new() -> Self {
        Program { statements: vec![] }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            String::new()
        }
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token) -> Self {
        let value = token.literal().to_string();
        Identifier {
            value,
            token: token,
        }
    }
}

impl Statement for Identifier {
    fn statement_node(&self) {}
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal().to_string()
    }
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl Statement for LetStatement {
    fn statement_node(&self) {}
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal().to_string()
    }
}
