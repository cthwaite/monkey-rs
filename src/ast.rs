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
    IntegerLiteral(i64),
    Boolean(bool),
    Prefix {
        operator: Token,
        right: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Nothing,
}

impl From<bool> for Expression {
    fn from(b: bool) -> Self {
        Expression::Boolean(b)
    }
}

impl From<i64> for Expression {
    fn from(i: i64) -> Self {
        Expression::IntegerLiteral(i)
    }
}

impl From<Identifier> for Expression {
    fn from(i: Identifier) -> Self {
        Expression::Identifier(i)
    }
}

impl Expression {
    pub fn new_infix<E: Into<Expression>>(left: E, operator: Token, right: E) -> Self {
        Expression::Infix {
            left: Box::new(left.into()),
            operator,
            right: Box::new(right.into()),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Identifier(name) => write!(f, "{}", name),
            Expression::IntegerLiteral(value) => write!(f, "{}", value),
            Expression::Boolean(value) => write!(f, "{}", value),
            Expression::Prefix { operator, right } => {
                write!(f, "({}{})", operator.literal(), right)
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alternative) = alternative {
                    write!(f, "else {}", alternative)?;
                }
                Ok(())
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator.literal(), right),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        token: Token,
        name: Identifier,
        value: Expression,
    },
    Return {
        token: Token,
        expr: Expression,
    },
    Expression {
        token: Token,
        expr: Expression,
    },
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { token, name, value } => {
                write!(f, "{} {}", token.literal(), name)?;
                match value {
                    Expression::Nothing => (),
                    _ => write!(f, " = {}", value)?,
                }
                write!(f, ";")
            }
            Statement::Return { token, expr } => {
                write!(f, "{}", token.literal())?;
                match expr {
                    Expression::Nothing => (),
                    _ => write!(f, " {}", expr)?,
                }
                write!(f, ";")
            }
            Statement::Expression { expr, .. } => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new(token: Token) -> Self {
        BlockStatement {
            token,
            statements: vec![],
        }
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    /// Get the number of statements in the program.
    pub fn len(&self) -> usize {
        self.statements.len()
    }
    /// Check if the program is empty.
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}\n", stmt)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_display_let_statement() {
        let stmt = Statement::Let {
            token: Token::Let,
            name: Identifier::new("x"),
            value: Expression::Identifier(Identifier::new("y")),
        };
        assert_eq!(format!("{}", stmt), "let x = y;");
    }

    #[test]
    fn test_display_return_statement() {
        let stmt = Statement::Return {
            token: Token::Return,
            expr: Expression::Nothing,
        };
        assert_eq!(format!("{}", stmt), "return;");
    }

    #[test]
    fn test_display_program() {
        let prog = Program {
            statements: vec![
                Statement::Let {
                    token: Token::Let,
                    name: Identifier::new("x"),
                    value: Expression::Identifier(Identifier::new("y")),
                },
                Statement::Return {
                    token: Token::Return,
                    expr: Expression::Nothing,
                },
            ],
        };
        assert_eq!(format!("{}", prog), "let x = y;\nreturn;\n");
    }

    #[test]
    fn test_display_prefix_expression() {
        let stmt = Expression::Prefix {
            operator: Token::Minus,
            right: Box::new(Expression::IntegerLiteral(5)),
        };
        assert_eq!(format!("{}", stmt), "(-5)");
    }

    #[test]
    fn test_display_infix_expression() {
        let stmt = Expression::Infix {
            left: Box::new(Expression::IntegerLiteral(5)),
            operator: Token::Plus,
            right: Box::new(Expression::IntegerLiteral(5)),
        };
        assert_eq!(format!("{}", stmt), "(5 + 5)");
    }
}
