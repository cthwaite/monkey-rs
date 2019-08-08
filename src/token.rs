#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenType {
    //
    Illegal,
    EOF,
    Ident,
    Int,
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Eq,
    NotEq,

    Lt,
    Gt,
    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

impl TokenType {}

#[derive(Debug, Clone)]
pub struct Token {
    pub typ: TokenType,
    pub literal: String,
}

impl Token {
    pub fn new(typ: TokenType, literal: &str) -> Self {
        Token {
            typ,
            literal: literal.to_owned(),
        }
    }
    pub fn new_illegal() -> Self {
        Token {
            typ: TokenType::Illegal,
            literal: String::new(),
        }
    }
    pub fn from_char(typ: TokenType, literal: char) -> Self {
        Token {
            typ,
            literal: literal.to_string(),
        }
    }
}
