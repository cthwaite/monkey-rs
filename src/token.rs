#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    //
    Illegal,
    EOF,
    Ident(String),
    Int(String),
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

impl Token {
    pub fn make_int(int: &str) -> Token {
        Token::Int(int.to_owned())
    }
    pub fn make_ident(ident: &str) -> Token {
        Token::Ident(ident.to_string())
    }
    pub fn lookup_ident(ident: &str) -> Token {
        match ident {
            "let" => Token::Let,
            "fn" => Token::Function,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Ident(ident.to_string()),
        }
    }
    pub fn literal(&self) -> &str {
        match self {
            Token::Illegal => "",
            Token::EOF => "",
            Token::Ident(literal) => &literal,
            Token::Int(literal) => &literal,
            // Operators
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Bang => "!",
            Token::Asterisk => "*",
            Token::Slash => "/",

            Token::Eq => "==",
            Token::NotEq => "!=",

            Token::Lt => "<",
            Token::Gt => ">",
            // Delimiters
            Token::Comma => ",",
            Token::Semicolon => ";",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            // Keywords
            Token::Function => "fn",
            Token::Let => "let",
            Token::If => "if",
            Token::Else => "else",
            Token::Return => "return",
            Token::True => "true",
            Token::False => "false",
        }
    }
}
