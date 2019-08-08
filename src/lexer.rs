use crate::token::Token;
use std::iter::Peekable;
use std::str::Chars;

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

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    iterator: Peekable<Chars<'a>>,
    ch: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let iterator = input.chars().peekable();
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            iterator,
        };
        lexer.read_char();
        lexer
    }

    /// Read a character and advance the read position.
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = self.iterator.next();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.ch {
            if !ch.is_whitespace() {
                break;
            }
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> &str {
        let position = self.position;

        while let Some(ch) = self.ch {
            if ch.is_alphabetic() || ch == '_' {
                self.read_char();
            } else {
                break;
            }
        }

        &self.input[position..self.position]
    }

    fn read_number(&mut self) -> &str {
        let position = self.position;

        fn is_numeric(ch: Option<char>) -> bool {
            if let Some(ch) = ch {
                ch.is_digit(10)
            } else {
                false
            }
        }

        while is_numeric(self.ch) {
            self.read_char();
        }

        &self.input[position..self.position]
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.iterator.peek()
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            Some('=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            Some('*') => Token::Asterisk,
            Some('/') => Token::Slash,

            Some('<') => Token::Lt,
            Some('>') => Token::Gt,

            Some('(') => Token::LParen,
            Some(')') => Token::RParen,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(',') => Token::Comma,
            Some(';') => Token::Semicolon,
            Some(ch) => {
                if ch.is_alphabetic() {
                    return Token::lookup_ident(self.read_identifier());
                } else if ch.is_digit(10) {
                    return Token::make_int(self.read_number());
                } else {
                    return Token::Illegal;
                }
            }
            None => Token::EOF,
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_output_tokens(input: &str, expected_output: Vec<Token>) {
        let mut lexer = Lexer::new(input);

        for (idx, expected_token) in expected_output.into_iter().enumerate() {
            let lexed_token = lexer.next_token();
            assert_eq!(
                expected_token,
                lexed_token,
                "[{}] Expected ({:?} {:?}) => Got ({:?} {:?})",
                idx,
                expected_token,
                expected_token.literal(),
                lexed_token,
                lexed_token.literal()
            );
        }
    }

    #[test]
    fn test_eq_op_tokens() {
        let input = r#"
        10 == 10;

        10 != 9;
        "#;
        let expected_output = vec![
            Token::make_int("10"),
            Token::Eq,
            Token::make_int("10"),
            Token::Semicolon,
            Token::make_int("10"),
            Token::NotEq,
            Token::make_int("9"),
            Token::Semicolon,
            Token::EOF,
        ];
        check_output_tokens(input, expected_output);
    }

    #[test]
    fn test_assignment() {
        let input = r#"

        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
        "#;
        let expected_output = vec![
            Token::Let,
            Token::make_ident("five"),
            Token::Assign,
            Token::make_int("5"),
            Token::Semicolon,
            Token::Let,
            Token::make_ident("ten"),
            Token::Assign,
            Token::make_int("10"),
            Token::Semicolon,
            Token::Let,
            Token::make_ident("add"),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::make_ident("x"),
            Token::Comma,
            Token::make_ident("y"),
            Token::RParen,
            Token::LBrace,
            Token::make_ident("x"),
            Token::Plus,
            Token::make_ident("y"),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::make_ident("result"),
            Token::Assign,
            Token::make_ident("add"),
            Token::LParen,
            Token::make_ident("five"),
            Token::Comma,
            Token::make_ident("ten"),
            Token::RParen,
            Token::Semicolon,
            Token::EOF,
        ];
        check_output_tokens(input, expected_output);
    }

    #[test]
    fn test_next_token() {
        let input = r#"

        !-/*5;

        5 < 10 > 5;
        if (5 < 10) {
            return true;
        } else {
            return false;
        }
        "#;
        let expected_output = vec![
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::make_int("5"),
            Token::Semicolon,
            Token::make_int("5"),
            Token::Lt,
            Token::make_int("10"),
            Token::Gt,
            Token::make_int("5"),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::make_int("5"),
            Token::Lt,
            Token::make_int("10"),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::EOF,
        ];
        check_output_tokens(input, expected_output);
    }
}
