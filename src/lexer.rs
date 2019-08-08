use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

pub fn lookup_ident(ident: &str) -> TokenType {
    match ident {
        "let" => TokenType::Let,
        "fn" => TokenType::Function,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "true" => TokenType::True,
        "false" => TokenType::False,
        _ => TokenType::Ident,
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
            Some(ch @ '=') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::new(TokenType::Eq, "==")
                } else {
                    Token::from_char(TokenType::Assign, ch)
                }
            }
            Some(ch @ '+') => Token::from_char(TokenType::Plus, ch),
            Some(ch @ '-') => Token::from_char(TokenType::Minus, ch),
            Some(ch @ '!') => {
                if let Some('=') = self.peek_char() {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=")
                } else {
                    Token::from_char(TokenType::Bang, ch)
                }
            }
            Some(ch @ '*') => Token::from_char(TokenType::Asterisk, ch),
            Some(ch @ '/') => Token::from_char(TokenType::Slash, ch),

            Some(ch @ '<') => Token::from_char(TokenType::Lt, ch),
            Some(ch @ '>') => Token::from_char(TokenType::Gt, ch),

            Some(ch @ '(') => Token::from_char(TokenType::LParen, ch),
            Some(ch @ ')') => Token::from_char(TokenType::RParen, ch),
            Some(ch @ '{') => Token::from_char(TokenType::LBrace, ch),
            Some(ch @ '}') => Token::from_char(TokenType::RBrace, ch),
            Some(ch @ ',') => Token::from_char(TokenType::Comma, ch),
            Some(ch @ ';') => Token::from_char(TokenType::Semicolon, ch),
            Some(ch) => {
                if ch.is_alphabetic() {
                    let literal = self.read_identifier();
                    let typ = lookup_ident(literal);
                    return Token::new(typ, literal);
                } else if ch.is_digit(10) {
                    return Token::new(TokenType::Int, self.read_number());
                } else {
                    return Token::new_illegal();
                }
            }
            None => Token::new(TokenType::EOF, ""),
        };
        self.read_char();
        tok
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_output_tokens(input: &str, expected_output: Vec<(TokenType, &str)>) {
        let mut lexer = Lexer::new(input);

        for (idx, (tok_typ, literal)) in expected_output.into_iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                tok.typ, tok_typ,
                "[{}] Expected ({:?} {:?}) => Got ({:?} {:?})",
                idx, tok.typ, tok.literal, tok_typ, literal
            );
            assert_eq!(tok.literal, literal);
        }
    }

    #[test]
    fn test_eq_op_tokens() {
        let input = r#"
        10 == 10;

        10 != 9;
        "#;
        let expected_output = vec![
            (TokenType::Int, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Int, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
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
            (TokenType::Let, "let"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::EOF, ""),
        ];
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
        let tests = vec![
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::Gt, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::Lt, "<"),
            (TokenType::Int, "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (idx, (tok_typ, literal)) in tests.into_iter().enumerate() {
            let tok = lexer.next_token();
            assert_eq!(
                tok.typ, tok_typ,
                "[{}] Comparing {:?} {:?}",
                idx, tok.typ, tok.literal
            );
            assert_eq!(tok.literal, literal);
        }
    }
}
