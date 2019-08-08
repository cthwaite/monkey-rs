use crate::lexer::Lexer;
use crate::token::TokenType;
use std::io::{self, BufRead, BufReader, Read, Write};

const PROMPT: &'static str = ">> ";

pub fn start<R: Read, W: Write>(reader: &mut R, writer: &mut W) -> io::Result<()> {
    let mut buffer = String::new();
    let mut reader = BufReader::new(reader);
    loop {
        write!(writer, "{}", PROMPT)?;
        writer.flush()?;
        buffer.clear();
        let line_len = reader.read_line(&mut buffer)?;
        if line_len == 0 {
            return Ok(());
        }
        let mut lexer = Lexer::new(&buffer);
        let mut tok = lexer.next_token();
        loop {
            writeln!(writer, "{:?}", tok)?;
            if tok.typ == TokenType::EOF {
                break;
            }
            tok = lexer.next_token();
        }
    }
}
