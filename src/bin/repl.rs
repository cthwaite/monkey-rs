use interp::repl;
use std::io::{stdin, stdout};

fn main() -> Result<(), std::io::Error> {
    println!("Welcome to the Monkey programming language");
    repl::start(&mut stdin(), &mut stdout())
}
