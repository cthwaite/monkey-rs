use interp::repl::start;
use std::io::BufRead;
use std::io::{stdin, stdout};

fn main() -> Result<(), std::io::Error> {
    start(&mut stdin(), &mut stdout())
}
