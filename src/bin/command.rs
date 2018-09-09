//! Defines a simple REPL you can use to test out the command parser.

extern crate rfcbot_lib;

use rfcbot_lib::command;

use std::io;
use std::io::prelude::*;

fn main() -> io::Result<()> {
    let mut input = String::new();
    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    loop {
        write!(stdout, "> ")?;
        stdout.flush()?;

        input.clear();
        stdin.read_line(&mut input)?;

        match command::parse(&input, command::Config::default()) {
            Ok(cmd) => println!("{:?}", cmd),
            Err(err) => println!("{}", err),
        }
    }
}
