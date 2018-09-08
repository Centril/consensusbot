#![recursion_limit="128"]

#[macro_use] extern crate maplit;
#[macro_use] extern crate combine;

#[cfg(test)]
#[macro_use]
extern crate proptest;

extern crate itertools;

mod command;

use std::io;

fn main() -> io::Result<()> {
    use std::io::prelude::*;

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

        match command::parse(&input) {
            Ok(cmd) => println!("{:?}", cmd),
            Err(err) => println!("{}", err),
        }
    }
}
