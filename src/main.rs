#![recursion_limit="128"]

#[macro_use] extern crate maplit;
#[macro_use] extern crate combine;

extern crate itertools;

mod command;

fn main() {
    command::main();
}
