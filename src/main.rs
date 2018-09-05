#![recursion_limit="128"]

#[macro_use] extern crate maplit;
#[macro_use] extern crate nom;
#[macro_use] extern crate combine;
#[macro_use] extern crate indexmap;

mod command;
mod cmd_combine;

fn parse_vec_ok(text: &str) -> Vec<command::Command<&str>> {
    command::parse(text)
        .filter_map(Result::ok)
        .map(|(_, cmd)| cmd)
        .collect::<Vec<_>>()
}

fn main() {
    cmd_combine::main();

    println!("{:#?}", parse_vec_ok("{{@rfcbot merge}}"));
}
