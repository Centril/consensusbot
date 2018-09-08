#![recursion_limit="128"]

#[macro_use] extern crate nom;

#[macro_use] extern crate maplit;
#[macro_use] extern crate combine;

extern crate itertools;

mod nom_command;
mod command;

fn parse_vec_ok(text: &str) -> Vec<nom_command::Command<&str>> {
    nom_command::parse(text)
        .filter_map(Result::ok)
        .map(|(_, cmd)| cmd)
        .collect::<Vec<_>>()
}

fn main() {
    command::main();

    println!("{:#?}", parse_vec_ok("{{@rfcbot merge}}"));
}
