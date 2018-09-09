#![recursion_limit="128"]

#[macro_use] extern crate maplit;
#[macro_use] extern crate combine;

extern crate toml;
extern crate serde;
#[macro_use] extern crate serde_derive;

#[cfg(test)]
#[macro_use]
extern crate proptest;

extern crate itertools;

mod teams;
pub mod command;
