#![recursion_limit="128"]

#[macro_use] extern crate maplit;
#[macro_use] extern crate combine;

#[cfg(test)]
#[macro_use]
extern crate proptest;

extern crate itertools;

pub mod command;
