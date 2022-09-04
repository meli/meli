#[macro_use]
extern crate lalrpop_util;

pub mod lexer;
lalrpop_mod!(pub muttrc); // synthesized by LALRPOP

fn main() {
    println!(
        "Hello, world! {:?}",
        muttrc::MuttrcCommandParser::new().parse("").unwrap()
    );
}
