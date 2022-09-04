#[macro_use]
extern crate lalrpop_util;

pub mod lexer;
lalrpop_mod!(pub muttrc); // synthesized by LALRPOP

fn main() {
    let muttrc = std::fs::read_to_string("./.muttrc").unwrap();
    println!(
        "Hello, world! {:?}",
        muttrc::MuttrcCommandParser::new().parse(&muttrc).unwrap()
    );
}
