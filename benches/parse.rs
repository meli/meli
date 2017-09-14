#![feature(test)]
extern crate melib;

use melib::mailbox::email::Mail;

extern crate test;
use self::test::Bencher;

#[bench]
fn mail_parse(b: &mut Bencher) {
    b.iter(|| Mail::from("test/attachment_test") );
}
