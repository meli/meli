#![feature(test)]
extern crate melib;

use melib::mailbox::email::Mail;
use melib::mailbox::backends::BackendOpGenerator;
use melib::mailbox::backends::maildir::MaildirOp;

extern crate test;
use self::test::Bencher;

#[bench]
fn mail_parse(b: &mut Bencher) {
                            b.iter(|| Mail::from(Box::new(BackendOpGenerator::new(Box::new(move || {
                               Box::new(MaildirOp::new("test/attachment_test".to_string()))})))) );
}
