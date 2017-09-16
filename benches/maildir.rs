#![feature(test)]
extern crate melib;
use melib::mailbox::backends::maildir::*;

extern crate test;
use self::test::Bencher;

#[bench]
fn bench_threads_1(b: &mut Bencher) {
    b.iter(|| MaildirType::new("").get_multicore(1));
}
#[bench]
fn bench_threads_2(b: &mut Bencher) {
    b.iter(|| MaildirType::new("").get_multicore(2));
}
#[bench]
fn bench_threads_3(b: &mut Bencher) {
    b.iter(|| MaildirType::new("").get_multicore(3));
}
#[bench]
fn bench_threads_4(b: &mut Bencher) {
    b.iter(|| MaildirType::new("").get_multicore(4));
}
#[bench]
fn bench_threads_6(b: &mut Bencher) {
    b.iter(|| MaildirType::new("").get_multicore(6));
}
