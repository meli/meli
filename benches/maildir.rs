//#![feature(test)]
//use melib::conf::AccountSettings;
//use melib::mailbox::backends::maildir::*;
//
//extern crate test;
//use self::test::Bencher;
//
//#[bench]
//fn bench_threads_1(b: &mut Bencher) {
//    b.iter(|| {
//        let folder = Folder::new(
//            String::from(""),
//            String::from(""),
//            vec![],
//        );
//        MaildirType::new("").multicore(1, &folder)
//    });
//}
//#[bench]
//fn bench_threads_2(b: &mut Bencher) {
//    b.iter(|| {
//        let folder = Folder::new(
//            String::from(""),
//            String::from(""),
//            vec![],
//        );
//        MaildirType::new("").multicore(2, &folder)
//    });
//}
//#[bench]
//fn bench_threads_3(b: &mut Bencher) {
//    b.iter(|| {
//        let folder = Folder::new(
//            String::from(""),
//            String::from(""),
//            vec![],
//        );
//        MaildirType::new("").multicore(3, &folder)
//    });
//}
//#[bench]
//fn bench_threads_4(b: &mut Bencher) {
//    b.iter(|| {
//        let folder = Folder::new(
//            String::from(""),
//            String::from(""),
//            vec![],
//        );
//        MaildirType::new("").multicore(4, &folder)
//    });
//}
//#[bench]
//fn bench_threads_6(b: &mut Bencher) {
//    b.iter(|| {
//        let folder = Folder::new(
//            String::from(""),
//            String::from(""),
//            vec![],
//        );
//        MaildirType::new("").multicore(6, &folder)
//    });
//}
