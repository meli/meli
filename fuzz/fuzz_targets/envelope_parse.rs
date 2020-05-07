#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate melib;

use melib::Envelope;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    let _envelope = Envelope::from_bytes(data, None);
});
