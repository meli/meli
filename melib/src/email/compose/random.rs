use std::char;
use std::fs::File;
use std::io::prelude::*;
use std::time::SystemTime;

fn random_u64() -> u64 {
    let mut f = File::open("/dev/urandom").unwrap();
    let mut buffer = [0; 8];

    // read exactly 10 bytes
    f.read_exact(&mut buffer).unwrap();

    u64::from(buffer[0])
        | (u64::from(buffer[1]) << 8)
        | (u64::from(buffer[2]) << 16)
        | (u64::from(buffer[3]) << 24)
        | (u64::from(buffer[4]) << 32)
        | (u64::from(buffer[5]) << 40)
        | (u64::from(buffer[6]) << 48)
        | (u64::from(buffer[7]) << 56)
}

fn clock() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

fn base36(mut m: u64) -> String {
    let mut stack = Vec::with_capacity(32);

    while m >= 36 {
        stack.push((m % 36) as u32);
        m /= 36;
    }

    let mut ret = String::with_capacity(stack.len());

    while let Some(d) = stack.pop() {
        ret.push(char::from_digit(d, 36).unwrap());
    }
    ret
}

pub fn gen_message_id(fqdn: &str) -> String {
    let clock = base36(clock());
    let rand = base36(random_u64());

    format!("<{}.{}@{}>", clock, rand, fqdn)
}
