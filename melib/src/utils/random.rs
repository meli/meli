/*
 * meli - melib crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

use std::{fs::File, io::prelude::*, time::SystemTime};

const EXPECT: &str = "Could not open/read /dev/urandom";

pub fn random_u64() -> u64 {
    let mut f = File::open("/dev/urandom").expect(EXPECT);
    let mut buffer = [0; 8];

    // read exactly 8 bytes
    f.read_exact(&mut buffer).expect(EXPECT);

    u64::from(buffer[0])
        | (u64::from(buffer[1]) << 8)
        | (u64::from(buffer[2]) << 16)
        | (u64::from(buffer[3]) << 24)
        | (u64::from(buffer[4]) << 32)
        | (u64::from(buffer[5]) << 40)
        | (u64::from(buffer[6]) << 48)
        | (u64::from(buffer[7]) << 56)
}

pub fn random_u32() -> u32 {
    let mut f = File::open("/dev/urandom").expect(EXPECT);
    let mut buffer = [0; 4];

    // read exactly 4 bytes
    f.read_exact(&mut buffer).expect(EXPECT);

    u32::from(buffer[0])
        | (u32::from(buffer[1]) << 8)
        | (u32::from(buffer[2]) << 16)
        | (u32::from(buffer[3]) << 24)
}

pub fn random_u8() -> u8 {
    let mut f = File::open("/dev/urandom").expect(EXPECT);
    let mut buffer = [0; 1];

    // read exactly 1 byte
    f.read_exact(&mut buffer).expect(EXPECT);

    buffer[0]
}

pub fn clock() -> u64 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

pub fn clock_millis() -> u128 {
    SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_millis()
}
