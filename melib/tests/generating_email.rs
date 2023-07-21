/*
 * meli
 *
 * Copyright 2019 Manos Pitsidianakis
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

use std::io::Read;

use flate2::bufread::GzDecoder;
use melib::email::Draft;

#[test]
fn test_build_draft() {
    let mut new_draft = Draft::default();
    let attachment = melib::email::attachment_from_file(&"./tests/data/test_image.gif")
        .expect("Could not open test_image.gif.");
    new_draft.headers_mut().remove("User-Agent");
    new_draft.headers_mut().remove("Date");

    new_draft.attachments_mut().push(attachment);
    new_draft.set_body("hello world.".to_string());
    let raw = new_draft.finalise().expect("could not finalise draft");
    let boundary_def = raw.find("bzz_bzz__bzz__").unwrap();
    let boundary_end = boundary_def + raw[boundary_def..].find('\"').unwrap();
    let boundary = raw[boundary_def..boundary_end].to_string();
    let boundary_str = &boundary["bzz_bzz__bzz__".len()..];

    let raw = raw.replace(boundary_str, "");

    let mut gz = GzDecoder::new(include_bytes!("./data/generated_email.eml.gz").as_slice());
    let mut s = String::new();
    gz.read_to_string(&mut s).unwrap();

    assert_eq!(&s, &raw);
}
