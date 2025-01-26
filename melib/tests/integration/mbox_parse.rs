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

use std::{
    collections::HashMap,
    io::Read,
    sync::{Arc, Mutex},
};

use flate2::bufread::GzDecoder;
use melib::{mbox::*, Envelope, Result};

#[test]
fn test_mbox_parse() {
    fn gz_to_string(bytes: &'static [u8]) -> String {
        let mut gz = GzDecoder::new(bytes);
        let mut s = String::new();
        gz.read_to_string(&mut s).unwrap();
        s
    }

    {
        let sha1dc_diet_op = gz_to_string(
            include_bytes!("../data/PATCH-Put-sha1dc-on-a-diet_op.mbox.gz").as_slice(),
        );
        let sha1dc_diet_thread =
            gz_to_string(include_bytes!("../data/PATCH-Put-sha1dc-on-a-diet.mbox.gz").as_slice());

        let message_iter = MessageIterator {
            index: Arc::new(Mutex::new(HashMap::default())),
            input: sha1dc_diet_thread.as_bytes(),
            offset: 0,
            file_offset: 0,
            format: Some(MboxFormat::MboxCl2),
            is_crlf: false,
        };
        let envelopes: Vec<Envelope> = message_iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(envelopes.len(), 36);

        let mut original_post: Vec<Envelope> = MessageIterator {
            index: Arc::new(Mutex::new(HashMap::default())),
            input: sha1dc_diet_op.as_bytes(),
            offset: 0,
            file_offset: 0,
            format: Some(MboxFormat::MboxCl2),
            is_crlf: false,
        }
        .collect::<Result<Vec<_>>>()
        .unwrap();
        assert_eq!(original_post.len(), 1);
        let original_post = original_post.pop().unwrap();

        assert_eq!(&original_post, &envelopes[0]);
    }

    {
        let git_am_op = gz_to_string(
            include_bytes!("../data/git-am-breakage-with-MIME-decoding_op.mbox.gz").as_slice(),
        );
        let git_am_thread = gz_to_string(
            include_bytes!("../data/git-am-breakage-with-MIME-decoding.mbox.gz").as_slice(),
        );

        let message_iter = MessageIterator {
            index: Arc::new(Mutex::new(HashMap::default())),
            input: git_am_thread.as_bytes(),
            offset: 0,
            file_offset: 0,
            format: Some(MboxFormat::MboxCl2),
            is_crlf: false,
        };
        let envelopes: Vec<Envelope> = message_iter.collect::<Result<Vec<_>>>().unwrap();
        assert_eq!(envelopes.len(), 7);

        let mut original_post: Vec<Envelope> = MessageIterator {
            index: Arc::new(Mutex::new(HashMap::default())),
            input: git_am_op.as_bytes(),
            offset: 0,
            file_offset: 0,
            format: Some(MboxFormat::MboxCl2),
            is_crlf: false,
        }
        .collect::<Result<Vec<_>>>()
        .unwrap();
        assert_eq!(original_post.len(), 1);
        let original_post = original_post.pop().unwrap();

        assert_eq!(&original_post, &envelopes[0]);
    }
}
