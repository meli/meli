//
// melib
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of melib.
//
// melib is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// melib is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with melib. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::path::{Path, PathBuf};

use regex::Regex;

use crate::{
    backends::FlagOp,
    email::Flag,
    error::Result,
    maildir::{move_to_cur, Configuration, MaildirPathTrait},
};

fn set_flags(config: &Configuration, path: &Path, flag_ops: &[FlagOp]) -> Result<PathBuf> {
    let mut new_flags = path.flags();
    for op in flag_ops.iter() {
        if let FlagOp::Set(f) | FlagOp::UnSet(f) = op {
            new_flags.set(*f, op.as_bool());
        }
    }

    path.set_flags(new_flags, config)
}

#[test]
fn test_maildir_move_to_cur_rename() {
    let config = Configuration { rename_regex: None };
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:2,FRS")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,FRS")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:2,")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1:1,")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:1,:2,")
    );
    assert_eq!(
        move_to_cur(&config, Path::new("/path/to/new/1423819205.29514_1")).unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1:2,")
    );
}

#[test]
fn test_maildir_move_to_cur_rename_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
    };
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=1:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo,U=:2,S")
    );
    assert_eq!(
        move_to_cur(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo:2,S")
        )
        .unwrap(),
        Path::new("/path/to/cur/1423819205.29514_1.foo:2,S")
    );
}

#[test]
fn test_maildir_set_flags() {
    let config = Configuration { rename_regex: None };

    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,FRS").to_path_buf()),
        "Setting the same flags should not change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::UnSet(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,").to_path_buf()),
        "UnSetting all the set flags should change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::TRASHED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,FRST").to_path_buf()),
        "Setting new flags should change the path to include them"
    );
}

#[test]
fn test_maildir_set_flags_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
    };

    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,S"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo:2,FRS").to_path_buf()),
        "Setting the same flags should not change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS"),
            &[FlagOp::UnSet(Flag::FLAGGED | Flag::SEEN | Flag::REPLIED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo:2,").to_path_buf()),
        "UnSetting all the set flags should change the path"
    );
    assert_eq!(
        set_flags(
            &config,
            Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS"),
            &[FlagOp::Set(Flag::FLAGGED | Flag::TRASHED)]
        ),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo:2,FRST").to_path_buf()),
        "Setting new flags should change the path to include them"
    );
}

#[test]
fn test_maildir_place_in_dir() {
    let config = Configuration { rename_regex: None };

    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,")
            .place_in_dir(Path::new("/path/to/new/"), &config),
        Ok(Path::new("/path/to/new/1423819205.29514_1:2,").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change the parent dir",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,").to_path_buf()),
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1:2,FRS")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,FRS").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change flags",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1:2,").to_path_buf()),
        "place_in_dir() should add missing `:2,` substring"
    );
}

#[test]
fn test_maildir_place_in_dir_regexp() {
    let config = Configuration {
        rename_regex: Some(Regex::new(r",U=\d\d*").unwrap()),
    };

    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,")
            .place_in_dir(Path::new("/path/to/new/"), &config),
        Ok(Path::new("/path/to/new/1423819205.29514_1.foo:2,").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change the parent dir",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,").to_path_buf()),
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123:2,FRS")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,FRS").to_path_buf()),
        "place_in_dir() where dest_dir is the same should not change flags",
    );
    assert_eq!(
        Path::new("/path/to/new/1423819205.29514_1.foo,U=123")
            .place_in_dir(Path::new("/path/to2/new/"), &config),
        Ok(Path::new("/path/to2/new/1423819205.29514_1.foo:2,").to_path_buf()),
        "place_in_dir() should add missing `:2,` substring"
    );
}
