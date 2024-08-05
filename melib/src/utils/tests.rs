//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{
    fs::{File, OpenOptions},
    io::Write,
    os::unix::fs::PermissionsExt,
    path::Path,
};

use sealed_test::prelude::*;
use tempfile::TempDir;

use crate::error::{Errno, ErrorKind};

#[sealed_test]
#[ignore]
#[test]
fn test_shellexpandtrait() {
    use super::shellexpand::*;

    fn create_file_util(path: &str) {
        let path = Path::new(path).expand();
        let mut f = File::create(&path).unwrap();
        let mut permissions = f.metadata().unwrap().permissions();
        permissions.set_mode(0o600); // Read/write for owner only.
        f.set_permissions(permissions).unwrap();
        f.write_all(b"\n").unwrap();
        f.flush().unwrap();
        assert!(path.exists());
    }

    fn create_dir_util(path: &str) {
        let path = Path::new(path).expand();
        std::fs::create_dir(&path).unwrap();
        assert!(path.exists());
        assert!(path.is_dir());
    }

    std::env::remove_var("HOME");

    let tmp_dir = TempDir::new().unwrap();

    std::env::set_var("HOME", tmp_dir.path());

    assert_eq!(&Path::new("~").expand(), tmp_dir.path());

    assert_eq!(
        &tmp_dir.path().join("~/doc.pdf").expand(),
        &tmp_dir.path().join("~/doc.pdf")
    );

    assert_eq!(
        Path::new("~").expand().complete(false, false),
        Completions::IsDirectory
    );
    assert_eq!(
        Path::new("~").expand().complete(true, false),
        Completions::Entries(vec!["/".to_string()]),
        r#" ~<TAB> should append a "/": "~/""#,
    );
    assert_eq!(
        Path::new("~").expand().complete(true, true),
        Completions::Entries(vec![]),
        r#"~/<TAB> should return directory entries."#
    );

    create_file_util("~/doc.pdf");

    assert_eq!(
        Path::new("~").expand().complete(false, false),
        Completions::IsDirectory
    );
    assert_eq!(
        Path::new("~").expand().complete(true, false),
        Completions::Entries(vec!["/".to_string()]),
        r#"~<TAB> should again append a "/": "~/""#
    );
    assert_eq!(
        Path::new("~").expand().complete(true, true),
        Completions::Entries(vec!["doc.pdf".to_string()]),
        r#"~/<TAB> should return directory entries."#
    );
    assert_eq!(
        Path::new("~/doc").expand().complete(true, false),
        Completions::Entries(vec![".pdf".to_string()]),
        r#"~/pattern<TAB> should return directory entries matching ~/pattern* glob."#
    );
    create_file_util("~/doc2.pdf");
    assert_eq!(
        Path::new("~/doc").expand().complete(true, false),
        Completions::Entries(vec![".pdf".to_string(), "2.pdf".to_string()]),
        r#"~/pattern<TAB> should again return directory entries matching ~/pattern* glob."#
    );
    create_dir_util("~/doc");
    create_file_util("~/doc/2.pdf");
    assert_eq!(
        Path::new("~/doc").expand().complete(true, false),
        Completions::Entries(vec![
            ".pdf".to_string(),
            "/".to_string(),
            "2.pdf".to_string(),
        ]),
        r#"~/pattern<TAB> should again return directory entries matching ~/pattern* glob."#
    );
    assert_eq!(
        Path::new("~/doc").expand().complete(true, false),
        Completions::Entries(vec![
            ".pdf".to_string(),
            "/".to_string(),
            "2.pdf".to_string(),
        ]),
        r#"~/pattern<TAB> should again return directory entries matching ~/pattern* glob."#
    );
    assert_eq!(
        Path::new("~/doc").expand().complete(false, false),
        Completions::IsDirectory,
        r#"~/doc/<TAB> should not return ~/ entries matching ~/doc* glob."#
    );
    assert_eq!(
        Path::new("~/doc").expand().complete(true, true),
        Completions::Entries(vec!["2.pdf".to_string()])
    );
    assert_eq!(
        Path::new("/").expand().complete(false, false),
        Completions::IsDirectory
    );
    assert_eq!(
        Path::new("/").expand().complete(true, false),
        Completions::IsDirectory
    );
    assert!(matches!(Path::new("/").expand().complete(true, true),
        Completions::Entries(entries) if !entries.is_empty()));
    _ = tmp_dir.close();
}

#[cfg(target_os = "linux")]
#[sealed_test]
#[ignore]
#[test]
fn test_shellexpandtrait_impls() {
    use super::shellexpand::*;

    fn create_file_util(path: &str) {
        let path = Path::new(path).expand();
        let mut f = File::create(&path).unwrap();
        let mut permissions = f.metadata().unwrap().permissions();
        permissions.set_mode(0o600); // Read/write for owner only.
        f.set_permissions(permissions).unwrap();
        f.write_all(b"\n").unwrap();
        f.flush().unwrap();
        assert!(path.exists());
    }

    fn create_dir_util(path: &str) {
        let path = Path::new(path).expand();
        std::fs::create_dir(&path).unwrap();
        assert!(path.exists());
        assert!(path.is_dir());
    }

    std::env::remove_var("HOME");

    let tmp_dir = TempDir::new().unwrap();

    std::env::set_var("HOME", tmp_dir.path());

    assert_eq!(&Path::new("~").expand(), tmp_dir.path());

    macro_rules! assert_complete {
        (($path:expr, $force:literal, $treat_as_dir:literal), $($expected:tt)*) => {{
            assert_eq!(
                impls::inner_complete_linux($path, $force, $treat_as_dir),
                impls::inner_complete_generic($path, $force, $treat_as_dir),
                concat!("Expected ", stringify!($($expected)*)),
            );
            assert_eq!(
                impls::inner_complete_generic($path, $force, $treat_as_dir),
                $($expected)*,
            );
        }};
    }

    assert_complete!(
        (&Path::new("~").expand(), false, false),
        Completions::IsDirectory
    );
    assert_complete!(
        (&Path::new("~").expand(), true, false),
        Completions::Entries(vec!["/".to_string()])
    );
    assert_complete!(
        (&Path::new("~").expand(), true, true),
        Completions::Entries(vec![])
    );

    create_file_util("~/doc.pdf");

    assert_complete!(
        (&Path::new("~").expand(), false, false),
        Completions::IsDirectory
    );
    assert_complete!(
        (&Path::new("~").expand(), true, false),
        Completions::Entries(vec!["/".to_string()])
    );
    assert_complete!(
        (&Path::new("~").expand(), true, true),
        Completions::Entries(vec!["doc.pdf".to_string()])
    );
    assert_complete!(
        (&Path::new("~/doc").expand(), true, false),
        Completions::Entries(vec![".pdf".to_string()])
    );
    create_file_util("~/doc2.pdf");
    assert_complete!(
        (&Path::new("~/doc").expand(), true, false),
        Completions::Entries(vec![".pdf".to_string(), "2.pdf".to_string()])
    );
    create_dir_util("~/doc");
    create_file_util("~/doc/2.pdf");
    assert_complete!(
        (&Path::new("~/doc").expand(), true, false),
        Completions::Entries(vec![
            ".pdf".to_string(),
            "/".to_string(),
            "2.pdf".to_string(),
        ])
    );
    assert_complete!(
        (&Path::new("~/doc").expand(), false, false),
        Completions::IsDirectory
    );
    assert_complete!(
        (&Path::new("~/doc").expand(), true, true),
        Completions::Entries(vec!["2.pdf".to_string()])
    );
    assert_complete!(
        (&Path::new("/").expand(), false, false),
        Completions::IsDirectory
    );
    assert_complete!(
        (&Path::new("/").expand(), true, false),
        Completions::IsDirectory
    );
    _ = tmp_dir.close();
}

#[test]
fn test_fd_locks() {
    use crate::utils::lock::*;

    fn create_file_util(path: &Path) -> File {
        use super::shellexpand::ShellExpandTrait;

        let path = Path::new(path).expand();
        let mut f = File::create(&path).unwrap();
        let mut permissions = f.metadata().unwrap().permissions();
        permissions.set_mode(0o600); // Read/write for owner only.
        f.set_permissions(permissions).unwrap();
        f.write_all(b"\n").unwrap();
        f.flush().unwrap();
        assert!(path.exists());
        f
    }

    let tmp_dir = TempDir::new().unwrap();

    let file_path = tmp_dir.path().join("test.txt");

    let f = create_file_util(&file_path)
        .lock(FileLockOptions::Blocking, &file_path)
        .unwrap();
    let open = || {
        OpenOptions::new()
            .read(true)
            .write(true)
            .open(&file_path)
            .unwrap()
    };
    let f2 = open();
    assert_eq!(
        f2.lock(
            FileLockOptions::Nonblocking {
                max_tries: 0,
                try_wait: None,
            },
            &file_path,
        )
        .unwrap_err()
        .kind,
        ErrorKind::OSError(Errno::EAGAIN)
    );
    assert_eq!(
        open()
            .lock(
                FileLockOptions::Nonblocking {
                    max_tries: 1,
                    try_wait: None,
                },
                &file_path,
            )
            .unwrap_err()
            .kind,
        ErrorKind::OSError(Errno::EAGAIN)
    );
    drop(f);
    open()
        .lock(
            FileLockOptions::Nonblocking {
                max_tries: 0,
                try_wait: None,
            },
            &file_path,
        )
        .unwrap();
}
