/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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
    borrow::Cow,
    fs::OpenOptions,
    io::Write as _,
    os::{fd::OwnedFd, unix::fs::PermissionsExt as _},
    path::{Path, PathBuf},
};

use melib::{
    error::*,
    text::{TextProcessing as _, Truncate as _},
    uuid::Uuid,
    ShellExpandTrait as _,
};

/// Temporary file that can optionally cleaned up when it is dropped.
#[derive(Debug)]
pub struct File {
    /// File's path.
    path: PathBuf,
    /// Delete file when it is dropped.
    delete_on_drop: bool,
}

impl Drop for File {
    fn drop(&mut self) {
        if self.delete_on_drop {
            let _ = std::fs::remove_file(self.path());
        }
    }
}

impl File {
    /// Open as a standard library file type.
    pub fn as_std_file(&self) -> Result<std::fs::File> {
        OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&self.path)
            .chain_err_summary(|| format!("Could not create/open path {}", self.path.display()))
    }

    /// The file's path.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Convenience method to read `File` to `String`.
    pub fn read_to_string(&self) -> Result<String> {
        std::fs::read_to_string(&self.path).chain_err_related_path(&self.path)
    }

    /// Returned `File` will be deleted when dropped if `delete_on_drop` is set,
    /// so make sure to add it on `context.temp_files` to reap it later.
    pub fn create_temp_file(
        bytes: &[u8],
        filename: Option<&str>,
        mut path: Option<&mut PathBuf>,
        extension: Option<&str>,
        delete_on_drop: bool,
    ) -> Result<Self> {
        let filename_value: Option<Cow<'_, str>> = filename.map(|f| {
            let mut f = Cow::Borrowed(f);
            sanitize_filename(&mut f);
            f
        });
        let mut filename: Option<&str> = filename_value.as_deref();

        loop {
            let mut dir = std::env::temp_dir();
            let path = if let Some(ref mut p) = path {
                if p.is_dir() {
                    if let Some(filename) = filename {
                        p.push(filename);
                        let parts = p
                            .extension()
                            .and_then(|ext| Some((p.file_stem()?, ext)))
                            .map(|(stem, ext)| {
                                (stem.to_string_lossy().into_owned(), ext.to_os_string())
                            });
                        'exists: while p.try_exists().unwrap_or_default() {
                            for i in 0..u8::MAX {
                                p.pop();
                                if let Some((ref stem, ref ext)) = parts {
                                    p.push(format!("{stem}_{i}"));
                                    p.set_extension(ext);
                                } else {
                                    p.push(format!("{filename}_{i}"));
                                }
                                if !p.try_exists().unwrap_or_default() {
                                    break 'exists;
                                }
                            }
                            loop {
                                p.pop();
                                if let Some((ref stem, ref ext)) = parts {
                                    p.push(format!("{stem}_{}", Uuid::new_v4().as_simple()));
                                    p.set_extension(ext);
                                } else {
                                    p.push(format!("{filename}_{}", Uuid::new_v4().as_simple()));
                                }
                                if !p.try_exists().unwrap_or_default() {
                                    break;
                                }
                            }
                        }
                    } else {
                        let u = Uuid::new_v4();
                        p.push(u.as_simple().to_string());
                    }
                }
                p
            } else {
                dir.push("meli");
                std::fs::DirBuilder::new().recursive(true).create(&dir)?;
                if let Some(filename) = filename {
                    dir.push(filename);
                    let parts = dir
                        .extension()
                        .and_then(|ext| Some((dir.file_stem()?, ext)))
                        .map(|(stem, ext)| {
                            (stem.to_string_lossy().into_owned(), ext.to_os_string())
                        });
                    'exists: while dir.try_exists().unwrap_or_default() {
                        for i in 0..u8::MAX {
                            dir.pop();
                            if let Some((ref stem, ref ext)) = parts {
                                dir.push(format!("{stem}_{i}"));
                                dir.set_extension(ext);
                            } else {
                                dir.push(format!("{filename}_{i}"));
                            }
                            if !dir.try_exists().unwrap_or_default() {
                                break 'exists;
                            }
                        }
                        loop {
                            dir.pop();
                            if let Some((ref stem, ref ext)) = parts {
                                dir.push(format!("{stem}_{}", Uuid::new_v4().as_simple()));
                                dir.set_extension(ext);
                            } else {
                                dir.push(format!("{filename}_{}", Uuid::new_v4().as_simple()));
                            }
                            if !dir.try_exists().unwrap_or_default() {
                                break;
                            }
                        }
                    }
                } else {
                    let u = Uuid::new_v4();
                    dir.push(u.as_simple().to_string());
                }
                &mut dir
            };
            if let Some(ext) = extension {
                path.set_extension(ext);
            }
            fn inner(path: &Path, bytes: &[u8], delete_on_drop: bool) -> Result<File> {
                let path = path.expand();
                let mut f = std::fs::File::options()
                    .read(true)
                    .write(true)
                    .create_new(true)
                    .open(&path)?;
                let metadata = f.metadata()?;
                let mut permissions = metadata.permissions();

                permissions.set_mode(0o600); // Read/write for owner only.
                f.set_permissions(permissions)?;

                f.write_all(bytes)?;
                f.flush()?;
                Ok(File {
                    path,
                    delete_on_drop,
                })
            }
            match (inner(path, bytes, delete_on_drop), filename) {
                (Err(err), Some(ref mut val))
                    if matches!(
                        err.kind,
                        ErrorKind::OSError(Errno::ENAMETOOLONG | Errno::EEXIST)
                    ) && val.grapheme_len() > 1 =>
                {
                    val.truncate_at_boundary(val.grapheme_len().saturating_sub(1));
                    filename = Some(val);
                }
                (Err(err), _) => {
                    return Err(err).chain_err_summary(|| {
                        format!("Could not create file at path {}", path.display())
                    })
                }
                (ok @ Ok(_), _) => return ok,
            }
        }
    }
}

pub fn pipe() -> Result<(OwnedFd, OwnedFd)> {
    nix::unistd::pipe().map_err(|err| {
        Error::new("Could not create pipe")
            .set_source(Some(Box::new(err)))
            .set_kind(ErrorKind::Platform)
    })
}

/// Remove system path separator from filename
#[inline(always)]
pub fn sanitize_separator(value: &mut Cow<'_, str>) {
    if value.contains(std::path::MAIN_SEPARATOR) {
        *value = Cow::Owned(value.replace(std::path::MAIN_SEPARATOR, "_"))
    };
}

pub fn sanitize_filename(value: &mut Cow<'_, str>) {
    // Replace with <https://docs.rs/regex/latest/regex/macro.regex.html> when we update the regex
    // dependency
    macro_rules! regex {
        ($re:literal) => {{
            static REGEX: std::sync::LazyLock<regex::Regex> =
                std::sync::LazyLock::new(|| regex::Regex::new($re).expect("invalid regex pattern"));

            // Coerce returned type from `&Lazy<Regex>` to `&Regex` to avoid making the
            // inner type public.
            let re: &regex::Regex = &REGEX;
            re
        }};
    }

    // Macro to detect whether <regex>.replace_all performed no replacements, because it returns a
    // Cow::Borrowed that borrowes the _haystack_ and not the function argument `value`'s lifetime.
    macro_rules! replace_all {
        ($re:expr, $with:literal) => {{
            let re = $re;
            match re.replace_all(value.as_ref(), $with) {
                Cow::Owned(owned) => {
                    *value = Cow::Owned(owned);
                }
                Cow::Borrowed(_haystack) => {}
            }
        }};
    }

    sanitize_separator(value);

    replace_all!(regex!(r"(?m)[[:space:]]+"), "_");
    replace_all!(regex!(r#"(?m)[!"'/\\]+"#), "-");
    replace_all!(regex!(r"(?m)[[:cntrl:]]*"), "");
    replace_all!(regex!(r"(?m)[[:blank:]]*"), "");
    replace_all!(regex!(r#"^[!"'/\\]*"#), "");
    replace_all!(regex!(r"(?m)__+"), "_");
    replace_all!(regex!(r#"[!"'/\\]*$"#), "");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_invalid_path() {
        let f = File {
            path: PathBuf::from("//////"),
            delete_on_drop: true,
        };
        f.as_std_file().unwrap_err();
    }

    #[test]
    fn test_file_delete_on_drop() {
        const S: &str = "hello world";
        let tempdir = tempfile::tempdir().unwrap();

        let delete_on_drop = File::create_temp_file(
            S.as_bytes(),
            None,
            Some(&mut tempdir.path().join("test")),
            None,
            true,
        )
        .unwrap();
        assert_eq!(&delete_on_drop.read_to_string().unwrap(), S);
        drop(delete_on_drop);
        assert!(!tempdir.path().join("test").try_exists().unwrap());

        let persist = File::create_temp_file(
            S.as_bytes(),
            None,
            Some(&mut tempdir.path().join("test")),
            None,
            false,
        )
        .unwrap();
        assert_eq!(&persist.read_to_string().unwrap(), S);
        drop(persist);
        assert!(tempdir.path().join("test").try_exists().unwrap());

        _ = tempdir.close();
    }

    #[test]
    fn test_file_sanitize_filename() {
        const OK_FILENAME: &str = "okay";
        const PATH_SEP_FILENAME: &str = "meli/meli/issues/712/comment/4492@git.meli-email.org";
        const EFFED_UP_FILENAME: &str = "Re: Some long subject - \"User Dot. Name\" \
                                         <user1@example.com> Sent from my bPad 2024-09-07, on   a \
                                         sunny Saturday";

        let mut filename = Cow::Borrowed(OK_FILENAME);
        sanitize_filename(&mut filename);
        assert_eq!(filename, Cow::<'static, str>::Borrowed(OK_FILENAME));
        sanitize_separator(&mut filename);
        assert_eq!(filename, Cow::<'static, str>::Borrowed(OK_FILENAME));

        let mut filename = Cow::Borrowed(PATH_SEP_FILENAME);
        sanitize_filename(&mut filename);
        assert_eq!(
            filename,
            Cow::<'static, str>::Owned(
                "meli_meli_issues_712_comment_4492@git.meli-email.org".to_string()
            )
        );
        let mut filename = Cow::Borrowed(PATH_SEP_FILENAME);
        sanitize_separator(&mut filename);
        assert_eq!(
            filename,
            Cow::<'static, str>::Owned(
                "meli_meli_issues_712_comment_4492@git.meli-email.org".to_string()
            )
        );

        let mut filename = Cow::Borrowed(EFFED_UP_FILENAME);
        sanitize_filename(&mut filename);
        assert_eq!(
            filename,
            Cow::<'static, str>::Owned(
                "Re:_Some_long_subject_-_-User_Dot._Name-_<user1@example.\
                 com>_Sent_from_my_bPad_2024-09-07,_on_a_sunny_Saturday"
                    .to_string()
            )
        );
    }

    #[test]
    fn test_file_alreadyexists() {
        const S: &str = "hello world";
        let tempdir = tempfile::tempdir().unwrap();

        let initial = File::create_temp_file(
            S.as_bytes(),
            Some("hello.txt"),
            Some(&mut tempdir.path().to_path_buf()),
            None,
            true,
        )
        .unwrap();
        assert_eq!(&initial.read_to_string().unwrap(), S);
        assert!(tempdir.path().try_exists().unwrap());
        let mut new_path = tempdir.path().to_path_buf();
        let new = File::create_temp_file(
            S.as_bytes(),
            Some("hello.txt"),
            Some(&mut new_path),
            None,
            true,
        )
        .unwrap();
        assert_eq!(&new.read_to_string().unwrap(), S);
        assert_eq!(&new.path(), &new_path);
        let ext = new_path.extension().unwrap().to_string_lossy().into_owned();
        let stem = new_path.file_stem().unwrap().to_string_lossy().into_owned();
        assert_eq!(stem, "hello_0");
        assert_eq!(ext, "txt");
        let mut new_path = tempdir.path().to_path_buf();
        let _new_2 = File::create_temp_file(
            S.as_bytes(),
            Some("hello.txt"),
            Some(&mut new_path),
            None,
            true,
        )
        .unwrap();
        let ext = new_path.extension().unwrap().to_string_lossy().into_owned();
        let stem = new_path.file_stem().unwrap().to_string_lossy().into_owned();
        assert_eq!(stem, "hello_1");
        assert_eq!(ext, "txt");

        _ = tempdir.close();
    }
}
