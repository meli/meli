/*
 * meli - build.rs
 *
 * Copyright 2020  Manos Pitsidianakis
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

extern crate proc_macro;
extern crate quote;
extern crate syn;
include!("config_macros.rs");

fn main() {
    println!("cargo:rerun-if-changed=src/conf/.rebuild.overrides.rs");
    override_derive(&[
        ("src/conf/pager.rs", "PagerSettings"),
        ("src/conf/listing.rs", "ListingSettings"),
        ("src/conf/notifications.rs", "NotificationsSettings"),
        ("src/conf/shortcuts.rs", "Shortcuts"),
        ("src/conf/composing.rs", "ComposingSettings"),
        ("src/conf/tags.rs", "TagsSettings"),
        ("src/conf/pgp.rs", "PGPSettings"),
    ]);
    #[cfg(feature = "cli-docs")]
    {
        use flate2::{Compression, GzBuilder};
        const MANDOC_OPTS: &[&str] = &["-T", "utf8", "-I", "os=Generated by mandoc(1)"];
        use std::{env, io::prelude::*, path::Path};

        let out_dir = env::var("OUT_DIR").unwrap();
        let mut out_dir_path = Path::new(&out_dir).to_path_buf();

        let mut cl = |filepath: &str, output: &str, source: bool| {
            out_dir_path.push(output);
            let output = if source {
                std::fs::read_to_string(filepath).unwrap().into_bytes()
            } else {
                let output = Command::new("mandoc")
                    .args(MANDOC_OPTS)
                    .arg(filepath)
                    .output()
                    .or_else(|_| Command::new("man").arg("-l").arg(filepath).output())
                    .expect(
                        "could not execute `mandoc` or `man`. If the binaries are not available \
                         in the PATH, disable `cli-docs` feature to be able to continue \
                         compilation.",
                    );
                output.stdout
            };

            let file = File::create(&out_dir_path).unwrap_or_else(|err| {
                panic!("Could not create file {}: {}", out_dir_path.display(), err)
            });
            let mut gz = GzBuilder::new()
                .comment(output.len().to_string().into_bytes())
                .write(file, Compression::default());
            gz.write_all(&output).unwrap();
            gz.finish().unwrap();
            out_dir_path.pop();
        };

        cl("docs/meli.1", "meli.txt.gz", false);
        cl("docs/meli.conf.5", "meli.conf.txt.gz", false);
        cl(
            "docs/meli.conf.examples.5",
            "meli.conf.examples.txt.gz",
            false,
        );
        cl("docs/meli-themes.5", "meli-themes.txt.gz", false);
        cl("docs/meli.7", "meli.7.txt.gz", false);
        cl("docs/meli.1", "meli.mdoc.gz", true);
        cl("docs/meli.conf.5", "meli.conf.mdoc.gz", true);
        cl(
            "docs/meli.conf.examples.5",
            "meli.conf.examples.mdoc.gz",
            true,
        );
        cl("docs/meli-themes.5", "meli-themes.mdoc.gz", true);
        cl("docs/meli.7", "meli.7.mdoc.gz", true);
    }
}
