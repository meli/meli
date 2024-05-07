# Build `meli`

For a quick start, build and install locally:

```sh
PREFIX=~/.local make install
```

Available subcommands for `make` are listed with `make help`.
The Makefile *should* be POSIX portable and not require a specific `make` version.

`meli` requires rust version 1.70.0 or later and rust's package manager, Cargo.
Information on how to get it on your system can be found here: <https://doc.rust-lang.org/cargo/getting-started/installation.html>

With Cargo available, the project can be built with `make` and the resulting binary will then be found under `target/release/meli`.
Run `make install` to install the binary and man pages.
This requires root, so I suggest you override the default paths and install it in your `$HOME`: `make PREFIX=${HOME}/.local install`.

You can build and run `meli` with one command: `cargo run --release`.

## Build features

Some functionality is held behind "feature gates", or compile-time flags. The following list explains each feature's purpose:

- `gpgme` enables GPG support via `libgpgme` (on by default)
- `dbus-notifications` enables showing notifications using `dbus` (on by default)
- `notmuch` provides support for using a notmuch database as a mail backend (on by default)
- `jmap` provides support for connecting to a jmap server and use it as a mail backend (on by default)
- `sqlite3` provides support for builting fast search indexes in local sqlite3 databases (on by default)
- `cli-docs` includes the manpage documentation compiled by either `mandoc` or `man` binary to plain text in `meli`'s command line. Embedded documentation can be viewed with the subcommand `meli man [PAGE]` (on by default).
- `regexp` provides experimental support for theming some e-mail fields based
  on regular expressions.
  It uses the `pcre2` library.
  Since it's actual use in the code is very limited, it is not recommended to use this (off by default).
- `static` and `*-static` bundle C libraries in dependencies so that you don't need them installed in your system (on by default).

Though not a feature, the presence of the environment variable `UNICODE_REGENERATE_TABLES` in compile-time of the `melib` crate will force the regeneration of unicode tables.
Otherwise the tables are included with the source code, and there's no real reason to regenerate them unless you intend to modify the code or update to a new Unicode version.

## Build Debian package (*deb*)

Building with Debian's packaged cargo might require the installation of these two packages: `librust-openssl-sys-dev librust-libdbus-sys-dev`

A `*.deb` package can be built with `make deb-dist`

## Using notmuch

To use the optional notmuch backend feature, you must have `libnotmuch5` installed in your system.
In Debian-like systems, install the `libnotmuch5` packages.
`meli` detects the library's presence on runtime.
If it is not detected, you can use the `library_file_path` setting on your notmuch account to specify the absolute path of the library.

## Using GPG

To use the optional gpg feature, you must have `libgpgme` installed in your system.
In Debian-like systems, install the `libgpgme11` package.
`meli` detects the library's presence on runtime.

## Development

Development builds can be built and/or run with

```
cargo build
cargo run
```

There is a debug/tracing log feature that can be enabled by using the flag `--feature debug-tracing` after uncommenting the features in `Cargo.toml`.
The logs are printed in stderr when the env var `MELI_DEBUG_STDERR` is defined, thus you can run `meli` with a redirection (i.e `2> log`).

To trace network and protocol communications you can enable the following features:

- `imap-trace`
- `jmap-trace`
- `nntp-trace`
- `smtp-trace`
