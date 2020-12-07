# meli [![GitHub license](https://img.shields.io/github/license/meli/meli)](https://github.com/meli/meli/blob/master/COPYING) [![Crates.io](https://img.shields.io/crates/v/meli)](https://crates.io/crates/meli)

**BSD/Linux terminal email client with support for multiple accounts and Maildir / mbox / notmuch / IMAP / JMAP.**

Community links:
[mailing lists](https://lists.meli.delivery/) | `#meli` on freenode IRC | Report bugs and/or feature requests in [meli's issue tracker](https://git.meli.delivery/meli/meli/issues "meli gitea issue tracker")

| | | |
:---:|:---:|:---:
![Main view screenshot](./docs/screenshots/main.webp "mail meli view screenshot")  |  ![Compact main view screenshot](./docs/screenshots/compact.webp "compact main view screenshot") | ![Compose with embed terminal editor screenshot](./docs/screenshots/compose.webp "composing view screenshot")
Main view             |  Compact main view | Compose with embed terminal editor

Main repository:
* https://git.meli.delivery/meli/meli

Official mirrors:
* https://github.com/meli/meli

## Install
- Try an [online interactive web demo](https://meli.delivery/wasm2.html "online interactive web demo") powered by WebAssembly
- [`cargo install meli`](https://crates.io/crates/meli "crates.io meli package")
- [Download and install pre-built debian package, static linux binary](https://github.com/meli/meli/releases/ "github releases for meli"), or
- Install with [Nix](https://search.nixos.org/packages?show=meli&query=meli&from=0&size=30&sort=relevance&channel=unstable#disabled "nixos package search results for 'meli'")

## Documentation

See also [Quickstart tutorial](https://meli.delivery/documentation.html#quick-start).

After installing meli, see `meli(1)`, `meli.conf(5)` and `meli-themes(5)` for documentation. Sample configuration and theme files can be found in the `docs/samples/` subdirectory. Manual pages are also [hosted online](https://meli.delivery/documentation.html "meli documentation").

meli by default looks for a configuration file in this location: `$XDG_CONFIG_HOME/meli/config.toml`

You can run meli with arbitrary configuration files by setting the `$MELI_CONFIG`
environment variable to their locations, i.e.:

```sh
MELI_CONFIG=./test_config cargo run
```

## Build
For a quick start, build and install locally:

```sh
 PREFIX=~/.local make install
```

Available subcommands for `make` are listed with `make help`. The Makefile *should* be POSIX portable and not require a specific `make` version.

meli requires rust 1.39 and rust's package manager, Cargo. Information on how
to get it on your system can be found here: <https://doc.rust-lang.org/cargo/getting-started/installation.html>

With Cargo available, the project can be built with `make` and the resulting binary will then be found under `target/release/meli`. Run `make install` to install the binary and man pages. This requires root, so I suggest you override the default paths and install it in your `$HOME`: `make PREFIX=$HOME/.local install`.

You can build and run meli with one command: `cargo run --release`.

### Build features

Some functionality is held behind "feature gates", or compile-time flags. The following list explains each feature's purpose:

- `gpgme` enables GPG support via `libgpgme` (on by default)
- `dbus-notifications` enables showing notifications using `dbus` (on by default)
- `notmuch` provides support for using a notmuch database as a mail backend (on by default)
- `jmap` provides support for connecting to a jmap server and use it as a mail backend (off by default)
- `sqlite3` provides support for builting fast search indexes in local sqlite3 databases (on by default)
- `cli-docs` includes the manpage documentation compiled by either `mandoc` or `man` binary to plain text in `meli`'s command line. Embedded documentation can be viewed with the subcommand `meli man [PAGE]`
- `svgscreenshot` provides support for taking screenshots of the current view of meli and saving it as SVG files. Its only purpose is taking screenshots for the official meli webpage. (off by default)
- `debug-tracing` enables various trace debug logs from various places around the meli code base. The trace log is printed in `stderr`. (off by default)

### Build Debian package (*deb*)

Building with Debian's packaged cargo might require the installation of these
two packages: `librust-openssl-sys-dev librust-libdbus-sys-dev`

A `*.deb` package can be built with `make deb-dist`

### Using notmuch

To use the optional notmuch backend feature, you must have `libnotmuch5` installed in your system. In Debian-like systems, install the `libnotmuch5` packages. meli detects the library's presence on runtime.

### Using GPG

To use the optional gpg feature, you must have `libgpgme` installed in your system. In Debian-like systems, install the `libgpgme11` package. meli detects the library's presence on runtime.

### Building with JMAP

To build with JMAP support, prepend the environment variable `MELI_FEATURES='jmap'` to your make invocation:

```sh
MELI_FEATURES="jmap" make
```

or if building directly with cargo, use the flag `--features="jmap"'.

# Development

Development builds can be built and/or run with

```
cargo build
cargo run
```

There is a debug/tracing log feature that can be enabled by using the flag
`--feature debug-tracing` after uncommenting the features in `Cargo.toml`. The logs
are printed in stderr, thus you can run meli with a redirection (i.e `2> log`)

Code style follows the default rustfmt profile.

## Testing

How to run specific tests:

```sh
cargo test -p {melib, meli} (-- --nocapture) (--test test_name)
```

## Profiling

```sh
perf record -g target/debug/bin
perf script | stackcollapse-perf | rust-unmangle | flamegraph > perf.svg
```

## Running fuzz targets

Note: `cargo-fuzz` requires the nightly toolchain.

```sh
cargo +nightly fuzz run envelope_parse -- -dict=fuzz/envelope_tokens.dict
```
