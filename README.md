# meli  ![Established, created in 2017](https://img.shields.io/badge/Est.-2017-blue) ![Minimum Supported Rust Version](https://img.shields.io/badge/MSRV-1.70.0-blue) [![GitHub license](https://img.shields.io/github/license/meli/meli)](https://github.com/meli/meli/blob/master/COPYING) [![Crates.io](https://img.shields.io/crates/v/meli)](https://crates.io/crates/meli) [![IRC channel](https://img.shields.io/badge/irc.oftc.net-%23meli-blue)](ircs://irc.oftc.net:6697/%23meli)

**BSD/Linux/macos terminal email client with support for multiple accounts and Maildir / mbox / notmuch / IMAP / JMAP / NNTP (Usenet).**

Try an [old, outdated but online and interactive web demo](https://meli-email.org/wasm2.html "online interactive web demo") powered by WebAssembly!

* `#meli` on OFTC IRC
* [Mailing lists](https://lists.meli-email.org/)
* Main repository <https://git.meli-email.org/meli/meli> Report bugs and/or feature requests in [meli's issue tracker](https://git.meli-email.org/meli/meli/issues "meli gitea issue tracker")<details><summary>Official git mirrors</summary>
  - <https://codeberg.org/meli/meli>
  - <https://github.com/meli/meli>
  - <https://ayllu-forge.org/meli/meli>
  - <https://gitlab.com/meli-project/meli>
  </details>

**Table of contents**:

- [Install](#install)
- [Build](#build)
- [Quick start](#quick-start)
- [Supported E-mail backends](#supported-e-mail-backends)
- [E-mail submission backends](#e-mail-submission-backends)
- [Non-exhaustive list of features](#non-exhaustive-list-of-features)
- [HTML Rendering](#html-rendering)
- [Documentation](#documentation)

## Install

<a href="https://repology.org/project/meli/versions">
  <img src="https://repology.org/badge/vertical-allrepos/meli.svg" alt="Packaging status table by repology.org" align="right">
</a>

- Crates.io with `cargo` on all supported systems and architectures <https://crates.io/crates/meli>

  ![Crates.io](https://img.shields.io/crates/v/meli)
  ```sh
  cargo install meli
  ```
  Install latest development snapshot from git repository:
  ```sh
  cargo install --git https://git.meli-email.org/meli/meli.git meli
  ```
- Official Debian (and Debian derivatives) packages <https://packages.debian.org/trixie/meli>

  ![Debian 13 package](https://repology.org/badge/version-for-repo/debian_13/meli.svg) ![Ubuntu 25.04 package](https://repology.org/badge/version-for-repo/ubuntu_25_04/meli.svg) ![Raspbian Testing package](https://repology.org/badge/version-for-repo/raspbian_testing/meli.svg)
  ```sh
  apt install meli
  ```
- AUR (archlinux) <https://aur.archlinux.org/packages/meli>

  ![AUR package](https://repology.org/badge/version-for-repo/aur/meli.svg)
- OpenSUSE <https://build.opensuse.org/package/show/openSUSE:Factory/meli>

  ![openSUSE Tumbleweed package](https://repology.org/badge/version-for-repo/opensuse_tumbleweed/meli.svg)
- Alpine Linux <https://pkgs.alpinelinux.org/packages?name=meli>

  ![Alpine Linux Edge package](https://repology.org/badge/version-for-repo/alpine_edge/meli.svg)
  ```sh
  apk install meli
  ```
- NetBSD with pkgsrc <https://pkgsrc.se/mail/meli>

  ![pkgsrc current package](https://repology.org/badge/version-for-repo/pkgsrc_current/meli.svg)
- OpenBSD ports <https://openports.pl/path/mail/meli>

  ![OpenBSD port](https://repology.org/badge/version-for-repo/openbsd/meli.svg)
- macOS with
  - Homebrew <https://formulae.brew.sh/formula/meli>

    ![Homebrew package](https://repology.org/badge/version-for-repo/homebrew/meli.svg)
    ```sh
    brew install meli
    ```
  - MacPorts <https://ports.macports.org/port/meli/>

    ![MacPorts package](https://repology.org/badge/version-for-repo/macports/meli.svg)
    ```sh
    port install meli
    ```
- Nix with Nixpkgs <https://search.nixos.org/packages?query=meli>

  ![nixpkgs unstable package](https://repology.org/badge/version-for-repo/nix_unstable/meli.svg)
- [Pre-built debian package, static binaries](https://github.com/meli/meli/releases/ "github releases for meli") for <code>amd64</code>, <code>arm64</code> architectures

## Build

Run `make` or `cargo build --release --bin meli`.

For detailed building instructions, see [`BUILD.md`](./BUILD.md)

### Cargo Compile-time Features

`meli` supports opting in and out of features at compile time with cargo features.

The contents of the `default` feature are:

```toml
default = ["sqlite3", "notmuch", "smtp", "dbus-notifications", "gpgme", "cli-docs", "jmap", "static"]
```

A list of all the features and a description for each follows:

| Feature flag                                                  | Dependencies                                                                                 | Notes                                                                                                                                                                                             |
|---------------------------------------------------------------|----------------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| <a name="notmuch-feature">`notmuch`</a>                       | `maildir` feature                                                                            | Provides the *notmuch* backend                                                                                                                                                                    |
| <a name="jmap-feature">`jmap`</a>                             | `http` feature, `url` crate with `serde` feature                                             | Provides the *JMAP* backend                                                                                                                                                                       |
| <a name="smtp-feature">`smtp`</a>                             | `tls` feature                                                                                | Integrated async *SMTP* client                                                                                                                                                                    |
| <a name="sqlite3-feature">`sqlite3`</a>                       | `rusqlite` crate with `bundled-full` feature                                                 | Used in caches                                                                                                                                                                                    |
| <a name="sqlite3-static-feature">`sqlite3-static`</a>         | `rusqlite` crate with `bundled-full` feature                                                 | Same as `sqlite3` feature but provided for consistency and in case `sqlite3` feature stops bundling libsqlite3 statically in the future.                                                          |
| <a name="smtp-trace-feature">`smtp-trace`</a>                 | `smtp` feature                                                                               | Connection trace logs on the `trace` logging level                                                                                                                                                |
| <a name="gpgme-feature">`gpgme`</a>                           |                                                                                              | *GPG* use by dynamically loading `libgpgme.so`                                                                                                                                                    |
| <a name="tls-static-feature">`tls-static`</a>                 | `native-tls` crate with `vendored` feature                                                   | Links with `OpenSSL` statically where it's used                                                                                                                                                   |
| <a name="http-static-feature">`http-static`</a>               | `isahc` crate with `static-curl` feature                                                     | Links with `curl` statically                                                                                                                                                                      |
| <a name="dbus-notifications-feature">`dbus-notifications`</a> | `notify-rust` dependency                                                                     | Uses DBus notifications                                                                                                                                                                           |
| <a name="dbus-static-feature">`dbus-static`</a>               | `notify-rust` dependency and enableds its `d_vendored` feature                               | Includes the dbus library statically.                                                                                                                                                             |
| <a name="cli-docs-feature">`cli-docs`</a>                     | `flate2` dependency                                                                          | Includes the manpage documentation compiled by either `mandoc` or `man` binary to plain text in `meli`'s command line. Embedded documentation can be viewed with the subcommand `meli man [PAGE]` |
| <a name="libz-static-feature">`libz-static`</a>               | `libz-sys` dependency and enables its `static` feature                                       | Allows for the transitive dependency libz (from `curl`) to be linked statically.                                                                                                                  |
| <a name="static-feature">`static`</a>                         | enables `tls-static`, `http-static`, `sqlite3-static`, `dbus-static`, `libz-static` features |                                                                                                                                                                                                   |

## Quick start

```sh
# Create configuration file in ${XDG_CONFIG_HOME}/meli/config.toml:
$ meli create-config
# Edit configuration in ${EDITOR} or ${VISUAL}:
$ meli edit-config
# Optionally, install manual pages if installed via cargo:
$ meli install-man
# Ready to go.
$ meli
# You can read any manual page with the CLI subcommand `man`:
$ meli man meli.7
# See help output for all options and subcommands.
$ meli --help
```

See a comprehensive tour of `meli` in the manual page [`meli(7)`](./meli/docs/meli.7).

See also the [Quickstart tutorial](https://meli-email.org/documentation.html#quick-start) online.

After installing `meli`, see `meli(1)`, `meli.conf(5)`, `meli(7)` and `meli-themes(5)` for documentation.
Sample configuration and theme files can be found in the `meli/docs/samples/` subdirectory.
Examples for configuration file settings can be found in `meli.conf.examples(5)`
Manual pages are also [hosted online](https://meli-email.org/documentation.html "meli documentation").
`meli` by default looks for a configuration file in this location: `${XDG_CONFIG_HOME}/meli/config.toml`.

You can run meli with arbitrary configuration files by setting the `${MELI_CONFIG}` environment variable to their locations, i.e.:

```sh
MELI_CONFIG=./test_config cargo run
```

See [`meli(7)`](./meli/docs/meli.7) for an extensive tutorial and [`meli.conf(5)`](./meli/docs/meli.conf.5) for all configuration values.

| Main view | Compact main view | Compose with embed terminal editor |
|-----------|-------------------|------------------------------------|
| ![Main view screenshot](./meli/docs/screenshots/main.webp "mail meli view screenshot") | ![Compact main view screenshot](./meli/docs/screenshots/compact.webp "compact main view screenshot") | ![Compose with embed terminal editor screenshot](./meli/docs/screenshots/compose.webp "composing view screenshot") |

### Supported E-mail backends

| Protocol      | Support    |
|---------------|------------|
| IMAP          | full       |
| Maildir       | full       |
| notmuch       | full[^0]   |
| mbox          | read-only  |
| JMAP          | functional |
| NNTP / Usenet | functional |

[^0]: there's no support for searching through all email directly, you'd have to
      create a mailbox with a notmuch query that returns everything and search
      inside that mailbox.

### E-mail submission backends

- SMTP
- Pipe to shell script
- Server-side submission when supported

### Non-exhaustive list of features

- TLS
- email threading support
- multithreaded, async operation
- optionally run your editor of choice inside meli, with an embedded
  xterm-compatible terminal emulator
- plain text configuration in TOML
- ability to open emails in UI tabs and switch to them
- optional sqlite3 index search
- override almost any setting per mailbox, per account
- contact list (+read-only vCard and mutt alias file support)
- forced UTF-8 (other encodings are read-only)
- configurable shortcuts
- theming
- `NO_COLOR` support
- ascii-only drawing characters option
- view text/html attachments through an html filter command (w3m by default)
- pipe attachments/mail to stuff
- use external attachment file picker instead of typing in an attachment's full path
- GPG signing, encryption, signing + encryption
- GPG signature verification

## HTML Rendering

HTML rendering is achieved using [w3m](https://github.com/tats/w3m) by default.
You can use the `pager.html_filter` setting to override this (for more details you can consult [`meli.conf(5)`](./meli/docs/meli.conf.5)).


## Documentation

See a comprehensive tour of `meli` in the manual page [`meli(7)`](./meli/docs/meli.7).

See also the [Quickstart tutorial](https://meli-email.org/documentation.html#quick-start) online.

After installing `meli`, see `meli(1)`, `meli.conf(5)`, `meli(7)` and `meli-themes(5)` for documentation.
Sample configuration and theme files can be found in the `meli/docs/samples/` subdirectory.
Manual pages are also [hosted online](https://meli-email.org/documentation.html "meli documentation").

`meli` by default looks for a configuration file in this location: `${XDG_CONFIG_HOME}/meli/config.toml`

You can run meli with arbitrary configuration files by setting the `${MELI_CONFIG}` environment variable to their locations, or use the `[-c, --config]` argument:

```sh
MELI_CONFIG=./test_config meli
```

or

```sh
meli -c ./test_config
```
