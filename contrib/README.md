<!-- SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later -->
# Useful scripts and files for use with `meli`

This directory includes various useful scripts and files that are contributed
by the community and not actively maintained or supported.

If you believe something in this directory needs updates to work with the
current version of `meli` or there are bugs that need fixing, please file an
issue on our issue tracker!

## Connecting to a Gmail account with OAUTH2

The script [`./oauth2.py`](./oauth2.py) is a helper script to authenticate to a Gmail account using IMAP OAUTH2 tokens.

See [`meli.conf(5)`](../meli/docs/meli.conf.5) for documentation.

If the script does not work and you're certain it's because it needs changes to
work with Google's servers and not a user error on your part, please file a bug
on our issue tracker!

## Using `meli` for `mailto:` links

To use `meli` to open `mailto:` links from your browser place the [`mailto-meli`](./mailto-meli) and [`mailto-meli-expect`](./mailto-meli-expect) scripts into `/usr/bin`
(or `.local/bin`, and adjust the path in the script accordingly).

Ensure all scripts are executable by your user account, if not set the permissions accordingly:

```sh
chmod u+x /path/to/mailto-meli
```

and

```sh
chmod u+x /path/to/mailto-meli-expect
```

Then set `mailto-meli` as program to open `mailto` links
in your browser.

E.g. in Firefox this can be done under "Settings" (`about:preferences`) which you can access from the menu button or `Edit -> Settings`.

```text
General -> Applications -> Content-Type: mailto.
```

You can test that it works by clicking the system menu entry `File -> Email link...`.

_NOTE_: that you need to have the [`expect`](https://en.wikipedia.org/wiki/Expect) binary installed for this to work.
`expect` is a scripting language used for interactive with interactive terminal applications like `meli`.
