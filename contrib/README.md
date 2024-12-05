<!-- SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later -->
# Useful scripts and files for use with `meli`

This directory includes various useful scripts and files that are contributed
by the community and not actively maintained or supported.

If you believe something in this directory needs updates to work with the
current version of `meli` or there are bugs that need fixing, please file an
issue on our issue tracker!

## Using `meli` for `mailto:` links

To use `meli` to open `mailto:` links from your browser place the `mailto-meli` and `mailto-meli-expect` into `/usr/bin`
(or `.local/bin`, and adjust the path in the script accordingly). Then set `mailto-meli` as program to open `mailto` links
in your browser.

E.g. in Firefox this can be done under

```text
Settings -> Applications -> Content-Type: mailto.
```

Note that you need to have the [`expect`](https://en.wikipedia.org/wiki/Expect) binary installed for this to work.
