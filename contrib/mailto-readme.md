<!-- SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later -->
## Using `meli` for `mailto:` links

To use `meli` to open `mailto:` links from your browser place the `mailto-meli` and `mailto-meli-expect` into `/usr/bin`
(or `.local/bin`, and adjust the path in the script accordingly). Then set `mailto-meli` as program to open `mailto` links
in your browser.

E.g. in Firefox this can be done under

```text
Settings -> Applications -> Content-Type: mailto.
```

Note that you need to have the [`expect`](https://en.wikipedia.org/wiki/Expect) binary installed for this to work.
