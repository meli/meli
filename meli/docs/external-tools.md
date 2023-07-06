# Using other apps with `meli`

## Sending mail with a command line tool

`composing.send_mail` can use either settings for an SMTP server or a shell 
command to which it pipes new mail to.

### `msmtp` and `send_mail`

[`msmtp`][msmtp] is a command line SMTP client that can be configured to work 
with many SMTP servers. It supports queuing and other small useful features. 
See [the documentation](https://marlam.de/msmtp/msmtp.html).

```toml
[composing]
send_mail = 'msmtp --logfile=/home/user/.mail/msmtp.log --read-recipients 
--read-envelope-from'
```
[msmtp]: https://marlam.de/msmtp/

## Editor

Any editor you specify in `composing.editor_cmd` will be invoked with the 
e-mail draft file path appended as an argument to it. For example, if your 
setting is `editor_cmd = 'nano'`, `meli` will execute `nano /tmp/meli/...`.

### Configuration

#### `vim` / `neovim` command

The following command setting in your `meli` configuration file makes editing 
start at the first empty line, that is, after the e-mail headers. This allows 
you to start writing the e-mail body right away after opening the editor from 
`meli`.

```toml
[composing]
editor_cmd = '~/.local/bin/vim +/^$'
```

In `vim`, the `+` argument positions the cursor at the first file argument. `/` 
specifies a pattern position instead of a line number. `^` specifies the start 
of a line, and `$` the end of the line. The pattern altogether matches an empty 
line, which will be after the e-mail headers.

### Composing with `format=flowed`

`format=flowed` is a proposed IETF standard[^formatflowed] that lets you 
preserve the structure of paragraphs by disambiguating a *hard* and a *soft* 
line break. A line break that is preceded by a space character is *soft* and 
does not terminate the paragraph, while a line break without a space is a 
*hard* one and creates a new paragraph. This allows text to be re-flowed in 
e-mail clients at different display widths and font sizes without messing up 
the author's formatting.

#### `vim` / `neovim` and `format=flowed`

Create a `mail.vim` file type plugin in:

- `$HOME/.vim/after/ftplugin/mail.vim` for vim
- `$HOME/.config/nvim/after/ftplugin/mail.vim` for neovim

```vim
setlocal nomodeline
setlocal textwidth=72
setlocal formatoptions=aqtw2r
setlocal nojoinspaces
setlocal nosmartindent
setlocal comments+=nb:>
match ErrorMsg '\s\+$'
```

Also, don't forget that you can easily quote stuff with `MailQuote`.
From `:help ft-mail-plugin`:

> Local mappings:
> `<LocalLeader>q`   or   `\\MailQuote`
>   Quotes the text selected in Visual mode, or from the cursor position
>   to the end of the file in Normal mode.
>   This means "> " is inserted in each line.

See the accompanying [`mail.vim`](./mail.vim) for comments for each setting.

## `xbiff`

[`xbiff(1)`][xbiff] manual page says:[^xbiffmanpage]

> The `xbiff` program displays a little image of a mailbox. When there is no
> mail, the flag on the mailbox is down. When mail arrives, the flag goes up
> and the mailbox beeps.

This tool is very outdated, but some users might still have use for it. 
Therefore `meli` provides support (also, it's easy to support this feature).

Specify a file path in `notifications.xbiff_file_path` and `meli` will write to 
it when new mail arrives. This file can the be used as input to `xbiff`.

```toml
[notifications]
xbiff_file_path = "/tmp/xbiff"
```

[xbiff]: https://en.wikipedia.org/wiki/Xbiff
[^xbiffmanpage]: https://www.x.org/releases/X11R7.0/doc/html/xbiff.1.html

## Viewing HTML e-mail

By default `meli` tries to render HTML e-mail with `w3m`. You can override this 
by setting the `pager.html_filter` setting. The default setting corresponds to:

```toml
[pager]
html_filter = "w3m -I utf-8 -T text/html"
```

The HTML of the e-mail is piped into `html_filter`'s standard input.

## Externally refreshing e-mail accounts

If your account's syncing is handled by an external tool, you can use the 
refresh shortcuts within `meli` to call this tool with 
`accounts.refresh_command`.
