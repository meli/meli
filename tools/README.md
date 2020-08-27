# meli tools

This crate holds a collection of small binaries used for meli development. Of note is `imapshell` which opens a shell to an IMAP server for the user to play with.

## imapshell

```shell
cd tools/
cargo build --bin imapshell
# Usage: imap_conn server_hostname server_username server_password server_port
rlwrap ./target/debug/imapshell "mail.domain.tld" "epilys@domain.tld" "hunter2" 143
```

Example session:

First, the IMAP connections performs its own non-interactive setup:

```text
[2020-08-27 17:11:33]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M1 CAPABILITY
[2020-08-27 17:11:33]["main"] melib/src/backends/imap/connection.rs:408_33: &ret[last_line_idx..] = "M1 OK Pre-login capabilities listed, post-login capabilities have more.\r\n"
[2020-08-27 17:11:33]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M2 LOGIN "epilys@domain.tld" "hunter2"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M3 CAPABILITY
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:408_33: &ret[last_line_idx..] = "M3 OK Capability completed (0.000 + 0.120 + 0.119 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M4 ENABLE CONDSTORE
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:408_33: &ret[last_line_idx..] = "M4 OK Enabled (0.000 + 0.120 + 0.119 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/protocol_parser.rs:261_9: &val = "M4 OK Enabled (0.000 + 0.120 + 0.119 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M5 COMPRESS DEFLATE
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:408_33: &ret[last_line_idx..] = "M5 OK Begin compression (0.000 + 0.127 + 0.126 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/protocol_parser.rs:261_9: &val = "M5 OK Begin compression (0.000 + 0.127 + 0.126 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/protocol_parser.rs:261_9: &val = "M5 OK Begin compression (0.000 + 0.127 + 0.126 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M6 NOOP
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/connection.rs:408_33: &ret[last_line_idx..] = "M6 OK NOOP completed (0.000 + 0.000 secs).\r\n"
[2020-08-27 17:11:34]["main"] melib/src/backends/imap/protocol_parser.rs:261_9: &val = "M6 OK NOOP completed (0.000 + 0.000 secs).\r\n"
```

Then, input is read line by line and sent to the server. You don't have to prefix the commands with a unique ID, that is taken care of by the tool. Example command and reply:

```text
LIST "" "INBOX"
[2020-08-27 17:14:53]["main"] melib/src/backends/imap/connection.rs:717_9: send_command
[2020-08-27 17:14:53]["main"] melib/src/backends/imap/connection.rs:437_9: stream send_command()
[2020-08-27 17:14:53]["main"] melib/src/backends/imap/connection.rs:459_25: sent: M8 LIST "" "INBOX"
[2020-08-27 17:14:53]["main"] melib/src/backends/imap/connection.rs:729_13: send_command ok
[2020-08-27 17:14:53]["main"] melib/src/backends/imap.rs:1241_21: out: * LIST (\HasChildren) "." INBOX
M8 OK List completed (0.000 + 0.000 secs).
```
