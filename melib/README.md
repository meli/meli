<!-- SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later -->
# melib

[![GitHub license](https://img.shields.io/github/license/meli/meli)](https://github.com/meli/meli/blob/master/COPYING) [![Crates.io](https://img.shields.io/crates/v/melib)](https://crates.io/crates/melib) [![docs.rs](https://docs.rs/melib/badge.svg)](https://docs.rs/melib)

Library for handling email for email clients.

## Cargo Compile-time Features

`melib` supports opting in and out of features at compile time with cargo features.

The contents of the `default` feature are:

```toml
default = ["imap", "nntp", "maildir", "mbox-notify", "smtp"]
```

A list of all the features and a description for each follows:

| Feature flag                                    | Dependencies                                     | Notes                                                |
|-------------------------------------------------|--------------------------------------------------|------------------------------------------------------|
| <a name="maildir-feature">`maildir`</a>         | `notify` crate                                   | Provides the *maildir* backend                       |
| <a name="mbox-notify-feature">`mbox-notify`</a> | `notify` crate                                   | Provides notification support for the *mbox* backend |
| <a name="notmuch-feature">`notmuch`</a>         | `maildir` feature                                | Provides the *notmuch* backend                       |
| <a name="imap-feature">`imap`</a>               | `imap-codec` crate, `tls` feature                | Provides the *IMAP* backend                          |
| <a name="jmap-feature">`jmap`</a>               | `http` feature, `url` crate with `serde` feature | Provides the *JMAP* backend                          |
| <a name="nntp-feature">`nntp`</a>               | `tls` feature                                    | Provides the *NNTP* (Usenet) backend                 |
| <a name="smtp-feature">`smtp`</a>               | `tls` feature                                    | Integrated async *SMTP* client                       |
| <a name="sqlite3-feature">`sqlite3`</a>         | `rusqlite` crate with `bundled-full` feature     | Used in caches                                       |
| <a name="gpgme-feature">`gpgme`</a>             |                                                  | *GPG* use by dynamically loading `libgpgme.so`       |
| <a name="http-feature">`http`</a>               | `isahc` crate                                    | Used for *HTTP* client needs, notably JMAP`          |
| <a name="tls-feature">`tls`</a>                 | `native-tls` crate                               |                                                      |
| <a name="http-static-feature">`http-static`</a> | `isahc` crate with `static-curl` feature         | Links with `curl` statically                         |
| <a name="tls-static-feature">`tls-static`</a>   | `native-tls` crate with `vendored` feature       | Links with `OpenSSL` statically where it's used      |
| <a name="imap-trace-feature">`imap-trace`</a>   | `imap` feature                                   | Connection trace logs on the `trace` logging level   |
| <a name="jmap-trace-feature">`jmap-trace`</a>   | `jmap` feature                                   | Connection trace logs on the `trace` logging level   |
| <a name="nntp-trace-feature">`nntp-trace`</a>   | `nntp` feature                                   | Connection trace logs on the `trace` logging level   |
| <a name="smtp-trace-feature">`smtp-trace`</a>   | `smtp` feature                                   | Connection trace logs on the `trace` logging level   |

## Example: Parsing bytes into an `Envelope`

An `Envelope` represents the information you can get from an email's headers
and body structure. Addresses in `To`, `From` fields etc are parsed into
`Address` types.

```rust
use melib::{email::attachment_types::Text, Attachment, Envelope};

let raw_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: =?utf-8?Q?gratuitously_encoded_subject?=
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
MIME-Version: 1.0
Content-Type: multipart/mixed; charset="utf-8";
 boundary="bzz_bzz__bzz__"

This is a MIME formatted message with attachments. Use a MIME-compliant client to view it properly.
--bzz_bzz__bzz__

hello world.
--bzz_bzz__bzz__
Content-Type: image/gif; name="test_image.gif"; charset="utf-8"
Content-Disposition: attachment
Content-Transfer-Encoding: base64

R0lGODdhKAAXAOfZAAABzAADzQAEzgQFtBEAxAAGxBcAxwALvRcFwAAPwBcLugATuQEUuxoNuxYQ
sxwOvAYVvBsStSAVtx8YsRUcuhwhth4iuCQsyDAwuDc1vTc3uDg4uT85rkc9ukJBvENCvURGukdF
wUVKt0hLuUxPvVZSvFlYu1hbt2BZuFxdul5joGhqlnNuf3FvlnBvwXJyt3Jxw3N0oXx1gH12gV99
z317f3N7spFxwHp5wH99gYB+goF/g25+26tziIOBhWqD3oiBjICAuudkjIN+zHeC2n6Bzc1vh4eF
iYaBw8F0kImHi4KFxYyHmIWIvI2Lj4uIvYaJyY+IuJGMi5iJl4qKxZSMmIuLxpONnpGPk42NvI2M
1LKGl46OvZePm5ORlZiQnJqSnpaUmLyJnJuTn5iVmZyUoJGVyZ2VoZSVw5iXoZmWrO18rJiUyp6W
opuYnKaVnZ+Xo5yZncaMoaCYpJiaqo+Z2Z2annuf5qGZpa2WoJybpZmayZ2Z0KCZypydrZ6dp6Cd
oZ6a0aGay5ucy5+eqKGeouWMgp+b0qKbzKCfqdqPnp2ezaGgqqOgpKafqrScpp+gz6ajqKujr62j
qayksKmmq62lsaiosqqorOyWnaqqtKeqzLGptaurta2rr7Kqtq+ssLOrt6+uuLGusuqhfbWtubCv
ubKvs7GwurOwtPSazbevu+ali7SxtbiwvOykjLOyvLWytuCmqOankrSzvbazuLmyvrW0vre0uba1
wLi1ury0wLm2u721wbe3wbq3vMC2vLi4wr+3w7m5w8C4xLi6yry6vsG5xbu7xcC6zMK6xry8xry+
u8O7x729x8C9wb++yMG+wsO+vMK/w8a+y8e/zMnBzcXH18nL2///////////////////////////
////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////ywAAAAAKAAXAAAI/gBP4Cjh
IYMLEh0w4EgBgsMLEyFGFBEB5cOFABgzatS4AVssZAOsLOHCxooVMzCyoNmzaBOkJlS0VEDyZMjG
mxk3XOMF60CDBgsoPABK9KcDCRImPCiQYAECAgQCRMU4VSrGCjFarBgUSJCgQ10FBTrkNRCfPnz4
dA3UNa1btnDZqgU7Ntqzu3ej2X2mFy9eaHuhNRtMGJrhwYYN930G2K7eaNIY34U2mfJkwpgzI9Yr
GBqwR2KSvAlMOXHnw5pTNzPdLNoWIWtU9XjGjDEYS8LAlFm1SrVvzIKj5TH0KpORSZOryPgCZgqL
Ob+jG0YVRBErUrOiiGJ8KxgtYsh27xWL/tswnTtEbsiRVYdJNMHk4yOGhswGjR88UKjQ9Ey+/8TL
XKKGGn7Akph/8XX2WDTTcAYfguVt9hhrEPqmzIOJ3VUheb48WJiHG6amC4i+WVJKKCimqGIoYxyj
WWK8kKjaJ9bA18sxvXjYhourmbbMMrjI+OIn1QymDCVXANGFK4S1gQw0PxozzC+33FLLKUJq9gk1
gyWDhyNwrMLkYGUEM4wvuLRiCiieXIJJJVlmJskcZ9TZRht1lnFGGmTMkMoonVQSSSOFAGJHHI0w
ouiijDaaCCGQRgrpH3q4QYYXWDihxBE+7KCDDjnUIEVAADs=
--bzz_bzz__bzz__--"#;

let envelope = Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
assert_eq!(envelope.subject().as_ref(), "gratuitously encoded subject");
assert_eq!(envelope.message_id(), "h2g7f.z0gy2pgaen5m@example.com");
assert_eq!(&envelope.message_id().display_brackets().to_string(), "<h2g7f.z0gy2pgaen5m@example.com>");

let body = envelope.body_bytes(raw_mail.as_bytes());
assert_eq!(body.content_type().to_string().as_str(), "multipart/mixed");

let body_text = body.text(Text::Plain);
assert_eq!(body_text.as_str(), "hello world.");

let subattachments: Vec<Attachment> = body.attachments();
assert_eq!(subattachments.len(), 3);
assert_eq!(
    subattachments[2].content_type().name().unwrap(),
    "test_image.gif"
);
```
