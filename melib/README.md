# melib

[![GitHub license](https://img.shields.io/github/license/meli/meli)](https://github.com/meli/meli/blob/master/COPYING) [![Crates.io](https://img.shields.io/crates/v/melib)](https://crates.io/crates/melib) [![docs.rs](https://docs.rs/melib/badge.svg)](https://docs.rs/melib)

Library for handling email for email clients.

## Optional Features

| Feature flag  | Dependencies                                     | Notes                                                |
|---------------|--------------------------------------------------|------------------------------------------------------|
| `maildir`     | `notify` crate                                   | Provides the *maildir* backend                       |
| `mbox-notify` | `notify` crate                                   | Provides notification support for the *mbox* backend |
| `notmuch`     | `maildir` feature                                | Provides the *notmuch* backend                       |
| `imap`        | `imap-codec` crate, `tls` feature                | Provides the *IMAP* backend                          |
| `jmap`        | `http` feature, `url` crate with `serde` feature | Provides the *JMAP* backend                          |
| `nntp`        | `tls` feature                                    | Provides the *NNTP* (Usenet) backend                 |
| `smtp`        | `tls` feature                                    | Integrated async *SMTP* client                       |
| `sqlite3`     | `rusqlite` crate with `bundled-full` feature     | Used in caches                                       |
| `gpgme`       |                                                  | *GPG* use by dynamically loading `libgpgme.so`       |
| `http`        | `isahc` crate                                    | Used for *HTTP* client needs, notably JMAP`          |
| `tls`         | `native-tls` crate                               |                                                      |
| `http-static` | `isahc` crate with `static-curl` feature         | Links with `curl` statically                         |
| `tls-static`  | `native-tls` crate with `vendored` feature       | Links with `OpenSSL` statically where it's used      |
| `imap-trace`  | `imap` feature                                   | Connection trace logs on the `trace` logging level   |
| `jmap-trace`  | `jmap` feature                                   | Connection trace logs on the `trace` logging level   |
| `nntp-trace`  | `nntp` feature                                   | Connection trace logs on the `trace` logging level   |
| `smtp-trace`  | `smtp` feature                                   | Connection trace logs on the `trace` logging level   |

## Example: Parsing bytes into an `Envelope`

An `Envelope` represents the information you can get from an email's headers
and body structure. Addresses in `To`, `From` fields etc are parsed into
`Address` types.

```rust
use melib::{Attachment, Envelope};

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
assert_eq!(&envelope.message_id().display_bracket().to_string(), "<h2g7f.z0gy2pgaen5m@example.com>");

let body = envelope.body_bytes(raw_mail.as_bytes());
assert_eq!(body.content_type().to_string().as_str(), "multipart/mixed");

let body_text = body.text();
assert_eq!(body_text.as_str(), "hello world.");

let subattachments: Vec<Attachment> = body.attachments();
assert_eq!(subattachments.len(), 3);
assert_eq!(subattachments[2].content_type().name().unwrap(), "test_image.gif");
```
