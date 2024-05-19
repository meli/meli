//
// melib
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use HeaderName as HDR;

use super::*;

#[test]
fn test_email_mailto() {
    macro_rules! addr {
        ($lit:literal) => {
            Address::try_from($lit).unwrap()
        };
    }

    macro_rules! mlt {
        ($lit:literal) => {
            Mailto::try_from($lit).expect("Could not parse mailto link.")
        };
    }

    macro_rules! hdr {
        ($lit:literal) => {
            HeaderName::try_from($lit).expect("Could not parse header name.")
        };
    }

    macro_rules! hdrmap {
            ($(($field:literal, $val:literal)),+) => {{
                let mut m = HeaderMap::empty();
                $(
                    m.insert(hdr!($field), $val.into());
                )+

                m
            }};
        }

    macro_rules! test_case {
            ($mailto:literal, addresses => $($addr:literal),*; body => $body:expr; $(($field:literal, $val:literal)),+) => {{
                let addresses = &[
                    $(
                        addr!($addr)
                    ),*
                ];
                let Mailto {
                    address,
                    body,
                    headers,
                } = mlt!($mailto);
                assert_eq!(
                    (address.as_slice(), body.as_ref().map(|b| b.as_str()), headers),
                    (addresses.as_slice(), $body, hdrmap!($(($field, $val)),*))
                );
            }}
        }

    test_case!("mailto:info@example.com?subject=email%20subject",
        addresses=> "info@example.com";
        body => None;
        ("To", "info@example.com"), ("Subject", "email subject")
    );
    test_case!("mailto:info@example.com?cc=8cc9@example.com",
        addresses=> "info@example.com";
        body => None;
        ("To", "info@example.com"), ("Cc", "8cc9@example.com")
    );
    test_case!("mailto:info@example.com?bcc=7bcc8@example.com&body=line%20first%0Abut%20not%0Alast",
        addresses=> "info@example.com";
        body => Some("line first\nbut not\nlast");
        ("To", "info@example.com"), ("Bcc", "7bcc8@example.com")
    );

    test_case!("mailto:info@example.com?In-Reply-To=%3C20230526204845.673031-1-manos.pitsidianakis@linaro.org%3E&Cc=kraxel%40redhat.com%2Cqemu-devel%40nongnu.org&Subject=Re%3A%20%5BPATCH%5D%20Add%20virtio-sound%20and%20virtio-sound-pci%20devices",
        addresses=> "info@example.com";
        body => None;
        ("To", "info@example.com"), ("Subject", "Re: [PATCH] Add virtio-sound and virtio-sound-pci devices"), ("Cc", "kraxel@redhat.com,qemu-devel@nongnu.org"), ("In-Reply-To", "<20230526204845.673031-1-manos.pitsidianakis@linaro.org>")
    );

    assert_eq!(
        mlt!("mailto:chris@example.com%2C%20tony@example.com"),
        mlt!("mailto:?to=chris@example.com%2C%20tony@example.com")
    );

    /* address plus to= should be ignored */
    assert!(
        Mailto::try_from("mailto:?to=chris@example.com%2C%20tony@example.com")
            != Mailto::try_from("mailto:chris@example.com?to=tony@example.com"),
        "{:?} == {:?}",
        Mailto::try_from("mailto:?to=chris@example.com%2C%20tony@example.com"),
        Mailto::try_from("mailto:chris@example.com?to=tony@example.com")
    );

    //  URLs for an ordinary individual mailing address:
    test_case!("mailto:chris@example.com",
        addresses=> "chris@example.com";
        body => None;
        ("To", "chris@example.com")
    );

    // A URL for a mail response system that requires the name of the file in the
    // subject:

    test_case!("mailto:infobot@example.com?subject=current-issue",
        addresses => "infobot@example.com";
        body => None;
        ("To", "infobot@example.com"), ("Subject", "current-issue")
    );

    // A mail response system that requires a "send" request in the body:

    test_case!("mailto:infobot@example.com?body=send%20current-issue",
        addresses => "infobot@example.com";
        body => Some("send current-issue");
        ("To", "infobot@example.com")
    );

    //A similar URL could have two lines with different "send" requests (in this
    // case, "send current-issue" and, on the next line, "send index".)

    test_case!("mailto:infobot@example.com?body=send%20current-issue%0D%0Asend%20index",
        addresses => "infobot@example.com";
        body => Some("send current-issue\r\nsend index");
        ("To", "infobot@example.com")
    );
    // An interesting use of your mailto URL is when browsing archives of messages.
    // Each browsed message might contain a mailto URL like:

    test_case!("mailto:foobar@example.com?In-Reply-To=%3c3469A91.D10AF4C@example.com%3e",
        addresses => "foobar@example.com";
        body => None;
        ("To", "foobar@example.com"), ("In-Reply-To", "<3469A91.D10AF4C@example.com>")
    );

    // A request to subscribe to a mailing list:

    test_case!("mailto:majordomo@example.com?body=subscribe%20bamboo-l",
        addresses => "majordomo@example.com";
        body => Some("subscribe bamboo-l");
        ("To", "majordomo@example.com")
    );

    // A URL for a single user which includes a CC of another user:

    test_case!("mailto:joe@example.com?cc=bob@example.com&body=hello",
        addresses => "joe@example.com";
        body => Some("hello");
        ("To", "joe@example.com"), ("Cc", "bob@example.com")
    );

    // Another way of expressing the same thing:

    test_case!("mailto:?to=joe@example.com&cc=bob@example.com&body=hello",
        addresses => "joe@example.com";
        body => Some("hello");
        ("To", "joe@example.com"), ("Cc", "bob@example.com")
    );

    //    Note the use of the "&" reserved character, above. The following example,
    // by using "?" twice, is incorrect: <mailto:joe@example.com?cc=bob@
    // example.com?body=hello>   ; WRONG!

    Mailto::try_from("mailto:joe@example.com?cc=bob@example.com?body=hello").unwrap_err();

    // <a href="mailto:?to=joe@xyz.com&amp;cc=bob@xyz.com&amp;body=hello"> assert
    // these are equal

    test_case!("mailto:?to=joe@example.com&amp;cc=bob@example.com&amp;body=hello",
        addresses => "joe@example.com";
        body => Some("hello");
        ("To", "joe@example.com"), ("Cc", "bob@example.com")
    );

    // To indicate the address "gorby%kremvax@example.com" one would do:
    // <mailto:gorby%25kremvax@example.com>

    test_case!("mailto:gorby%25kremvax@example.com",
        addresses => "gorby%kremvax@example.com";
        body => None;
        ("To", "gorby%kremvax@example.com")
    );

    // Custom header is ignored
    // <mailto:address@example.com?blat=foop>

    test_case!("mailto:address@example.com?blat=foop",
        addresses => "address@example.com";
        body => None;
        ("To", "address@example.com")
    );

    // 6.2.  Examples of Complicated Email Addresses

    assert_eq!(
        mlt!("mailto:%22not%40me%22@example.org").address,
        vec![addr!(r#""not@me"@example.org"#)]
    );

    // Email address: "oh\\no"@example.org; corresponding 'mailto' URI:

    // <mailto:%22oh%5C%5Cno%22@example.org>.
    assert_eq!(
        mlt!("mailto:%22not%40me%22@example.org").address,
        vec![addr!(r#""not@me"@example.org"#)]
    );

    // Email address: "\\\"it's\ ugly\\\""@example.org; corresponding
    // 'mailto' URI:

    // <mailto:%22%5C%5C%5C%22it's%5C%20ugly%5C%5C%5C%22%22@example.org>.
    assert_eq!(
        mlt!("mailto:%22%5C%5C%5C%22it's%5C%20ugly%5C%5C%5C%22%22@example.org").address,
        vec![addr!(r#""\\\"it's\ ugly\\\""@example.org"#)]
    );

    // When an email address itself includes an "&" (ampersand) character, that
    // character has to be percent-encoded.  For example, the 'mailto' URI
    // to send mail to "Mike&family@example.org" is
    // <mailto:Mike%26family@example.org>.
    assert_eq!(
        mlt!("mailto:Mike%26family@example.org").address,
        vec![addr!("Mike&family@example.org")]
    );

    // Sending a mail with the subject "coffee" in French, i.e., "cafe" where the
    // final e is an e-acute, using UTF-8 and percent-encoding:
    // <mailto:user@example.org?subject=caf%C3%A9>
    assert_eq!(
        &mlt!("mailto:user@example.org?subject=caf%C3%A9").headers[HDR::SUBJECT],
        "café"
    );

    // The same subject, this time using an encoded-word (escaping the "="
    // and "?" characters used in the encoded-word syntax, because they are
    // reserved):
    // <mailto:user@example.org?subject=%3D%3Futf-8%3FQ%3Fcaf%3DC3%3DA9%3F%3D>
    assert_eq!(
        &mlt!("mailto:user@example.org?subject=%3D%3Futf-8%3FQ%3Fcaf%3DC3%3DA9%3F%3D").headers
            [HDR::SUBJECT],
        "=?utf-8?Q?caf=C3=A9?="
    );

    // The same subject, this time encoded as iso-8859-1:

    // <mailto:user@example.org?subject=%3D%3Fiso-8859-1%3FQ%3Fcaf%3DE9%3F%3D>
    assert_eq!(
        &mlt!("mailto:user@example.org?subject=%3D%3Fiso-8859-1%3FQ%3Fcaf%3DE9%3F%3D").headers
            [HDR::SUBJECT],
        "=?iso-8859-1?Q?caf=E9?="
    );

    // Going back to straight UTF-8 and adding a body with the same value:
    //
    // <mailto:user@example.org?subject=caf%C3%A9&body=caf%C3%A9>
    test_case!("mailto:user@example.org?subject=caf%C3%A9&body=caf%C3%A9",
        addresses => "user@example.org";
        body => Some("café");
        ("To", "user@example.org"),
        ("Subject", "café")
    );

    // The following example uses the Japanese word "natto" (Unicode
    // characters U+7D0D U+8C46) as a domain name label, sending a mail to a
    // user at "natto".example.org:

    // <mailto:user@%E7%B4%8D%E8%B1%86.example.org?subject=Test&body=NATTO>

    // When constructing the email, the domain name label is converted to
    // punycode.  The resulting message may look as follows:

    //    From: sender@example.net
    //    To: user@xn--99zt52a.example.org
    //    Subject: Test
    //    Content-Type: text/plain
    //    Content-Transfer-Encoding: 7bit
    //
    //    NATTO
    test_case!("mailto:user@%E7%B4%8D%E8%B1%86.example.org?subject=Test&body=NATTO",
        addresses => "user@納豆.example.org";
        body => Some("NATTO");
        ("To", "user@納豆.example.org"),
        ("Subject", "Test")
    );
}
