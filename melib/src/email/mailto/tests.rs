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

    test_case!("mailto:1001084@bugs.debian.org?In-Reply-To=%3C168435582154.540248.1403466294276093439%40xxxxxxxxxxxxxx%3E&References=%3C163857393129.1083042.11053317018847169002.reportbug%40xxxxxxxxxxxxxx%3E%0A%20%3C168435582154.540248.1403466294276093439%40xxxxxxxxxxxxxx%3E&subject=Re%3A%20ITP%3A%20meli%20--%20terminal%20mail%20client&body=On%20Wed%2C%2017%20May%202023%2022%3A37%3A01%20%2B0200%20xxxxx%20zzzzzzzzzz%20%3Cdr%40xxxxxxxx%3E%20wrote%3A%0A%3E%200.7.2%2B20230517%20draft%201%20needs%20embedding%208%20crates%20%286%20missing%2C%201%20unwanted%2C%201%20ahead%29%3B%0A%3E%20runs%20and%20seems%20to%20work%20from%20a%20brief%20test%20use.%0A%3E%20%0A%3E%20Main%20tasks%20are%20still%20to%20keep%20package%20up-to-date%20with%20upstream%20releases%2C%20and%0A%3E%20to%20package%20more%20of%20the%20crates%20currently%20embedded.%0A%3E%20%0A%3E%20Here%27s%20how%20you%20can%20help%3A%0A%3E%20%0A%3E%20As%20user%20running%20Debian%2C%20you%20can%20test%20this%20draft%20package%3A%20Either%20build%20it%0A%3E%20yourself%20from%20source%20or%20tell%20%28by%20posting%20to%20this%20bugreport%29%20if%20you%0A%3E%20prefer%20testing%20the%20binary%20packages%20I%20built%20-%20then%20I%20will%20share%20those.%0A%3E%20%0A%3E%20As%20developer%20%28but%20no%20need%20to%20be%20official%20member%20of%20Debian%21%29%2C%20you%20can%0A%3E%20join%20the%20Debian%20Rust%20team%20and%20help%20package%20these%20missing%20crates%3A%0A%3E%20https%3A%2F%2Fsalsa.debian.org%2Fdebian%2Fmeli%2F-%2Fblob%2Fdebian%2Flatest%2Fdebian%2FTODO%0A%3E%20%0A%3E%20%0A%3E%20%20-%20JJJJJ%0A%3E%20%0A%3E%20--%20%0A%3E%20%20%2A%20JJJJJ%20xxxxxxxxxx%20-%20idealist%20%26%20Internet-arkitekt%0A%3E%20%20%2A%20Tlf.%3A%20%2B45%20%20%20Website%3A%20http%3A%2F%2Fxxxxxxxxxxx%2F%0A%3E%20%0A%3E%20%20%5Bx%5D%20quote%20me%20freely%20%20%5B%20%5D%20ask%20before%20reusing%20%20%5B%20%5D%20keep%20private",
        addresses => "1001084@bugs.debian.org";
        body => Some(
            "On Wed, 17 May 2023 22:37:01 +0200 xxxxx zzzzzzzzzz <dr@xxxxxxxx> wrote:\n> 0.7.2+20230517 draft 1 needs embedding 8 crates (6 missing, 1 unwanted, 1 ahead);\n> runs and seems to work from a brief test use.\n> \n> Main tasks are still to keep package up-to-date with upstream releases, and\n> to package more of the crates currently embedded.\n> \n> Here's how you can help:\n> \n> As user running Debian, you can test this draft package: Either build it\n> yourself from source or tell (by posting to this bugreport) if you\n> prefer testing the binary packages I built - then I will share those.\n> \n> As developer (but no need to be official member of Debian!), you can\n> join the Debian Rust team and help package these missing crates:\n> https://salsa.debian.org/debian/meli/-/blob/debian/latest/debian/TODO\n> \n> \n>  - JJJJJ\n> \n> -- \n>  * JJJJJ xxxxxxxxxx - idealist & Internet-arkitekt\n>  * Tlf.: +45   Website: http://xxxxxxxxxxx/\n> \n>  [x] quote me freely  [ ] ask before reusing  [ ] keep private",
        );
        ("To", "1001084@bugs.debian.org"),
        ("In-Reply-To", "<168435582154.540248.1403466294276093439@xxxxxxxxxxxxxx>"),
        ("References", "<163857393129.1083042.11053317018847169002.reportbug@xxxxxxxxxxxxxx>\n <168435582154.540248.1403466294276093439@xxxxxxxxxxxxxx>"),
        ("Subject", "Re: ITP: meli -- terminal mail client")
    );

    test_case!("mailto:1001084@bugs.debian.org?body=On%20Tue%2C%2023%20Jan%202024%2019%3A46%3A47%20%2B0100%20Jonas%20Smedegaard%20%3Cdr%40jones.dk%3E%20wrote%3A%0A%3E%200.8.5%2B20240101%20draft%201%20needs%20embedding%205%20crates%20%283%20missing%2C%202%20ahead%29%3B%20runs%20and%20seems%20to%20work%20from%20a%20brief%20test%20use.%0A%3E%20%0A%3E%20Main%20tasks%20are%20still%20to%20keep%20package%20up-to-date%20with%20upstream%20releases%2C%0A%3E%20and%20to%20package%20more%20of%20the%20crates%20currently%20embedded.%0A%3E%20%0A%3E%20Here%27s%20how%20you%20can%20help%3A%0A%3E%20%0A%3E%20As%20user%20running%20Debian%2C%20you%20can%20test%20this%20draft%20package%3A%20Either%20build%20it%0A%3E%20yourself%20from%20source%20or%20tell%20%28by%20posting%20to%20this%20bugreport%29%20if%20you%0A%3E%20prefer%20testing%20the%20binary%20packages%20I%20built%20-%20then%20I%20will%20share%20those.%0A%3E%20%0A%3E%20As%20developer%20%28but%20no%20need%20to%20be%20official%20member%20of%20Debian%21%29%2C%20you%20can%0A%3E%20join%20the%20Debian%20Rust%20team%20and%20help%20package%20these%20missing%20crates%3A%0A%3E%20https%3A%2F%2Fsalsa.debian.org%2Fdebian%2Fmeli%2F-%2Fblob%2Fdebian%2Flatest%2Fdebian%2FTODO%0A%3E%20%0A%3E%20%0A%3E%20%20-%20Jonas%0A%3E%20%0A%3E%20--%20%0A%3E%20%20%2A%20Jonas%20Smedegaard%20-%20idealist%20%26%20Internet-arkitekt%0A%3E%20%20%2A%20Tlf.%3A%20%2B45%2040843136%20%20Website%3A%20http%3A%2F%2Fdr.jones.dk%2F%0A%3E%20%0A%3E%20%20%5Bx%5D%20quote%20me%20freely%20%20%5B%20%5D%20ask%20before%20reusing%20%20%5B%20%5D%20keep%20private&In-Reply-To=%3C170603560794.2588145.11750867315250989304%40auryn.jones.dk%3E&References=%3C163857393129.1083042.11053317018847169002.reportbug%40auryn.jones.dk%3E%0A%20%3C170603560794.2588145.11750867315250989304%40auryn.jones.dk%3E&subject=Re%3A%20ITP%3A%20meli%20--%20terminal%20mail%20client",
        addresses=> "1001084@bugs.debian.org";
        body => Some("On Tue, 23 Jan 2024 19:46:47 +0100 Jonas Smedegaard <dr@jones.dk> wrote:\n> 0.8.5+20240101 draft 1 needs embedding 5 crates (3 missing, 2 ahead); runs and seems to work from a brief test use.\n> \n> Main tasks are still to keep package up-to-date with upstream releases,\n> and to package more of the crates currently embedded.\n> \n> Here's how you can help:\n> \n> As user running Debian, you can test this draft package: Either build it\n> yourself from source or tell (by posting to this bugreport) if you\n> prefer testing the binary packages I built - then I will share those.\n> \n> As developer (but no need to be official member of Debian!), you can\n> join the Debian Rust team and help package these missing crates:\n> https://salsa.debian.org/debian/meli/-/blob/debian/latest/debian/TODO\n> \n> \n>  - Jonas\n> \n> -- \n>  * Jonas Smedegaard - idealist & Internet-arkitekt\n>  * Tlf.: +45 40843136  Website: http://dr.jones.dk/\n> \n>  [x] quote me freely  [ ] ask before reusing  [ ] keep private");
        ("To", "1001084@bugs.debian.org"), ("Subject", "Re: ITP: meli -- terminal mail client"), ("In-Reply-To", "<170603560794.2588145.11750867315250989304@auryn.jones.dk>"), ("References", "<163857393129.1083042.11053317018847169002.reportbug@auryn.jones.dk>\n <170603560794.2588145.11750867315250989304@auryn.jones.dk>")
    );
}
