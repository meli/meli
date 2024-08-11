//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

#[test]
fn test_terminal_text_presentation() {
    use crate::terminal::TextPresentation;

    // A big thanks to every spammer, e-shop and even patch submitter who used
    // emojis in subjects and inspired me to add this feature.
    const TEST_CASES: &[(&str, &str)] = &[
        (
            "The Darkness Issue is now shipping worldwide 🦇",
            "The Darkness Issue is now shipping worldwide 🦇︎",
        ),
        ("🐝 <user@example.com>", "🐝︎ <user@example.com>"),
        (
            "Happy Women's Day 🎀 - ΠΑΡΕ ΤΟ ΔΩΡΟ ΣΟΥ 🎁",
            "Happy Women's Day 🎀︎ - ΠΑΡΕ ΤΟ ΔΩΡΟ ΣΟΥ 🎁︎",
        ),
        (
            "💨 Εσύ θα προλάβεις; 🔴 🐇 Καλό Πάσχα!",
            "💨︎ Εσύ θα προλάβεις; 🔴︎ 🐇︎ Καλό Πάσχα!",
        ),
        ("Dream drop 💤", "Dream drop 💤︎"),
        (
            "⭐ Αξιολόγησε τον επαγγελματία! ⭐",
            "⭐︎ Αξιολόγησε τον επαγγελματία! ⭐︎",
        ),
        (
            "🔓 MYSTERY UNLOCKED: 💀NEW💀 SIGNED VENTURE BROS. DVD SALE & MERCH RESTOCK",
            "🔓︎ MYSTERY UNLOCKED: 💀︎NEW💀︎ SIGNED VENTURE BROS. DVD SALE & MERCH RESTOCK",
        ),
        (
            "[PATCH RFC 00/26] Multifd 🔀 device state transfer support with VFIO consumer",
            "[PATCH RFC 00/26] Multifd 🔀︎ device state transfer support with VFIO consumer",
        ),
    ];

    for (emoji, text) in TEST_CASES {
        assert_eq!(&emoji.text_pr(), text);
    }
}

#[test]
fn test_terminal_osc8_print() {
    use crate::terminal::Hyperlink;

    const TEST_CASES: &[(Hyperlink<str, str, str>, &str)] = &[
        (
            Hyperlink::new("text", "url"),
            "\x1b]8;;url\x07text\x1b]8;;\x07",
        ),
        (
            Hyperlink::new("/tmp/", "file:///tmp/"),
            "\x1b]8;;file:///tmp/\x07/tmp/\x1b]8;;\x07",
        ),
        (
            Hyperlink::new("meli(1)", "man:meli(1)"),
            "\x1b]8;;man:meli(1)\x07meli(1)\x1b]8;;\x07",
        ),
        (
            Hyperlink::with_id("duplicated", "meli(1)", "man:meli(1)"),
            "\x1b]8;id=duplicated;man:meli(1)\x07meli(1)\x1b]8;;\x07",
        ),
        (
            Hyperlink::with_id("duplicated", "meli(1)", "man:meli(1)"),
            "\x1b]8;id=duplicated;man:meli(1)\x07meli(1)\x1b]8;;\x07",
        ),
    ];

    for (input, output) in TEST_CASES {
        println!("{}", input);
        assert_eq!(&input.to_string(), output);
    }
}
