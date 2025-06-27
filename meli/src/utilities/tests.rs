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
fn test_utilities_text_input_field() {
    use super::TextField;
    use crate::{melib::text::TextProcessing, Component, Key, UIEvent};

    const PANGRAM: &str = "Blocky dwarf zings the jump.";
    const PANGRAM_END: usize = PANGRAM.len();

    let mut field = TextField::default();
    field.set_content(PANGRAM.to_string());
    assert_eq!(field.cursor(), PANGRAM_END);
    field.cursor_dec();
    field.cursor_dec();
    assert_eq!(field.cursor(), PANGRAM_END - 2);
    field.cursor_inc();
    field.cursor_inc();
    let tmpdir = tempfile::TempDir::new().unwrap();
    let mut context = crate::state::Context::new_mock(&tmpdir);

    // Test arrow key movements

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Left), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END - 1);
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Right), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END);

    // Test shortcut movements

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('b')), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END - 1);
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('f')), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END);

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('a')), &mut context));
    assert_eq!(field.cursor(), 0);

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('e')), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END);

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Home), &mut context));
    assert_eq!(field.cursor(), 0);

    assert!(field.process_event(&mut UIEvent::InsertInput(Key::End), &mut context));
    assert_eq!(field.cursor(), PANGRAM_END);

    // Test text editing operations

    // Transpose characters at the end.
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the jum.p");
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), PANGRAM);

    // Transpose intermediate characters.
    field.cursor_dec();
    field.cursor_dec();
    field.cursor_dec();
    assert_eq!(field.cursor(), PANGRAM_END - 3);
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ujmp.");
    assert_eq!(field.cursor(), PANGRAM_END - 2);
    field.cursor_dec();
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), PANGRAM);
    assert_eq!(field.cursor(), PANGRAM_END - 2);
    field.cursor_inc();
    field.cursor_inc();
    // Cut word.
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('w')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ");
    assert_eq!(field.cursor(), "Blocky dwarf zings the ".len());
    // Backward one alphanumeric word
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Alt('b')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ");
    assert_eq!(field.cursor(), "Blocky dwarf zings the ".len() - 1);
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Alt('b')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ");
    assert_eq!(
        field.cursor(),
        "Blocky dwarf zings the ".len() - "the ".len()
    );
    // Forward one alphanumeric word
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Alt('f')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ");
    assert_eq!(field.cursor(), "Blocky dwarf zings t".len());
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Alt('f')), &mut context));
    assert_eq!(field.as_str(), "Blocky dwarf zings the ");
    assert_eq!(field.cursor(), "Blocky dwarf zings the ".len());

    // Paste
    assert!(field.process_event(
        &mut UIEvent::InsertInput(Key::Paste("jump.".into())),
        &mut context
    ));
    assert_eq!(field.as_str(), PANGRAM);
    assert_eq!(field.cursor(), PANGRAM_END);

    const EMOJIGRAM: &str = "😶 🙃 🤔 👋 🤡 🤯";
    const EMOJIGRAM_END: usize = EMOJIGRAM.len();

    let emojigram_grapheme_end: usize = EMOJIGRAM.grapheme_len();

    field.set_content(EMOJIGRAM.to_string());
    assert_eq!(
        (field.byte_cursor(), field.cursor()),
        (EMOJIGRAM_END, emojigram_grapheme_end)
    );
    field.cursor_dec();
    assert_eq!(
        (field.byte_cursor(), field.cursor()),
        (EMOJIGRAM_END - 4, emojigram_grapheme_end - 1)
    );
    field.cursor_dec();
    assert_eq!(
        (field.byte_cursor(), field.cursor()),
        (EMOJIGRAM_END - 5, emojigram_grapheme_end - 2)
    );
    field.cursor_inc();
    field.cursor_inc();
    assert_eq!(
        (field.byte_cursor(), field.cursor()),
        (EMOJIGRAM_END, emojigram_grapheme_end)
    );

    // Transpose characters at the end.
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), "😶 🙃 🤔 👋 🤡🤯 ");
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(field.as_str(), EMOJIGRAM);

    // Transpose intermediate characters.
    field.cursor_dec();
    field.cursor_dec();
    field.cursor_dec();
    assert_eq!(&field.as_str()[..field.byte_cursor()], "😶 🙃 🤔 👋 ");
    assert_eq!(field.byte_cursor(), EMOJIGRAM_END - 9);
    {
        let current_cursor = field.cursor();
        let current_byte_cursor = field.byte_cursor();
        assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
        assert_eq!(field.as_str(), "😶 🙃 🤔  👋🤡 🤯");
        // transpose increases cursor position
        assert_eq!(field.cursor(), current_cursor + 1);
        assert_ne!(field.byte_cursor(), current_byte_cursor);
        assert_eq!(field.byte_cursor(), current_byte_cursor + "🤡".len());
        assert_eq!(&field.as_str()[..field.byte_cursor()], "😶 🙃 🤔  👋🤡");
    }
    field.cursor_dec();
    assert!(field.process_event(&mut UIEvent::InsertInput(Key::Ctrl('t')), &mut context));
    assert_eq!(&field.as_str()[..field.byte_cursor()], "😶 🙃 🤔 👋 🤡");
    assert_eq!(field.byte_cursor(), EMOJIGRAM_END - 5);
    field.cursor_dec();
    assert_eq!(field.byte_cursor(), EMOJIGRAM_END - 9);
    assert_eq!(field.as_str(), EMOJIGRAM);
    _ = tmpdir.close();
}

/// Returns a closure that prints the string " OK\n" to `stderr`.
///
/// If `stderr` is a TTY, the output will contain escape code sequences to
/// change the foreground color of the text with the second indexed color
/// (usually green).
pub fn eprintln_ok_fn() -> Box<dyn Fn()> {
    if crate::terminal::is_tty() && std::env::var_os("NO_COLOR").is_none() {
        struct SetAf2(String);
        struct Sgr0(String);
        fn get_tput_sequences() -> Option<(SetAf2, Sgr0)> {
            use std::process::{Command, Stdio};

            let setaf_2 = SetAf2(
                String::from_utf8(
                    Command::new("tput")
                        .args(["setaf", "2"])
                        .stdout(Stdio::piped())
                        .stdin(Stdio::null())
                        .stderr(Stdio::inherit())
                        .output()
                        .ok()?
                        .stdout,
                )
                .ok()?,
            );

            let sgr0 = Sgr0(
                String::from_utf8(
                    Command::new("tput")
                        .arg("sgr0")
                        .stdout(Stdio::piped())
                        .stdin(Stdio::null())
                        .stderr(Stdio::inherit())
                        .output()
                        .ok()?
                        .stdout,
                )
                .ok()?,
            );
            Some((setaf_2, sgr0))
        }

        if let Some((SetAf2(setaf_2), Sgr0(sgr0))) = get_tput_sequences() {
            return Box::new(move || {
                eprintln!(" {setaf_2}OK{sgr0}");
            });
        }
    }

    Box::new(|| {
        eprintln!(" OK");
    })
}

/// Returns a closure that prints a formatted message, without a trailing
/// newline, and prefixed with an increasing counter.
pub fn eprint_step_fn() -> Box<dyn FnMut(std::fmt::Arguments) -> usize> {
    let mut counter = 1;
    Box::new(move |args: std::fmt::Arguments| {
        let step = counter;
        eprint!("{step}. {args}");
        counter += 1;
        step
    })
}
