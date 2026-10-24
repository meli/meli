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

use rusty_fork::rusty_fork_test;

use crate::{
    terminal::{Screen, Virtual},
    utilities::widgets::ScrollBar,
};

rusty_fork_test! {
    #[test]
    fn test_utilities_text_input_field() {
        utilities_text_input_field()
    }

    #[test]
    fn test_utilities_scrollbar() {
        utilities_scrollbar()
    }
}

fn utilities_text_input_field() {
    use super::TextField;
    use crate::{melib::text::TextProcessing as _, Component as _, Key, UIEvent};

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

fn utilities_scrollbar() {
    macro_rules! assert_eq_grid {
        ($i:expr, $left:expr, $right:expr) => {{
            let i = $i;
            let left = $left;
            let right = $right;
            assert_eq!(&left, &right, "i = {i}\nleft:\n{left}\nright:\n{right}");
        }};
    }

    struct TestCase {
        // cols, rows
        area_size: (usize, usize),
        show_arrows: bool,
        ascii_drawing: bool,
        position: usize,
        visible_rows: usize,
        total_rows: usize,
        expected: &'static str,
    }

    let tmpdir = tempfile::TempDir::new().unwrap();
    let context = crate::state::Context::new_mock(&tmpdir);
    let mut screen = Screen::<Virtual>::new(Default::default());
    let theme_default = crate::conf::theme_value(&context, "theme_default");

    const TEST_CASES: &[TestCase] = &[
        TestCase {
            area_size: (3, 20),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20,
            expected: "   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \
                       \n   \n   \n   \n   \n   \n",
        },
        TestCase {
            area_size: (3, 20),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 21,
            expected: "  ▀\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  █\n  \
                       █\n  █\n  █\n  █\n   \n  ▄\n",
        },
        TestCase {
            area_size: (3, 20),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20 * 5,
            expected: "  ▀\n  █\n  █\n  █\n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \n   \
                       \n   \n   \n   \n   \n  ▄\n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20 * 5,
            expected: "  ▀\n  █\n   \n  ▄\n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: true,
            ascii_drawing: true,
            position: 0,
            visible_rows: 20,
            total_rows: 20 * 5,
            expected: "  ^\n  #\n   \n  v\n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20 * 5,
            expected: "  █\n   \n   \n   \n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 10,
            visible_rows: 20,
            total_rows: 20 * 2,
            expected: "   \n  █\n  █\n   \n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 20,
            visible_rows: 20,
            total_rows: 20 * 2,
            expected: "   \n   \n  █\n  █\n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 20 * 2,
            visible_rows: 20,
            total_rows: 20 * 2,
            expected: "   \n   \n  █\n  █\n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20,
            expected: "   \n   \n   \n   \n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 21,
            expected: "  █\n  █\n  █\n   \n",
        },
        TestCase {
            area_size: (3, 4),
            show_arrows: false,
            ascii_drawing: true,
            position: 0,
            visible_rows: 20,
            total_rows: 21,
            expected: "  #\n  #\n  #\n   \n",
        },
    ];

    for (
        i,
        &TestCase {
            area_size: (cols, rows),
            ascii_drawing,
            show_arrows,
            position,
            visible_rows,
            total_rows,
            expected,
        },
    ) in TEST_CASES.iter().enumerate()
    {
        assert!(screen.resize(cols, rows));
        let area = screen.area();
        screen.grid_mut().ascii_drawing = ascii_drawing;
        screen.grid_mut().clear_area(area, theme_default);
        ScrollBar::default().set_show_arrows(show_arrows).draw(
            screen.grid_mut(),
            area.nth_col(area.width().saturating_sub(1)),
            &context,
            position,
            visible_rows,
            total_rows,
        );
        // eprintln!(
        //     "i = {i}\n{boundary}\n{output}\n{boundary}",
        //     boundary = i.to_string().repeat(20),
        //     output = screen.grid().to_string()
        // );
        assert_eq_grid!(i, expected, screen.grid().to_string());
    }

    const HORZ_TEST_CASES: &[TestCase] = &[
        TestCase {
            area_size: (20, 1),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20,
            expected: "                    \n",
        },
        TestCase {
            area_size: (20, 1),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 21,
            expected: "▌█████████████████ ▐\n",
        },
        TestCase {
            area_size: (20, 1),
            show_arrows: true,
            ascii_drawing: false,
            position: 0,
            visible_rows: 20,
            total_rows: 20 * 3,
            expected: "▌██████            ▐\n",
        },
        TestCase {
            area_size: (20, 1),
            show_arrows: true,
            ascii_drawing: false,
            position: 10,
            visible_rows: 20,
            total_rows: 20 * 3,
            expected: "▌   ██████         ▐\n",
        },
        TestCase {
            area_size: (20, 1),
            show_arrows: true,
            ascii_drawing: true,
            position: 10,
            visible_rows: 20,
            total_rows: 20 * 3,
            expected: "<   ######         >\n",
        },
        TestCase {
            area_size: (20, 1),
            show_arrows: false,
            ascii_drawing: true,
            position: 10,
            visible_rows: 20,
            total_rows: 20 * 3,
            expected: "    ######          \n",
        },
    ];
    for (
        i,
        &TestCase {
            area_size: (cols, rows),
            ascii_drawing,
            show_arrows,
            position,
            visible_rows,
            total_rows,
            expected,
        },
    ) in HORZ_TEST_CASES.iter().enumerate()
    {
        assert!(screen.resize(cols, rows));
        let area = screen.area();
        screen.grid_mut().ascii_drawing = ascii_drawing;
        screen.grid_mut().clear_area(area, theme_default);
        ScrollBar::default()
            .set_show_arrows(show_arrows)
            .draw_horizontal(
                screen.grid_mut(),
                area.nth_row(area.height().saturating_sub(1)),
                &context,
                position,
                visible_rows,
                total_rows,
            );
        // eprintln!(
        //     "i = {i}\n{boundary}\n{output}\n{boundary}",
        //     boundary = i.to_string().repeat(20),
        //     output = screen.grid().to_string()
        // );
        assert_eq_grid!(i, expected, screen.grid().to_string());
    }
}
