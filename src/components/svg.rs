/*
 * meli - svg screenshot
 *
 * Copyright  Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */
use std::{collections::BTreeMap, io::Write};

use super::*;

#[derive(Debug)]
pub struct SVGScreenshotFilter {
    save_screenshot: bool,
    id: ComponentId,
}

impl fmt::Display for SVGScreenshotFilter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "svg screenshot filter")
    }
}

impl SVGScreenshotFilter {
    pub fn new() -> Self {
        SVGScreenshotFilter {
            save_screenshot: false,
            id: ComponentId::default(),
        }
    }
}

impl Component for SVGScreenshotFilter {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, context: &mut Context) {
        if !self.save_screenshot {
            return;
        }
        self.save_screenshot = false;
        let grid: &CellBuffer = _grid;
        use svg_crate::{
            node::{
                element::{Definitions, Group, Rectangle, Style, Text, Use},
                Text as TextNode,
            },
            Document,
        };

        let (width, height) = grid.size();
        /*
         * Format frame as follows:
         * - The entire background is a big rectangle.
         * - Every text piece with unified foreground color is a text element
         *   inserted into the
         * `definitions` field of the svg, and then `use`ed as a reference
         * - Every background piece (a slice of unified background color) is a
         *   rectangle element
         * inserted along with the `use` elements
         *
         * Each row is arbritarily set at 17px high, and each character cell is 8
         * pixels wide. Rectangle cells each have one extra pixel (so 18px *
         * 9px) in their dimensions in order to cover the spacing between
         * cells.
         */
        let mut definitions = Definitions::new();
        let mut rows_group = Group::new();
        let mut text = String::with_capacity(width);
        /* Before creating text node out of `text` variable, escape what's necessary */
        let mut escaped_text = String::with_capacity(width);

        /* keep a map with used colors and write a stylesheet when we're done */
        let mut classes: BTreeMap<(u8, u8, u8), usize> = BTreeMap::new();
        for (row_idx, row) in grid.bounds_iter(((0, 0), (width, height))).enumerate() {
            text.clear();
            escaped_text.clear();
            /* Each row is a <g> group element, consisting of text elements */
            let mut row_group = Group::new().set("id", format!("{:x}", row_idx + 1));
            /* Keep track of colors and attributes.
             * - Whenever the foreground color changes, emit a text element with the
             *   accumulated
             * text in the specific foreground color.
             * - Whenever the backgrund color changes, emit a rectangle element filled
             *   with the
             * specific background color.
             */
            let mut cur_fg = Color::Default;
            let mut cur_bg = Color::Default;
            let mut cur_attrs = Attr::DEFAULT;
            let mut prev_x_fg = 0;
            let mut is_start = true;
            let mut prev_x_bg = 0;
            for (x, c) in row.enumerate() {
                if cur_bg != grid[c].bg() || cur_fg != grid[c].fg() || cur_attrs != grid[c].attrs()
                //|| (grid[c].ch() == ' ' && !is_start)
                {
                    if cur_bg != Color::Default {
                        let mut rect = Rectangle::new()
                            .set("x", prev_x_bg * 8)
                            .set("y", 17 * row_idx)
                            .set("width", (x - prev_x_bg) * 8 + 1)
                            //.set("bgname", format!("{:?}", cur_bg))
                            .set("height", 18);
                        match cur_bg {
                            Color::Rgb(r, g, b) => {
                                let class = if classes.contains_key(&(r, g, b)) {
                                    classes[&(r, g, b)]
                                } else {
                                    let classes_size = classes.len();
                                    classes.insert((r, g, b), classes_size);
                                    classes_size
                                };
                                rect = rect.set("class", format!("f{:x}", class).as_str());
                            }
                            Color::Default => {
                                unreachable!();
                            }
                            c if c.as_byte() < 16 => {
                                rect = rect.set("class", format!("c{}", c.as_byte()).as_str());
                            }
                            c => {
                                let c = c.as_byte();
                                let (r, g, b) = XTERM_COLORS[c as usize];
                                let class = if classes.contains_key(&(r, g, b)) {
                                    classes[&(r, g, b)]
                                } else {
                                    let classes_size = classes.len();
                                    classes.insert((r, g, b), classes_size);
                                    classes_size
                                };
                                rect = rect.set("class", format!("f{:x}", class).as_str());
                            }
                        }
                        rows_group = rows_group.add(rect);
                    }
                    prev_x_bg = x;
                    cur_bg = grid[c].bg();
                    if !text.is_empty() {
                        let text_length = text.grapheme_width();
                        for c in text.chars() {
                            match c {
                                '"' => escaped_text.push_str("&quot;"),
                                '&' => escaped_text.push_str("&amp;"),
                                '\'' => escaped_text.push_str("&apos;"),
                                '<' => escaped_text.push_str("&lt;"),
                                '>' => escaped_text.push_str("&gt;"),
                                c => escaped_text.push(c),
                            }
                        }
                        let mut text_el = Text::new()
                            .add(TextNode::new(&escaped_text))
                            .set("x", prev_x_fg * 8)
                            .set("textLength", text_length * 8);
                        /* .set("fgname", format!("{:?}", cur_fg)); */
                        if cur_attrs.intersects(Attr::BOLD) {
                            text_el = text_el.set("font-weight", "bold");
                        }
                        if cur_attrs.intersects(Attr::ITALICS) {
                            text_el = text_el.set("font-style", "italic");
                        }
                        if cur_attrs.intersects(Attr::UNDERLINE) {
                            text_el = text_el.set("text-decoration", "underline");
                        }
                        if cur_attrs.intersects(Attr::DIM) {
                            text_el = text_el.set("font-weight", "lighter");
                        }
                        if cur_attrs.intersects(Attr::HIDDEN) {
                            text_el = text_el.set("display", "none");
                        }
                        match cur_fg {
                            Color::Default if cur_attrs.intersects(Attr::REVERSE) => {
                                text_el = text_el.set("class", "b");
                            }
                            Color::Default => {
                                text_el = text_el.set("class", "f");
                            }
                            Color::Rgb(r, g, b) => {
                                let class = if classes.contains_key(&(r, g, b)) {
                                    classes[&(r, g, b)]
                                } else {
                                    let classes_size = classes.len();
                                    classes.insert((r, g, b), classes_size);
                                    classes_size
                                };
                                text_el = text_el.set("class", format!("f{:x}", class).as_str());
                            }
                            c if c.as_byte() < 16 => {
                                text_el =
                                    text_el.set("class", format!("c{}", c.as_byte()).as_str());
                            }
                            c => {
                                let c = c.as_byte();
                                let (r, g, b) = XTERM_COLORS[c as usize];
                                let class = if classes.contains_key(&(r, g, b)) {
                                    classes[&(r, g, b)]
                                } else {
                                    let classes_size = classes.len();
                                    classes.insert((r, g, b), classes_size);
                                    classes_size
                                };
                                text_el = text_el.set("class", format!("f{:x}", class).as_str());
                            }
                        };
                        row_group = row_group.add(text_el);
                        text.clear();
                        escaped_text.clear();
                    }
                    prev_x_fg = x;
                    cur_fg = grid[c].fg();
                    cur_attrs = grid[c].attrs();
                }
                match grid[c].ch() {
                    ' ' if is_start => {
                        prev_x_fg = x + 1;
                    }
                    c => text.push(c),
                }
                if grid[c].ch() != ' ' {
                    is_start = false;
                }
            }
            /* Append last elements of the row if any */
            if cur_bg != Color::Default {
                let mut rect = Rectangle::new()
                    .set("x", prev_x_bg * 8)
                    .set("y", 17 * row_idx)
                    .set("width", (width - prev_x_bg) * 8 + 1)
                    //.set("bgname", format!("{:?}", cur_bg))
                    .set("height", 18);
                match cur_bg {
                    Color::Rgb(r, g, b) => {
                        let class = if classes.contains_key(&(r, g, b)) {
                            classes[&(r, g, b)]
                        } else {
                            let classes_size = classes.len();
                            classes.insert((r, g, b), classes_size);
                            classes_size
                        };
                        rect = rect.set("class", format!("f{:x}", class).as_str());
                    }
                    Color::Default => {
                        unreachable!();
                    }
                    c if c.as_byte() < 16 => {
                        rect = rect.set("class", format!("c{}", c.as_byte()).as_str());
                    }
                    c => {
                        let c = c.as_byte();
                        let (r, g, b) = XTERM_COLORS[c as usize];
                        let class = if classes.contains_key(&(r, g, b)) {
                            classes[&(r, g, b)]
                        } else {
                            let classes_size = classes.len();
                            classes.insert((r, g, b), classes_size);
                            classes_size
                        };
                        rect = rect.set("class", format!("f{:x}", class).as_str());
                    }
                }
                rows_group = rows_group.add(rect);
            }
            if !text.is_empty() {
                let text_length = text.grapheme_width();
                for c in text.chars() {
                    match c {
                        '"' => escaped_text.push_str("&quot;"),
                        '&' => escaped_text.push_str("&amp;"),
                        '\'' => escaped_text.push_str("&apos;"),
                        '<' => escaped_text.push_str("&lt;"),
                        '>' => escaped_text.push_str("&gt;"),
                        c => escaped_text.push(c),
                    }
                }
                let mut text_el = Text::new()
                    .add(TextNode::new(&escaped_text))
                    .set("x", prev_x_fg * 8)
                    .set("textLength", text_length * 8);
                /* .set("fgname", format!("{:?}", cur_fg)); */
                if cur_attrs.intersects(Attr::BOLD) {
                    text_el = text_el.set("font-weight", "bold");
                }
                if cur_attrs.intersects(Attr::ITALICS) {
                    text_el = text_el.set("font-style", "italic");
                }
                if cur_attrs.intersects(Attr::UNDERLINE) {
                    text_el = text_el.set("text-decoration", "underline");
                }
                if cur_attrs.intersects(Attr::DIM) {
                    text_el = text_el.set("font-weight", "lighter");
                }
                if cur_attrs.intersects(Attr::HIDDEN) {
                    text_el = text_el.set("display", "none");
                }
                match cur_fg {
                    Color::Default if cur_attrs.intersects(Attr::REVERSE) => {
                        text_el = text_el.set("class", "b");
                    }
                    Color::Default => {
                        text_el = text_el.set("class", "f");
                    }
                    Color::Rgb(r, g, b) => {
                        let class = if classes.contains_key(&(r, g, b)) {
                            classes[&(r, g, b)]
                        } else {
                            let classes_size = classes.len();
                            classes.insert((r, g, b), classes_size);
                            classes_size
                        };
                        text_el = text_el.set("class", format!("f{:x}", class).as_str());
                    }
                    c if c.as_byte() < 16 => {
                        text_el = text_el.set("class", format!("c{}", c.as_byte()).as_str());
                    }
                    c => {
                        let c = c.as_byte();
                        let (r, g, b) = XTERM_COLORS[c as usize];
                        let class = if classes.contains_key(&(r, g, b)) {
                            classes[&(r, g, b)]
                        } else {
                            let classes_size = classes.len();
                            classes.insert((r, g, b), classes_size);
                            classes_size
                        };
                        text_el = text_el.set("class", format!("f{:x}", class).as_str());
                    }
                }
                row_group = row_group.add(text_el);
                text.clear();
                escaped_text.clear();
            }
            definitions = definitions.add(row_group);
            rows_group = rows_group.add(
                Use::new()
                    .set("xlink:href", format!("#{:x}", row_idx + 1))
                    .set("y", 17 * row_idx),
            );
        }
        let mut style_string = CSS_STYLE.to_string();
        for ((r, g, b), name) in classes {
            style_string
                .extend(format!(".f{:x}{{fill:#{:02x}{:02x}{:02x};}}", name, r, g, b).chars());
        }
        let document = Document::new()
            .set("viewBox", (0, 0, width * 8, height * 17 + 2))
            .set("width", width * 8)
            .set("height", height * 17 + 2)
            .add(Definitions::new().add(Style::new(&style_string).set("type", "text/css")))
            .add(
                Document::new()
                    .set("id", "t")
                    .set("preserveAspectRatio", "xMidYMin slice")
                    .set("viewBox", (0, 0, width * 8, height * 17))
                    .set("width", width * 8)
                    .set("height", height * 17)
                    .add(
                        Rectangle::new()
                            .set("class", "b")
                            .set("height", "100%")
                            .set("width", "100%")
                            .set("x", 0)
                            .set("y", 0),
                    )
                    .add(definitions)
                    .add(rows_group),
            )
            .set("xmlns", "http://www.w3.org/2000/svg")
            .set("baseProfile", "full")
            .set("xmlns:xlink", "http://www.w3.org/1999/xlink")
            .set("version", "1.1");

        let mut s = Vec::new();
        svg_crate::write(&mut s, &document).unwrap();
        let mut res = Vec::new();
        /*
         * svg crate formats text nodes like this:
         *
         *     <text>
         *     actual content
         *     </text>
         *
         * But we don't want any extra newlines before/after the tags:
         *
         *     <text>actual content</text>
         *
         * So remove all new lines from SVG file.
         */
        for b in s {
            if b == b'\n' {
                continue;
            }
            res.push(b);
        }
        let mut filename = melib::datetime::timestamp_to_string(
            melib::datetime::now(),
            Some("meli Screenshot - %e %h %Y %H:%M:%S.svg"),
            true,
        );
        while std::path::Path::new(&filename).exists() {
            filename.pop();
            filename.pop();
            filename.pop();
            filename.pop();
            filename.push_str("_.svg");
        }
        std::fs::File::create(&filename)
            .unwrap()
            .write_all(&res)
            .unwrap();
        context.replies.push_back(UIEvent::Notification(
            Some("Screenshot saved".into()),
            format!("Screenshot saved to {}", filename),
            None,
        ));
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        if let UIEvent::Input(Key::F(6)) = event {
            self.save_screenshot = true;
            true
        } else if let UIEvent::CmdInput(Key::F(6)) = event {
            self.save_screenshot = true;
            true
        } else if let UIEvent::EmbedInput((Key::F(6), _)) = event {
            self.save_screenshot = true;
            false
        } else {
            false
        }
    }
    fn set_dirty(&mut self, _value: bool) {}

    fn is_dirty(&self) -> bool {
        self.save_screenshot
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

const CSS_STYLE: &str = r#"#t{font-family:'DejaVu Sans Mono',monospace;font-style:normal;font-size:14px;} text {dominant-baseline: text-before-edge; white-space: pre;} .f{fill:#e5e5e5;} .b{fill:#000;} .c0 {fill:#000;} .c1 {fill:#cd0000;} .c2 {fill:#00cd00;} .c3 {fill:#cdcd00;} .c4 {fill:#00e;} .c5 {fill:#cd00cd;} .c6 {fill:#00cdcd;} .c7 {fill:#e5e5e5;} .c8 {fill:#7f7f7f;} .c9 {fill:#f00;} .c10 {fill:#0f0;} .c11 {fill:#ff0;} .c12 {fill:#5c5cff;} .c13 {fill:#f0f;} .c14 {fill:#0ff;} .c15 {fill:#fff;}"#;

const XTERM_COLORS: &[(u8, u8, u8)] = &[
    /* 0 */ (0, 0, 0),
    /* 1 */ (128, 0, 0),
    /* 2 */ (0, 128, 0),
    /* 3 */ (128, 128, 0),
    /* 4 */ (0, 0, 128),
    /* 5 */ (128, 0, 128),
    /* 6 */ (0, 128, 128),
    /* 7 */ (192, 192, 192),
    /* 8 */ (128, 128, 128),
    /* 9 */ (255, 0, 0),
    /* 10 */ (0, 255, 0),
    /* 11 */ (255, 255, 0),
    /* 12 */ (0, 0, 255),
    /* 13 */ (255, 0, 255),
    /* 14 */ (0, 255, 255),
    /* 15 */ (255, 255, 255),
    /* 16 */ (0, 0, 0),
    /* 17 */ (0, 0, 95),
    /* 18 */ (0, 0, 135),
    /* 19 */ (0, 0, 175),
    /* 20 */ (0, 0, 215),
    /* 21 */ (0, 0, 255),
    /* 22 */ (0, 95, 0),
    /* 23 */ (0, 95, 95),
    /* 24 */ (0, 95, 135),
    /* 25 */ (0, 95, 175),
    /* 26 */ (0, 95, 215),
    /* 27 */ (0, 95, 255),
    /* 28 */ (0, 135, 0),
    /* 29 */ (0, 135, 95),
    /* 30 */ (0, 135, 135),
    /* 31 */ (0, 135, 175),
    /* 32 */ (0, 135, 215),
    /* 33 */ (0, 135, 255),
    /* 34 */ (0, 175, 0),
    /* 35 */ (0, 175, 95),
    /* 36 */ (0, 175, 135),
    /* 37 */ (0, 175, 175),
    /* 38 */ (0, 175, 215),
    /* 39 */ (0, 175, 255),
    /* 40 */ (0, 215, 0),
    /* 41 */ (0, 215, 95),
    /* 42 */ (0, 215, 135),
    /* 43 */ (0, 215, 175),
    /* 44 */ (0, 215, 215),
    /* 45 */ (0, 215, 255),
    /* 46 */ (0, 255, 0),
    /* 47 */ (0, 255, 95),
    /* 48 */ (0, 255, 135),
    /* 49 */ (0, 255, 175),
    /* 50 */ (0, 255, 215),
    /* 51 */ (0, 255, 255),
    /* 52 */ (95, 0, 0),
    /* 53 */ (95, 0, 95),
    /* 54 */ (95, 0, 135),
    /* 55 */ (95, 0, 175),
    /* 56 */ (95, 0, 215),
    /* 57 */ (95, 0, 255),
    /* 58 */ (95, 95, 0),
    /* 59 */ (95, 95, 95),
    /* 60 */ (95, 95, 135),
    /* 61 */ (95, 95, 175),
    /* 62 */ (95, 95, 215),
    /* 63 */ (95, 95, 255),
    /* 64 */ (95, 135, 0),
    /* 65 */ (95, 135, 95),
    /* 66 */ (95, 135, 135),
    /* 67 */ (95, 135, 175),
    /* 68 */ (95, 135, 215),
    /* 69 */ (95, 135, 255),
    /* 70 */ (95, 175, 0),
    /* 71 */ (95, 175, 95),
    /* 72 */ (95, 175, 135),
    /* 73 */ (95, 175, 175),
    /* 74 */ (95, 175, 215),
    /* 75 */ (95, 175, 255),
    /* 76 */ (95, 215, 0),
    /* 77 */ (95, 215, 95),
    /* 78 */ (95, 215, 135),
    /* 79 */ (95, 215, 175),
    /* 80 */ (95, 215, 215),
    /* 81 */ (95, 215, 255),
    /* 82 */ (95, 255, 0),
    /* 83 */ (95, 255, 95),
    /* 84 */ (95, 255, 135),
    /* 85 */ (95, 255, 175),
    /* 86 */ (95, 255, 215),
    /* 87 */ (95, 255, 255),
    /* 88 */ (135, 0, 0),
    /* 89 */ (135, 0, 95),
    /* 90 */ (135, 0, 135),
    /* 91 */ (135, 0, 175),
    /* 92 */ (135, 0, 215),
    /* 93 */ (135, 0, 255),
    /* 94 */ (135, 95, 0),
    /* 95 */ (135, 95, 95),
    /* 96 */ (135, 95, 135),
    /* 97 */ (135, 95, 175),
    /* 98 */ (135, 95, 215),
    /* 99 */ (135, 95, 255),
    /* 100 */ (135, 135, 0),
    /* 101 */ (135, 135, 95),
    /* 102 */ (135, 135, 135),
    /* 103 */ (135, 135, 175),
    /* 104 */ (135, 135, 215),
    /* 105 */ (135, 135, 255),
    /* 106 */ (135, 175, 0),
    /* 107 */ (135, 175, 95),
    /* 108 */ (135, 175, 135),
    /* 109 */ (135, 175, 175),
    /* 110 */ (135, 175, 215),
    /* 111 */ (135, 175, 255),
    /* 112 */ (135, 215, 0),
    /* 113 */ (135, 215, 95),
    /* 114 */ (135, 215, 135),
    /* 115 */ (135, 215, 175),
    /* 116 */ (135, 215, 215),
    /* 117 */ (135, 215, 255),
    /* 118 */ (135, 255, 0),
    /* 119 */ (135, 255, 95),
    /* 120 */ (135, 255, 135),
    /* 121 */ (135, 255, 175),
    /* 122 */ (135, 255, 215),
    /* 123 */ (135, 255, 255),
    /* 124 */ (175, 0, 0),
    /* 125 */ (175, 0, 95),
    /* 126 */ (175, 0, 135),
    /* 127 */ (175, 0, 175),
    /* 128 */ (175, 0, 215),
    /* 129 */ (175, 0, 255),
    /* 130 */ (175, 95, 0),
    /* 131 */ (175, 95, 95),
    /* 132 */ (175, 95, 135),
    /* 133 */ (175, 95, 175),
    /* 134 */ (175, 95, 215),
    /* 135 */ (175, 95, 255),
    /* 136 */ (175, 135, 0),
    /* 137 */ (175, 135, 95),
    /* 138 */ (175, 135, 135),
    /* 139 */ (175, 135, 175),
    /* 140 */ (175, 135, 215),
    /* 141 */ (175, 135, 255),
    /* 142 */ (175, 175, 0),
    /* 143 */ (175, 175, 95),
    /* 144 */ (175, 175, 135),
    /* 145 */ (175, 175, 175),
    /* 146 */ (175, 175, 215),
    /* 147 */ (175, 175, 255),
    /* 148 */ (175, 215, 0),
    /* 149 */ (175, 215, 95),
    /* 150 */ (175, 215, 135),
    /* 151 */ (175, 215, 175),
    /* 152 */ (175, 215, 215),
    /* 153 */ (175, 215, 255),
    /* 154 */ (175, 255, 0),
    /* 155 */ (175, 255, 95),
    /* 156 */ (175, 255, 135),
    /* 157 */ (175, 255, 175),
    /* 158 */ (175, 255, 215),
    /* 159 */ (175, 255, 255),
    /* 160 */ (215, 0, 0),
    /* 161 */ (215, 0, 95),
    /* 162 */ (215, 0, 135),
    /* 163 */ (215, 0, 175),
    /* 164 */ (215, 0, 215),
    /* 165 */ (215, 0, 255),
    /* 166 */ (215, 95, 0),
    /* 167 */ (215, 95, 95),
    /* 168 */ (215, 95, 135),
    /* 169 */ (215, 95, 175),
    /* 170 */ (215, 95, 215),
    /* 171 */ (215, 95, 255),
    /* 172 */ (215, 135, 0),
    /* 173 */ (215, 135, 95),
    /* 174 */ (215, 135, 135),
    /* 175 */ (215, 135, 175),
    /* 176 */ (215, 135, 215),
    /* 177 */ (215, 135, 255),
    /* 178 */ (215, 175, 0),
    /* 179 */ (215, 175, 95),
    /* 180 */ (215, 175, 135),
    /* 181 */ (215, 175, 175),
    /* 182 */ (215, 175, 215),
    /* 183 */ (215, 175, 255),
    /* 184 */ (215, 215, 0),
    /* 185 */ (215, 215, 95),
    /* 186 */ (215, 215, 135),
    /* 187 */ (215, 215, 175),
    /* 188 */ (215, 215, 215),
    /* 189 */ (215, 215, 255),
    /* 190 */ (215, 255, 0),
    /* 191 */ (215, 255, 95),
    /* 192 */ (215, 255, 135),
    /* 193 */ (215, 255, 175),
    /* 194 */ (215, 255, 215),
    /* 195 */ (215, 255, 255),
    /* 196 */ (255, 0, 0),
    /* 197 */ (255, 0, 95),
    /* 198 */ (255, 0, 135),
    /* 199 */ (255, 0, 175),
    /* 200 */ (255, 0, 215),
    /* 201 */ (255, 0, 255),
    /* 202 */ (255, 95, 0),
    /* 203 */ (255, 95, 95),
    /* 204 */ (255, 95, 135),
    /* 205 */ (255, 95, 175),
    /* 206 */ (255, 95, 215),
    /* 207 */ (255, 95, 255),
    /* 208 */ (255, 135, 0),
    /* 209 */ (255, 135, 95),
    /* 210 */ (255, 135, 135),
    /* 211 */ (255, 135, 175),
    /* 212 */ (255, 135, 215),
    /* 213 */ (255, 135, 255),
    /* 214 */ (255, 175, 0),
    /* 215 */ (255, 175, 95),
    /* 216 */ (255, 175, 135),
    /* 217 */ (255, 175, 175),
    /* 218 */ (255, 175, 215),
    /* 219 */ (255, 175, 255),
    /* 220 */ (255, 215, 0),
    /* 221 */ (255, 215, 95),
    /* 222 */ (255, 215, 135),
    /* 223 */ (255, 215, 175),
    /* 224 */ (255, 215, 215),
    /* 225 */ (255, 215, 255),
    /* 226 */ (255, 255, 0),
    /* 227 */ (255, 255, 95),
    /* 228 */ (255, 255, 135),
    /* 229 */ (255, 255, 175),
    /* 230 */ (255, 255, 215),
    /* 231 */ (255, 255, 255),
    /* 232 */ (8, 8, 8),
    /* 233 */ (18, 18, 18),
    /* 234 */ (28, 28, 28),
    /* 235 */ (38, 38, 38),
    /* 236 */ (48, 48, 48),
    /* 237 */ (58, 58, 58),
    /* 238 */ (68, 68, 68),
    /* 239 */ (78, 78, 78),
    /* 240 */ (88, 88, 88),
    /* 241 */ (98, 98, 98),
    /* 242 */ (108, 108, 108),
    /* 243 */ (118, 118, 118),
    /* 244 */ (128, 128, 128),
    /* 245 */ (138, 138, 138),
    /* 246 */ (148, 148, 148),
    /* 247 */ (158, 158, 158),
    /* 248 */ (168, 168, 168),
    /* 249 */ (178, 178, 178),
    /* 250 */ (188, 188, 188),
    /* 251 */ (198, 198, 198),
    /* 252 */ (208, 208, 208),
    /* 253 */ (218, 218, 218),
    /* 254 */ (228, 228, 228),
    /* 255 */ (238, 238, 238),
];
