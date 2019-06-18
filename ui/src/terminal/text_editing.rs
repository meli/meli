use melib::Graphemes;

#[derive(Debug, Clone, Default, PartialEq)]
pub struct UText {
    content: String,
    cursor_pos: usize,
    grapheme_cursor_pos: usize,
}

impl UText {
    pub fn new(content: String) -> Self {
        UText {
            content,
            cursor_pos: 0,
            grapheme_cursor_pos: 0,
        }
    }

    pub fn set_cursor(&mut self, cursor_pos: usize) {
        if cursor_pos > self.content.len() {
            return;
        }

        let (first, _) = self.content.split_at(cursor_pos);
        self.grapheme_cursor_pos = first.split_graphemes().len();
        self.cursor_pos = cursor_pos;
    }

    pub fn as_str(&self) -> &str {
        self.content.as_str()
    }

    pub fn clear(&mut self) {
        self.content.clear();
        self.cursor_pos = 0;
        self.grapheme_cursor_pos = 0;
    }

    pub fn into_string(self) -> String {
        self.content
    }
    pub fn grapheme_len(&self) -> usize {
        self.content.split_graphemes().len()
    }

    pub fn cursor_inc(&mut self) {
        if self.cursor_pos >= self.content.len() {
            return;
        }

        let (_, right) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = right.next_grapheme() {
            self.cursor_pos += graph.len();
            self.grapheme_cursor_pos += 1;
        }
    }
    pub fn cursor_dec(&mut self) {
        if self.cursor_pos == 0 {
            return;
        }
        let (left, _) = self.content.split_at(self.cursor_pos);
        if let Some((_, graph)) = left.last_grapheme() {
            self.cursor_pos -= graph.len();
            self.grapheme_cursor_pos -= 1;
        }
    }

    pub fn cursor_pos(&self) -> usize {
        self.cursor_pos
    }

    pub fn grapheme_pos(&self) -> usize {
        self.grapheme_cursor_pos
    }

    /*
     * Insert code point `k` in position `self.cursor_pos`:
     *
     * before:
     *
     * self.content = xxxxxx....xxxxxxx;
     *                             ^
     *                      self.cursor_pos
     *
     * after:
     *
     * self.content = xxxxxx....xxxxkxxx;
     *                              ^
     *                       self.cursor_pos
     */
    pub fn insert_char(&mut self, k: char) {
        self.content.insert(self.cursor_pos, k);
        self.cursor_pos += k.len_utf8();
        self.grapheme_cursor_pos += 1;
    }

    /*
     * remove grapheme cluster that ends on `self.cursor_pos`:
     *
     * before:
     *
     * self.content = xxxxxx....xxggxxx;
     *                             ^
     *                      self.cursor_pos
     *
     * after:
     *
     * self.content = xxxxxx....xxxxxx;
     *                           ^
     *                    self.cursor_pos
     */
    pub fn backspace(&mut self) {
        if self.content.is_empty() {
            return;
        }
        let (offset, graph_len) = {
            /*
             * Split string at cursor_pos:
             */
            let (left, _) = self.content.split_at(self.cursor_pos);
            /*
             * left = xxxxxx....xxgg;
             * right = xxx;
             */
            if let Some((offset, graph)) = left.last_grapheme() {
                (offset, graph.len())
            } else {
                return;
            }
        };
        self.cursor_dec();

        self.content
            .drain(std::dbg!(offset..offset + graph_len))
            .count();
    }
}
