use ui::components::*;
use ui::cells::*;

fn make_entry_string(e: &Envelope, idx: usize) -> String {
    format!("{}    {}    {:.85}",idx,&e.get_datetime().format("%Y-%m-%d %H:%M:%S").to_string(),e.get_subject())
}

const MAX_COLS: usize = 500;

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `Pager`.
pub struct MailListing {
    cursor_pos: usize,
    new_cursor_pos: usize,
    length: usize,
    // sorting
    content: CellBuffer,
    dirty: bool,
    /// If `self.pager` exists or not.
    unfocused: bool,
    // content (2-d vec of bytes) or Cells?
    // current view on top of content
    // active or not? for key events
    pub mailbox: Mailbox,
    pager: Option<Pager>,

}

impl MailListing {
    pub fn new(mailbox: Mailbox) -> Self {
        let length = mailbox.len();

        MailListing {
            cursor_pos: 0,
            new_cursor_pos: 0,
            length: length,
            content: CellBuffer::new(MAX_COLS, length+1, Cell::with_char(' ')),
            dirty: true,
            unfocused: false,
            mailbox: mailbox,
            pager: None,
        }
    }
}


impl MailListing {
    /// Draw only the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area);
            let new_area = (upper_left, set_x(upper_left, get_x(bottom_right)));
            write_string_to_grid(&format!("Folder `{}` is empty.",
                                          self.mailbox.folder.get_name()),
                                          grid,
                                          Color::Default,
                                          Color::Default,
                                          new_area);
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;
        let prev_page_no = (self.cursor_pos).wrapping_div(rows);
        let page_no = (self.new_cursor_pos).wrapping_div(rows);


        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos != self.new_cursor_pos && prev_page_no == page_no {
            for idx in [self.cursor_pos, self.new_cursor_pos].iter() {
                if *idx >= self.length {
                    continue; //bounds check
                }
                let envelope: &Envelope = &self.mailbox.collection[*idx];

                let fg_color = if !envelope.is_seen() {
                    Color::Byte(0)
                } else {
                    Color::Default
                };
                let bg_color = if self.cursor_pos == *idx {
                    if !envelope.is_seen() {
                        Color::Byte(252)
                    } else if *idx % 2 == 0 {
                        Color::Byte(236)
                    } else {
                        Color::Default
                    }
                } else {
                    Color::Byte(246)
                };
                let new_area = (set_y(upper_left, get_y(upper_left)+(*idx % rows)), bottom_right);
                let x = write_string_to_grid(&make_entry_string(envelope, *idx),
                                             grid,
                                             fg_color,
                                             bg_color,
                                             new_area);
                for x in x..=get_x(bottom_right) {
                    grid[(x,get_y(upper_left)+(*idx % rows))].set_ch(' ');
                    grid[(x,get_y(upper_left)+(*idx % rows))].set_bg(bg_color);
                }
                context.dirty_areas.push_back(new_area);
            }
            self.cursor_pos = self.new_cursor_pos;
            return;
        }  else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }

        context.dirty_areas.push_back(area);
        let mut idx = page_no*rows;
        for y in get_y(upper_left)..=get_y(bottom_right) {
            if idx >= self.length {
                clear_area(grid,
                           (set_y(upper_left, y), bottom_right));
                break;
            }
            /* Write an entire line for each envelope entry. */
            let envelope: &Envelope = &self.mailbox.collection[idx];

            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if self.cursor_pos == idx {
                Color::Byte(246)
            } else {
                if !envelope.is_seen() {
                    Color::Byte(251)
                } else if idx % 2 == 0 {
                        Color::Byte(236)
                } else {
                    Color::Default
                }
            };
            let x = write_string_to_grid(&make_entry_string(envelope, idx),
                                         grid,
                                         fg_color,
                                         bg_color,
                                         (set_y(upper_left, y), bottom_right));

            for x in x..=get_x(bottom_right) {
                grid[(x,y)].set_ch(' ');
                grid[(x,y)].set_bg(bg_color);
            }

            idx+=1;
        }
    }

    /// Create a pager for the `Envelope` currently under the cursor.
    fn draw_mail_view(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let ref mail = self.mailbox.collection[self.cursor_pos];

        let rows = get_y(bottom_right) - get_y(upper_left);
        let cols = get_x(bottom_right) - get_x(upper_left);

        self.pager = Some(Pager::new(mail, rows, cols));
        self.pager.as_mut().map(|p| p.draw(grid, area, context));
    }
}

impl Component for MailListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.dirty {
                return;
            }
            self.dirty = false;
            /* Draw the entire list */
            let upper_left = upper_left!(area);
            let bottom_right = bottom_right!(area);
            self.draw_list(grid,
                           area,
                           context);
        } else {
            let upper_left = upper_left!(area);
            let bottom_right = bottom_right!(area);
            if self.length == 0 && self.dirty {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
            }
            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            /* TODO: define ratio in Configuration file */
            let bottom_entity_rows = (80*total_rows )/100;
            let mid = get_y(upper_left) + total_rows - bottom_entity_rows;

            if !self.dirty {
                if let Some(ref mut p) = self.pager {
                    p.draw(grid,
                           ((get_x(upper_left), get_y(upper_left) +  mid+6), bottom_right),
                           context);
                }
                return;
            }
            self.dirty = false;
            self.draw_list(grid,
                           (upper_left, (get_x(bottom_right), get_y(upper_left)+ mid-1)),
                           context);
            if self.length == 0 {
                return;
            }
            {
                /* TODO: Move the box drawing business in separate functions */
                if get_x(upper_left) > 0 {
                    if grid[(get_x(upper_left) - 1, mid)].ch() == VERT_BOUNDARY {
                        grid[(get_x(upper_left) - 1, mid)].set_ch(LIGHT_VERTICAL_AND_RIGHT);
                    }
                }

                for i in get_x(upper_left)..get_x(bottom_right) {
                    grid[(i, mid)].set_ch('─');
                }
            }

            let headers_rows: usize = 6;
            /* Draw header */
            {
                let ref mail = self.mailbox.collection[self.cursor_pos];

                eprintln!("writing headers {} {}", mail.get_date_as_str(), mail.get_subject());
                let x = write_string_to_grid(&format!("Date: {}", mail.get_date_as_str()),
                                             grid,
                                             Color::Byte(33),
                                             Color::Default,
                                             (set_y(upper_left, mid+1), set_y(bottom_right, mid+1)));
                for x in x..get_x(bottom_right) {
                    grid[(x, mid+1)].set_ch(' ');
                    grid[(x, mid+1)].set_bg(Color::Default);
                    grid[(x, mid+1)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("From: {}", mail.get_from()),
                                                      grid,
                                                      Color::Byte(33),
                                                      Color::Default,
                                                      (set_y(upper_left, mid+2), set_y(bottom_right, mid+2)));
                for x in x..get_x(bottom_right) {
                    grid[(x, mid+2)].set_ch(' ');
                    grid[(x, mid+2)].set_bg(Color::Default);
                    grid[(x, mid+2)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("To: {}", mail.get_to()),
                                                      grid,
                                                      Color::Byte(33),
                                                      Color::Default,
                                                      (set_y(upper_left, mid+3), set_y(bottom_right, mid+3)));
                for x in x..get_x(bottom_right) {
                    grid[(x, mid+3)].set_ch(' ');
                    grid[(x, mid+3)].set_bg(Color::Default);
                    grid[(x, mid+3)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Subject: {}", mail.get_subject()),
                                                      grid,
                                                      Color::Byte(33),
                                                      Color::Default,
                                                      (set_y(upper_left, mid+4), set_y(bottom_right, mid+4)));
                for x in x..get_x(bottom_right) {
                    grid[(x, mid+4)].set_ch(' ');
                    grid[(x, mid+4)].set_bg(Color::Default);
                    grid[(x, mid+4)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Message-ID: {}", mail.get_message_id_raw()),
                                                      grid,
                                                      Color::Byte(33),
                                                      Color::Default,
                                                      (set_y(upper_left, mid+5), set_y(bottom_right, mid+5)));
                for x in x..get_x(bottom_right) {
                    grid[(x, mid+5)].set_ch(' ');
                    grid[(x, mid+5)].set_bg(Color::Default);
                    grid[(x, mid+5)].set_fg(Color::Default);
                }
            }
            context.dirty_areas.push_back((set_y(upper_left, mid), set_y(bottom_right, mid+5)));

            /* Draw body */
            self.draw_mail_view(grid,
                                ((get_x(upper_left), get_y(upper_left) +  mid + headers_rows), bottom_right),
                                context);

        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos -= 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos < self.length - 1 {
                    self.new_cursor_pos += 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Char('\n')) if self.unfocused == false => {
                self.unfocused = true;
                self.dirty = true;

            },
            UIEventType::Input(Key::Esc) if self.unfocused == true => {
                self.unfocused = false;
                self.dirty = true;
                self.pager = None;

            },
            UIEventType::RefreshMailbox(ref m) => {
                self.cursor_pos = 0;
                self.new_cursor_pos = 0;
                self.mailbox = m.clone();
                self.length = m.collection.len();
                self.dirty = true;
                self.pager = None;
            },
            _ => {
            },
        }
        if let Some(ref mut p) = self.pager {
            p.process_event(event, context);
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.pager.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
}

#[derive(Debug)]
pub struct AccountMenu {
    entries: Vec<(usize, Folder)>,
    dirty: bool,
    name: String,
    cursor: Option<String>,
}

impl AccountMenu {
    pub fn new(account: &Account) -> Self{
        let mut entries = Vec::with_capacity(account.len());
        for (idx, acc) in account.list_folders().iter().enumerate() {
            entries.push((idx, acc.clone()));
        }

        AccountMenu {
            entries: entries,
            dirty: true,
            name: account.get_name().to_string(),
            cursor: None,
        }
    }
    fn highlight_folder(&mut self, m: &Mailbox) {
        self.dirty = true;
        self.cursor = Some(m.folder.get_name().to_string());
    }
}

impl Component for AccountMenu {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !(self.dirty) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut parents: Vec<Option<usize>> = vec!(None; self.entries.len());

        for (idx, e) in self.entries.iter().enumerate() {
            for c in e.1.get_children() {
                parents[*c] = Some(idx);
            }
        }
        let mut roots = Vec::new();
        for (idx, c) in parents.iter().enumerate() {
            if c.is_none() {
                roots.push(idx);
            }
        }

        let mut ind = 0;
        let mut depth = String::from("   ");
        let mut s = String::from(format!("\n\n  {}\n", self.name));
        fn pop(depth: &mut String) {
            depth.pop();
            depth.pop();
            depth.pop();
            depth.pop();
        }

        fn push(depth: &mut String, c: char) {
            depth.push(' ');
            depth.push(c);
            depth.push(' ');
            depth.push(' ');
        }

        fn print(root: usize, parents: &Vec<Option<usize>>, depth: &mut String, entries: &Vec<(usize, Folder)>, mut s: String) -> String {
            let len = s.len();
            s.insert_str(len, &format!("{}: {}\n  ", entries[root].0, &entries[root].1.get_name()));
            let children_no = entries[root].1.get_children().len();
            for (idx, child) in entries[root].1.get_children().iter().enumerate() {
                let len = s.len();
                s.insert_str(len, &format!("{}├─", depth));
                push(depth, if idx == children_no - 1 {'│'} else { ' ' });
                s = print(*child, parents, depth, entries, s);
                pop(depth);
            }
            s
        }
        for r in roots {
            s =print(r, &parents, &mut depth, &self.entries, s);

        }

        let lines: Vec<&str> = s.lines().collect();
        let lines_len = lines.len();
        let mut idx = 0;
        for y in get_y(upper_left)..get_y(bottom_right) {
            if idx == lines_len {
                break;
            }
            let s = if idx == lines_len - 2 {
                format!("{}", lines[idx].replace("├", "└"))
            } else {
                format!("{}", lines[idx])
            };
            write_string_to_grid(&s,
                                 grid,
                                 Color::Byte(30),
                                 Color::Default,
                                 (set_y(upper_left, y), bottom_right));
            idx += 1;
        }
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) {
        match event.event_type {
            UIEventType::RefreshMailbox(ref m) => {
                self.highlight_folder(m);
            },
            _ => {
            },
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
}
