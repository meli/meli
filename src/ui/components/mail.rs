use ui::components::*;
use ui::cells::*;


const MAX_COLS: usize = 500;

/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `Pager`.
pub struct MailListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    // TODO: sorting
    /// Cache current view.
    content: CellBuffer,
    dirty: bool,
    /// If `self.pager` exists or not.
    unfocused: bool,
    pager: Option<Pager>,
}

impl MailListing {
    fn make_entry_string(e: &Envelope, idx: usize) -> String {
        format!("{}    {}    {:.85}",idx,&e.get_datetime().format("%Y-%m-%d %H:%M:%S").to_string(),e.get_subject())
    }




    pub fn new(mailbox: Mailbox) -> Self {
        let mut content = CellBuffer::new(0, 0, Cell::with_char(' '));
        MailListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            content: content,
            dirty: true,
            unfocused: false,
            pager: None,
        }
    }
    fn refresh_mailbox(&mut self, context: &mut Context) {
        self.dirty = true;
        self.cursor_pos.2 = 0;
        self.new_cursor_pos.2 = 0;
        self.cursor_pos.1 = self.new_cursor_pos.1;
        let mailbox = &mut context.accounts[self.cursor_pos.0][self.cursor_pos.1].as_ref().unwrap().as_ref().unwrap();
        context.replies.push_back(UIEvent { id: 0, event_type: UIEventType::RefreshMailbox(mailbox.clone()) });
        self.length = mailbox.len();
        let mut content = CellBuffer::new(MAX_COLS, self.length+1, Cell::with_char(' '));
        if self.length == 0 {
            write_string_to_grid(&format!("Folder `{}` is empty.",
                                          mailbox.folder.get_name()),
                                          &mut content,
                                          Color::Default,
                                          Color::Default,
                                          ((0, 0), (MAX_COLS-1, 0)));
            self.content = content;
            return;
        }
        let mut idx = 0;
        for y in 0..=self.length {
            if idx >= self.length {
                clear_area(&mut content,
                           ((0, y), (MAX_COLS-1, self.length)));
                break;
            }
            /* Write an entire line for each envelope entry. */
            let envelope: &Envelope = &mailbox.collection[idx];

            let fg_color = if !envelope.is_seen() {
                Color::Byte(0)
            } else {
                Color::Default
            };
            let bg_color = if !envelope.is_seen() {
                Color::Byte(251)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            };
            let x = write_string_to_grid(&MailListing::make_entry_string(envelope, idx),
                                        &mut content,
                                        fg_color,
                                        bg_color,
                                        ((0, y) , (MAX_COLS-1, y)));

            for x in x..MAX_COLS {
                content[(x,y)].set_ch(' ');
                content[(x,y)].set_bg(bg_color);
            }

            idx+=1;
        }

        self.content = content;
    }
    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize, context: &mut Context) {
        let mailbox = &mut context.accounts[self.cursor_pos.0][self.cursor_pos.1].as_ref().unwrap().as_ref().unwrap();
        let envelope: &Envelope = &mailbox.collection[idx];

        let fg_color = if !envelope.is_seen() {
            Color::Byte(0)
        } else {
            Color::Default
        };
        let bg_color = if self.cursor_pos.2 != idx {
            if !envelope.is_seen() {
                Color::Byte(252)
            } else if idx % 2 == 0 {
                Color::Byte(236)
            } else {
                Color::Default
            }
        } else {
            Color::Byte(246)
        };
        change_colors(grid, area, fg_color, bg_color);
    }

    /// Draw only the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 {
            self.refresh_mailbox(context);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS-1, 0)));
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;
        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let idx = page_no*rows;


        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos.2 != self.new_cursor_pos.2 && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for idx in [old_cursor_pos.2, self.new_cursor_pos.2].iter() {
                if *idx >= self.length {
                    continue; //bounds check
                }
                let new_area = (set_y(upper_left, get_y(upper_left)+(*idx % rows)), set_y(bottom_right, get_y(upper_left) + (*idx % rows)));
                self.highlight_line(grid, new_area, *idx, context);
                context.dirty_areas.push_back(new_area);
            }
            return;
        }  else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }

        /* Page_no has changed, so draw new page */
        copy_area(grid, &self.content, area, ((0, idx), (MAX_COLS - 1, self.length)));
        self.highlight_line(grid, (set_y(upper_left, get_y(upper_left)+(idx % rows)), set_y(bottom_right, get_y(upper_left) + (idx % rows))), self.cursor_pos.2, context);
        context.dirty_areas.push_back(area);
    }

    /// Create a pager for the `Envelope` currently under the cursor.
    fn draw_mail_view(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        {
        let mailbox = &mut context.accounts[self.cursor_pos.0][self.cursor_pos.1].as_ref().unwrap().as_ref().unwrap();
        let envelope: &Envelope = &mailbox.collection[self.cursor_pos.2];

        self.pager = Some(Pager::new(envelope));
        }
        self.pager.as_mut().map(|p| p.draw(grid, area, context));
    }
}

impl Component for MailListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.unfocused {
            if !self.is_dirty() {
                return;
            }
            self.dirty = false;
            /* Draw the entire list */
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
            let headers_rows: usize = 6;
            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            /* TODO: define ratio in Configuration file */
            let pager_ratio = context.settings.pager.pager_ratio;
            let mut bottom_entity_rows = (pager_ratio*total_rows )/100;
            if bottom_entity_rows < headers_rows + 2 {
                bottom_entity_rows = headers_rows + 2;
            }
            if bottom_entity_rows > total_rows {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                return;
            }
            let mid = get_y(upper_left) + total_rows - bottom_entity_rows;

            if !self.dirty {
                if let Some(ref mut p) = self.pager {
                    p.draw(grid,
                           ((get_x(upper_left), get_y(upper_left) +  mid + headers_rows + 1), bottom_right),
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

                for i in get_x(upper_left)..=get_x(bottom_right) {
                    grid[(i, mid)].set_ch('─');
                }
            }

            /* Draw header */
            {
                let mailbox = &mut context.accounts[self.cursor_pos.0][self.cursor_pos.1].as_ref().unwrap().as_ref().unwrap();
                let envelope: &Envelope = &mailbox.collection[self.cursor_pos.2];

                let x = write_string_to_grid(&format!("Date: {}", envelope.get_date_as_str()),
                grid,
                Color::Byte(33),
                Color::Default,
                (set_y(upper_left, mid+1), set_y(bottom_right, mid+1)));
                for x in x..=get_x(bottom_right) {
                    grid[(x, mid+1)].set_ch(' ');
                    grid[(x, mid+1)].set_bg(Color::Default);
                    grid[(x, mid+1)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("From: {}", envelope.get_from()),
                grid,
                Color::Byte(33),
                Color::Default,
                (set_y(upper_left, mid+2), set_y(bottom_right, mid+2)));
                for x in x..=get_x(bottom_right) {
                    grid[(x, mid+2)].set_ch(' ');
                    grid[(x, mid+2)].set_bg(Color::Default);
                    grid[(x, mid+2)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("To: {}", envelope.get_to()),
                grid,
                Color::Byte(33),
                Color::Default,
                (set_y(upper_left, mid+3), set_y(bottom_right, mid+3)));
                for x in x..=get_x(bottom_right) {
                    grid[(x, mid+3)].set_ch(' ');
                    grid[(x, mid+3)].set_bg(Color::Default);
                    grid[(x, mid+3)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Subject: {}", envelope.get_subject()),
                grid,
                Color::Byte(33),
                Color::Default,
                (set_y(upper_left, mid+4), set_y(bottom_right, mid+4)));
                for x in x..=get_x(bottom_right) {
                    grid[(x, mid+4)].set_ch(' ');
                    grid[(x, mid+4)].set_bg(Color::Default);
                    grid[(x, mid+4)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Message-ID: {}", envelope.get_message_id_raw()),
                grid,
                Color::Byte(33),
                Color::Default,
                (set_y(upper_left, mid+5), set_y(bottom_right, mid+5)));
                for x in x..=get_x(bottom_right) {
                    grid[(x, mid+5)].set_ch(' ');
                    grid[(x, mid+5)].set_bg(Color::Default);
                    grid[(x, mid+5)].set_fg(Color::Default);
                }
            }
            clear_area(grid,
                       (set_y(upper_left, mid+headers_rows), set_y(bottom_right, mid+headers_rows)));


            context.dirty_areas.push_back((set_y(upper_left, mid), set_y(bottom_right, mid+headers_rows)));

            /* Draw body */
            self.draw_mail_view(grid,
                                ((get_x(upper_left), get_y(upper_left) +  mid + headers_rows + 1), bottom_right),
                                context);

        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos.2 > 0 {
                    self.new_cursor_pos.2 -= 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos.2 < self.length - 1 {
                    self.new_cursor_pos.2 += 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Char('\n')) if self.unfocused == false => {
                self.unfocused = true;
                self.dirty = true;

            },
            UIEventType::Input(Key::Esc) | UIEventType::Input(Key::Char('i')) if self.unfocused == true => {
                self.unfocused = false;
                self.dirty = true;
                self.pager = None;

            },
            UIEventType::Input(Key::Char(k @ 'J')) | UIEventType::Input(Key::Char(k @ 'K')) => {
                let folder_length = context.accounts[self.cursor_pos.0].len();
                match k {
                    'J' if folder_length > 0 && self.new_cursor_pos.1 < folder_length - 1 => {
                        self.new_cursor_pos.1 = self.cursor_pos.1 + 1;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    },
                    'K' if self.cursor_pos.1 > 0 => {
                        self.new_cursor_pos.1 = self.cursor_pos.1 - 1;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    },
                    _ => {
                    },
                }
            },
            UIEventType::Input(Key::Char(k @ 'h')) | UIEventType::Input(Key::Char(k @ 'l')) => {
                let accounts_length = context.accounts.len();
                match k {
                    'h' if accounts_length > 0 && self.new_cursor_pos.0 < accounts_length - 1 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 + 1;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    },
                    'l' if self.cursor_pos.0 > 0 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 - 1;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    },
                    _ => {
                    },
                }
            },
            UIEventType::RefreshMailbox(ref m) => {
                self.dirty = true;
                self.pager = None;
            },
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
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
struct AccountMenuEntry {
    name: String,
    index: usize,
    entries: Vec<(usize, Folder)>,
}


#[derive(Debug)]
pub struct AccountMenu {
    accounts: Vec<AccountMenuEntry>,
    dirty: bool,
    cursor: Option<(usize, usize)>,
}

impl AccountMenu {
    pub fn new(accounts: &Vec<Account>) -> Self {
        let accounts = accounts.iter().enumerate().map(|(i, a)| { 
            AccountMenuEntry {
                name: a.get_name().to_string(),
                index: i,
                entries: {
                    let mut entries = Vec::with_capacity(a.len());
                    for (idx, acc) in a.list_folders().iter().enumerate() {
                        entries.push((idx, acc.clone()));
                    }
                    entries}
            }
        }).collect();
        AccountMenu {
            accounts: accounts,
            dirty: true,
            cursor: None,
        }
    }
    fn highlight_folder(&mut self, m: &Mailbox) {
        self.dirty = true;
        self.cursor = None;
    }
    fn print_account(&self, grid: &mut CellBuffer, area: Area, a: &AccountMenuEntry) -> usize {
        if !is_valid_area!(area) {
            eprintln!("BUG: invalid area in print_account");
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let mut parents: Vec<Option<usize>> = vec!(None; a.entries.len());

        for (idx, e) in a.entries.iter().enumerate() {
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

        let mut inc = 0;
        let mut depth = String::from("");
        let mut s = String::from(format!("\n\n  {}\n", a.name));
        fn pop(depth: &mut String) {
            depth.pop();
            depth.pop();
        }

        fn push(depth: &mut String, c: char) {
            depth.push(c);
        }

        fn print(root: usize, parents: &Vec<Option<usize>>, depth: &mut String, entries: &Vec<(usize, Folder)>, s: &mut String, inc: &mut usize) -> () {
            let len = s.len();
            s.insert_str(len, &format!("{} {}\n  ", *inc,  &entries[root].1.get_name()));
            *inc += 1;
            let children_no = entries[root].1.get_children().len();
            for (idx, child) in entries[root].1.get_children().iter().enumerate() {
                let len = s.len();
                s.insert_str(len, &format!("{}├─", depth));
                push(depth, if idx == children_no - 1 {'│'} else { ' ' });
                print(*child, parents, depth, entries, s, inc);
                pop(depth);
            }
        }
        for r in roots {
            print(r, &parents, &mut depth, &a.entries, &mut s, &mut inc);

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
        idx

    }
}

impl Component for AccountMenu {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        clear_area(grid, area);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            y += self.print_account(grid,
                                    (set_y(upper_left, y), bottom_right),
                                    &a);
        }

        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) {
        match event.event_type {
            UIEventType::RefreshMailbox(ref m) => {
                self.highlight_folder(m);
            },
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            },
            _ => {
            },
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
}
