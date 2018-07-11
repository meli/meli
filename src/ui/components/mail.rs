use ui::components::*;
use ui::cells::*;

fn make_entry_string(e: &Envelope, idx: usize) -> String {
    format!("{}    {}    {:.85}",idx,&e.get_datetime().format("%Y-%m-%d %H:%M:%S").to_string(),e.get_subject())
}

const MAX_WIDTH: usize = 500;

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
            content: CellBuffer::new(MAX_WIDTH, length+1, Cell::with_char(' ')),
            dirty: false,
            unfocused: false,
            mailbox: mailbox,
            pager: None,
        }
    }
}


impl MailListing {
    /// Draw only the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        if self.length == 0 {
            write_string_to_grid(&format!("Folder `{}` is empty.", self.mailbox.path), grid, Color::Default, Color::Default, upper_left, upper_left);
            return;
        }
        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos != self.new_cursor_pos {
            for idx in [self.cursor_pos, self.new_cursor_pos].iter() {
                let color = if self.cursor_pos == *idx { if *idx % 2 == 0 { Color::Byte(236) } else {Color::Default } } else { Color::Byte(246) };
                let x = write_string_to_grid(&make_entry_string(&self.mailbox.collection[*idx], *idx), grid, Color::Default,  color, set_y(upper_left, get_y(upper_left) + *idx), bottom_right);
                for x in x..get_x(bottom_right)+1 {
                    grid[(x,get_y(upper_left)+idx)].set_ch(' ');
                    grid[(x,get_y(upper_left)+idx)].set_bg(color);
                }
            }
            self.cursor_pos = self.new_cursor_pos;
            return;
        }

        let mut idx = 0;
        for y in get_y(upper_left)..get_y(bottom_right) {
            if idx == self.length {
                clear_area(grid, set_y(upper_left, y), bottom_right);
                break;
            }
            /* Write an entire line for each envelope entry. */

            let color = if self.cursor_pos == idx { Color::Byte(246) } else { if idx % 2 == 0 { Color::Byte(236) } else {Color::Default } };
            let x = write_string_to_grid(&make_entry_string(&self.mailbox.collection[idx], idx), grid, Color::Default, color, set_y(upper_left, y), bottom_right);

            for x in x..get_x(bottom_right)+1 {
                grid[(x,y)].set_ch(' ');
                grid[(x,y)].set_bg(color);
            }

            idx+=1;
        }
    }

    /// Create a pager for the `Envelope` currently under the cursor.
    fn draw_mail_view(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        let ref mail = self.mailbox.collection[self.cursor_pos];

        let height = get_y(bottom_right) - get_y(upper_left);
        let width = get_x(bottom_right) - get_x(upper_left);

        self.pager = Some(Pager::new(mail, height, width));
        let pager = self.pager.as_mut().unwrap();
        pager.draw(grid, upper_left,bottom_right);
    }
}

impl Component for MailListing {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        if !self.unfocused {
            if !self.dirty {
                return;
            }
            self.dirty = false;
            /* Draw the entire list */
            self.draw_list(grid, upper_left,bottom_right);
        } else {
            if self.length == 0 && self.dirty {
                clear_area(grid, upper_left, bottom_right);
            }
            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            /* TODO: define ratio in Configuration file */
            let bottom_entity_height = (80*total_rows )/100;
            let mid = get_y(upper_left) + total_rows - bottom_entity_height;

            if !self.dirty {
                if let Some(ref mut p) = self.pager {
                    p.draw(grid, (get_x(upper_left), get_y(upper_left) +  mid+6), bottom_right);
                }
                return;
            }
            self.dirty = false;
            self.draw_list(grid, upper_left, (get_x(bottom_right), get_y(upper_left)+ mid -1));
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

                for i in get_x(upper_left)..get_x(bottom_right)+1 {
                    grid[(i, mid)].set_ch('─');
                }
            }

            let headers_height: usize = 6;
            /* Draw header */
            {
                let ref mail = self.mailbox.collection[self.cursor_pos];

                let x = write_string_to_grid(&format!("Date: {}", mail.get_date_as_str()), grid, Color::Byte(33), Color::Default, set_y(upper_left, mid+1), set_y(upper_left, mid+1));
                for x in x..get_x(bottom_right)+1 {
                    grid[(x, mid+1)].set_ch(' ');
                    grid[(x, mid+1)].set_bg(Color::Default);
                    grid[(x, mid+1)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("From: {}", mail.get_from()), grid, Color::Byte(33), Color::Default, set_y(upper_left, mid+2), set_y(upper_left, mid+2));
                for x in x..get_x(bottom_right)+1 {
                    grid[(x, mid+2)].set_ch(' ');
                    grid[(x, mid+2)].set_bg(Color::Default);
                    grid[(x, mid+2)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("To: {}", mail.get_to()), grid, Color::Byte(33), Color::Default, set_y(upper_left, mid+3), set_y(upper_left, mid+3));
                for x in x..get_x(bottom_right)+1 {
                    grid[(x, mid+3)].set_ch(' ');
                    grid[(x, mid+3)].set_bg(Color::Default);
                    grid[(x, mid+3)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Subject: {}", mail.get_subject()), grid, Color::Byte(33), Color::Default, set_y(upper_left, mid+4), set_y(upper_left, mid+4));
                for x in x..get_x(bottom_right)+1 {
                    grid[(x, mid+4)].set_ch(' ');
                    grid[(x, mid+4)].set_bg(Color::Default);
                    grid[(x, mid+4)].set_fg(Color::Default);
                }
                let x = write_string_to_grid(&format!("Message-ID: {}", mail.get_message_id_raw()), grid, Color::Byte(33), Color::Default, set_y(upper_left, mid+5), set_y(upper_left, mid+5));
                for x in x..get_x(bottom_right)+1 {
                    grid[(x, mid+5)].set_ch(' ');
                    grid[(x, mid+5)].set_bg(Color::Default);
                    grid[(x, mid+5)].set_fg(Color::Default);
                }
            }

            /* Draw body */
            self.draw_mail_view(grid, (get_x(upper_left), get_y(upper_left) +  mid + headers_height), bottom_right);

        }
    }
    fn process_event(&mut self, event: &UIEvent, queue: &mut VecDeque<UIEvent>) {
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos -= 1;
                    self.dirty = true;
                }
            },
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.cursor_pos < self.length - 1 {
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
            p.process_event(event, queue);
        }
    }
}

#[derive(Debug)]
pub struct AccountMenu {
    entries: Vec<(usize, String)>,
    dirty: bool,

}

impl AccountMenu {
    pub fn new(account: &Account) -> Self{
        let mut entries = Vec::with_capacity(account.len());
        for (idx, acc) in account.list_folders().iter().enumerate() {
            eprintln!("acc[{}] is {:?}", idx, acc);
            entries.push((idx, acc.clone()));
        }
        AccountMenu {
            entries: entries,
            dirty: true,
        }
    }
}

impl Component for AccountMenu {
    fn draw(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        eprintln!("reached accountmenu draw {:?}", self);
        if !(self.dirty) {
            return;
        }
        self.dirty = false;
        let mut idx = 0;
        for y in get_y(upper_left)..get_y(bottom_right) {
            if idx == self.entries.len() {
                break;
            }
            let s = format!("{:?}",self.entries[idx]);
            eprintln!("wrote {} to menu", s);
            write_string_to_grid(&s, grid, Color::Red, Color::Default, set_y(upper_left, y), bottom_right);
            idx += 1;
        }
    }
    fn process_event(&mut self, _event: &UIEvent, _queue: &mut VecDeque<UIEvent>) {
        return;
    }
}
