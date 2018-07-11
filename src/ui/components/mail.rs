use ui::components::*;
use ui::cells::*;

fn make_entry_string(e: &Envelope, idx: usize) -> String {
    format!("{}    {}    {:.85}",idx,&e.get_datetime().format("%Y-%m-%d %H:%M:%S").to_string(),e.get_subject())
}

const max_width: usize = 500;

pub struct MailListing {
    cursor_pos: usize,
    new_cursor_pos: usize,
    length: usize,
    // sorting
    content: CellBuffer,
    dirty: bool,
    unfocused: bool,
    // content (2-d vec of bytes) or Cells?
    // current view on top of content
    // active or not? for key events
    pub mailbox: Mailbox,
    pager: Option<Pager>,

}

impl MailListing {
    pub fn new(mailbox: Mailbox) -> Self {
        let length = mailbox.get_length();

        MailListing {
            cursor_pos: 0,
            new_cursor_pos: 0,
            length: length,
            content: CellBuffer::new(max_width, length+1, Cell::with_char(' ')),
            dirty: false,
            unfocused: false,
            mailbox: mailbox,
            pager: None,
        }
    }
}


impl MailListing {
    fn draw_list(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        if self.length == 0 {
            write_string_to_grid(&format!("Folder `{}` is empty.", self.mailbox.path), grid, Color::Default, Color::Default, upper_left, upper_left);
            return;
        }
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
                for _y in y..get_y(bottom_right) {
                    for x in get_x(upper_left)..get_x(bottom_right) {
                        grid[(x,_y)].set_ch(' ');
                        grid[(x,_y)].set_bg(Color::Default);
                        grid[(x,_y)].set_fg(Color::Default);
                    }
                }
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

    fn draw_mail_view(&mut self, grid: &mut CellBuffer, upper_left: Pos, bottom_right: Pos) {
        //Pager
        let ref mail = self.mailbox.collection[self.cursor_pos];
        let height = get_y(bottom_right) - get_y(upper_left);
        let width = get_x(bottom_right) - get_x(upper_left);
        self.pager = Some(Pager::new(mail, height, width));
        let pager = self.pager.as_mut().unwrap();
        pager.dirty = true;
        pager.draw(grid, upper_left,bottom_right);
        
        /*
        let text = mail.get_body().get_text();
        let lines: Vec<&str> = text.trim().split('\n').collect();
        let lines_length = lines.len();

        for y in get_y(upper_left)..get_y(bottom_right) {
            for x in get_x(upper_left)..get_x(bottom_right) {
                grid[(x,y)].set_ch(' ');
                grid[(x,y)].set_bg(Color::Default);
            }
        }

        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(l, grid, Color::Default, Color::Default, set_y(upper_left, get_y(upper_left)+i), bottom_right);
        }
        */
    }
    fn redraw_cursor(&mut self, _upper_left: Pos, _bottom_right: Pos, _grid: &mut CellBuffer) {

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
            /* TODO: ratio in Configuration */
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
            for i in get_x(upper_left)..get_x(bottom_right)+1 {
                grid[(i, mid)].set_ch('─');
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
