use super::*;

const MAX_COLS: usize = 500;


/// A list of all mail (`Envelope`s) in a `Mailbox`. On `\n` it opens the `Envelope` content in a
/// `MailView`.
pub struct MailListing {
    /// (x, y, z): x is accounts, y is folders, z is index inside a folder.
    cursor_pos: (usize, usize, usize),
    new_cursor_pos: (usize, usize, usize),
    length: usize,
    // TODO: sorting
    /// Cache current view.
    content: CellBuffer,
    /// If we must redraw on next redraw event
    dirty: bool,
    /// If `self.view` exists or not.
    unfocused: bool,
    view: Option<MailView>,
}

impl MailListing {
    /// Helper function to format entry strings for MailListing */
    /* TODO: Make this configurable */
    fn make_entry_string(e: &Envelope, idx: usize) -> String {
        format!(
            "{}    {}    {}",
            idx,
            &e.datetime().format("%Y-%m-%d %H:%M:%S").to_string(),
            e.subject()
        )
    }

    pub fn new() -> Self {
        let content = CellBuffer::new(0, 0, Cell::with_char(' '));
        MailListing {
            cursor_pos: (0, 1, 0),
            new_cursor_pos: (0, 0, 0),
            length: 0,
            content: content,
            dirty: true,
            unfocused: false,
            view: None,
        }
    }
    /// Fill the `self.content` `CellBuffer` with the contents of the account folder the user has
    /// chosen.
    fn refresh_mailbox(&mut self, context: &mut Context) {
        self.dirty = true;
        self.cursor_pos.2 = 0;
        self.new_cursor_pos.2 = 0;
        self.cursor_pos.1 = self.new_cursor_pos.1;
        self.cursor_pos.0 = self.new_cursor_pos.0;

        let threaded = context.accounts[self.cursor_pos.0]
            .runtime_settings
            .threaded;
        // Inform State that we changed the current folder view.
        context.replies.push_back(UIEvent {
            id: 0,
            event_type: UIEventType::RefreshMailbox((self.cursor_pos.0, self.cursor_pos.1)),
        });
        // Get mailbox as a reference.
        //
        loop {
            eprintln!("loop round");
        match context.accounts[self.cursor_pos.0].status(self.cursor_pos.1) {
            Ok(()) => { break; },
            Err(a) => {
                eprintln!("status returned {:?}", a);
            }
        }
        }
        let mailbox = &mut context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();

        self.length = if threaded {
            mailbox.threaded_collection.len()
        } else {
            mailbox.len()
        };
        let mut content = CellBuffer::new(MAX_COLS, self.length + 1, Cell::with_char(' '));
        if self.length == 0 {
            write_string_to_grid(
                &format!("Folder `{}` is empty.", mailbox.folder.name()),
                &mut content,
                Color::Default,
                Color::Default,
                ((0, 0), (MAX_COLS - 1, 0)),
                true,
                );
            self.content = content;
            return;
        }

        // TODO: Fix the threaded hell and refactor stuff into seperate functions and/or modules.
        if threaded {
            let mut indentations: Vec<bool> = Vec::with_capacity(6);
            let mut thread_idx = 0; // needed for alternate thread colors
            /* Draw threaded view. */
            let mut iter = mailbox.threaded_collection.iter().enumerate().peekable();
            let len = mailbox
                .threaded_collection
                .len()
                .to_string()
                .chars()
                .count();
            /* This is just a desugared for loop so that we can use .peek() */
            while let Some((idx, i)) = iter.next() {
                let container = mailbox.thread(*i);
                let indentation = container.indentation();

                if indentation == 0 {
                    thread_idx += 1;
                }

                assert_eq!(container.has_message(), true);
                match iter.peek() {
                    Some(&(_, x)) if mailbox.thread(*x).indentation() == indentation => {
                        indentations.pop();
                        indentations.push(true);
                    }
                    _ => {
                        indentations.pop();
                        indentations.push(false);
                    }
                }
                if container.has_sibling() {
                    indentations.pop();
                    indentations.push(true);
                }
                let envelope: &Envelope = &mailbox.collection[container.message().unwrap()];
                let fg_color = if !envelope.is_seen() {
                    Color::Byte(0)
                } else {
                    Color::Default
                };
                let bg_color = if !envelope.is_seen() {
                    Color::Byte(251)
                } else if thread_idx % 2 == 0 {
                    Color::Byte(236)
                } else {
                    Color::Default
                };
                let (x, _) = write_string_to_grid(
                    &MailListing::make_thread_entry(
                        envelope,
                        idx,
                        indentation,
                        container,
                        &indentations,
                        len,
                        ),
                        &mut content,
                        fg_color,
                        bg_color,
                        ((0, idx), (MAX_COLS - 1, idx)),
                        false,
                        );
                for x in x..MAX_COLS {
                    content[(x, idx)].set_ch(' ');
                    content[(x, idx)].set_bg(bg_color);
                }

                match iter.peek() {
                    Some(&(_, x)) if mailbox.thread(*x).indentation() > indentation => {
                        indentations.push(false);
                    }
                    Some(&(_, x)) if mailbox.thread(*x).indentation() < indentation => {
                        for _ in 0..(indentation - mailbox.thread(*x).indentation()) {
                            indentations.pop();
                        }
                    }
                    _ => {}
                }
            }
        } else {
            // Populate `CellBuffer` with every entry.
            // TODO: Lazy load?
            let mut idx = 0;
            for y in 0..=self.length {
                if idx >= self.length {
                    /* No more entries left, so fill the rest of the area with empty space */
                    clear_area(&mut content, ((0, y), (MAX_COLS - 1, self.length)));
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
                let (x, y) = write_string_to_grid(
                    &MailListing::make_entry_string(envelope, idx),
                    &mut content,
                    fg_color,
                    bg_color,
                    ((0, y), (MAX_COLS - 1, y)),
                    false,
                    );

                for x in x..MAX_COLS {
                    content[(x, y)].set_ch(' ');
                    content[(x, y)].set_bg(bg_color);
                }

                idx += 1;
            }
        }

        self.content = content;
    }
    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        let threaded = context.accounts[self.cursor_pos.0]
            .runtime_settings
            .threaded;
        let mailbox = &context.accounts[self.cursor_pos.0][self.cursor_pos.1]
            .as_ref()
            .unwrap();
        let envelope: &Envelope = if threaded {
            let i = mailbox.threaded_mail(idx);
            &mailbox.collection[i]
        } else {
            &mailbox.collection[idx]
        };

        let fg_color = if !envelope.is_seen() {
            Color::Byte(0)
        } else {
            Color::Default
        };
        let bg_color = if self.cursor_pos.2 == idx {
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
        change_colors(grid, area, fg_color, bg_color);
    }

    /// Draw the list of `Envelope`s.
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.cursor_pos.1 != self.new_cursor_pos.1 {
            self.refresh_mailbox(context);
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        if self.length == 0 {
            clear_area(grid, area);
            copy_area(grid, &self.content, area, ((0, 0), (MAX_COLS - 1, 0)));
            context.dirty_areas.push_back(area);
            return;
        }
        let rows = get_y(bottom_right) - get_y(upper_left) + 1;
        let prev_page_no = (self.cursor_pos.2).wrapping_div(rows);
        let page_no = (self.new_cursor_pos.2).wrapping_div(rows);

        let top_idx = page_no * rows;

        /* If cursor position has changed, remove the highlight from the previous position and
         * apply it in the new one. */
        if self.cursor_pos.2 != self.new_cursor_pos.2 && prev_page_no == page_no {
            let old_cursor_pos = self.cursor_pos;
            self.cursor_pos = self.new_cursor_pos;
            for idx in [old_cursor_pos.2, self.new_cursor_pos.2].iter() {
                if *idx >= self.length {
                    continue; //bounds check
                }
                let new_area = (
                    set_y(upper_left, get_y(upper_left) + (*idx % rows)),
                    set_y(bottom_right, get_y(upper_left) + (*idx % rows)),
                );
                self.highlight_line(grid, new_area, *idx, context);
                context.dirty_areas.push_back(new_area);
            }
            return;
        } else if self.cursor_pos != self.new_cursor_pos {
            self.cursor_pos = self.new_cursor_pos;
        }

        /* Page_no has changed, so draw new page */
        copy_area(
            grid,
            &self.content,
            area,
            ((0, top_idx), (MAX_COLS - 1, self.length)),
        );
        self.highlight_line(
            grid,
            (
                set_y(upper_left, get_y(upper_left) + (self.cursor_pos.2 % rows)),
                set_y(bottom_right, get_y(upper_left) + (self.cursor_pos.2 % rows)),
            ),
            self.cursor_pos.2,
            context,
        );
        context.dirty_areas.push_back(area);
    }

    fn make_thread_entry(
        envelope: &Envelope,
        idx: usize,
        indent: usize,
        container: &Container,
        indentations: &Vec<bool>,
        idx_width: usize,
    ) -> String {
        let has_sibling = container.has_sibling();
        let has_parent = container.has_parent();
        let show_subject = container.show_subject();

        let mut s = format!(
            "{}{}{} ",
            idx,
            " ".repeat(idx_width + 2 - (idx.to_string().chars().count())),
            MailListing::format_date(&envelope)
        );
        for i in 0..indent {
            if indentations.len() > i && indentations[i] {
                s.push('│');
            } else {
                s.push(' ');
            }
            if i > 0 {
                s.push(' ');
            }
        }
        if indent > 0 {
            if has_sibling && has_parent {
                s.push('├');
            } else if has_sibling {
                s.push('┬');
            } else {
                s.push('└');
            }
            s.push('─');
            s.push('>');
        }

        if show_subject {
            s.push_str(&format!("{:.85}", envelope.subject()));
        }
        let attach_count = envelope.body().count_attachments();
        if attach_count > 1 {
            s.push_str(&format!(" {}∞ ", attach_count - 1));
        }
        s
    }
    fn format_date(envelope: &Envelope) -> String {
        let d = std::time::UNIX_EPOCH + std::time::Duration::from_secs(envelope.date());
        let now: std::time::Duration = std::time::SystemTime::now().duration_since(d).unwrap();
        match now.as_secs() {
            n if n < 10 * 60 * 60 => format!("{} hours ago{}", n / (60 * 60), " ".repeat(8)),
            n if n < 24 * 60 * 60 => format!("{} hours ago{}", n / (60 * 60), " ".repeat(7)),
            n if n < 4 * 24 * 60 * 60 => {
                format!("{} days ago{}", n / (24 * 60 * 60), " ".repeat(9))
            }
            _ => envelope.datetime().format("%Y-%m-%d %H:%M:%S").to_string(),
        }
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
            self.draw_list(grid, area, context);
        } else {
            let upper_left = upper_left!(area);
            let bottom_right = bottom_right!(area);
            if self.length == 0 && self.dirty {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
            }

            /* Render the mail body in a pager, basically copy what HSplit does */
            let total_rows = get_y(bottom_right) - get_y(upper_left);
            let pager_ratio = context.runtime_settings.pager.pager_ratio;
            let bottom_entity_rows = (pager_ratio * total_rows) / 100;

            if bottom_entity_rows > total_rows {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                return;
            }
            let mid = get_y(upper_left) + total_rows - bottom_entity_rows;
            self.draw_list(
                grid,
                (
                    upper_left,
                    (get_x(bottom_right), get_y(upper_left) + mid - 1),
                ),
                context,
            );
            if self.length == 0 {
                self.dirty = false;
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
                context
                    .dirty_areas
                    .push_back((set_y(upper_left, mid), set_y(bottom_right, mid)));
            }
            // TODO: Make headers view configurable

            if !self.dirty {
                self.view
                    .as_mut()
                    .map(|v| v.draw(grid, (set_y(upper_left, mid + 1), bottom_right), context));
                return;
            }
            self.view = Some(MailView::new(self.cursor_pos, None, None));
            self.view
                .as_mut()
                .map(|v| v.draw(grid, (set_y(upper_left, mid + 1), bottom_right), context));
            self.dirty = false;
        }
    }
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos.2 > 0 {
                    self.new_cursor_pos.2 -= 1;
                    self.dirty = true;
                }
            }
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos.2 < self.length - 1 {
                    self.new_cursor_pos.2 += 1;
                    self.dirty = true;
                }
            }
            UIEventType::Input(Key::Char('\n')) if self.unfocused == false => {
                self.unfocused = true;
                self.dirty = true;
            }
            UIEventType::Input(Key::Char('m')) if self.unfocused == false => {
                use std::process::{Command, Stdio};
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    /* I tried thread::park() here but for some reason it never blocked and always
                     * returned. Spinlocks are also useless because you have to keep the mutex
                     * guard alive til the child process exits, which requires some effort.
                     *
                     * The only problem with this approach is tht the user has to send some input
                     * in order for the input-thread to wake up and realise it should kill itself.
                     *
                     * I tried writing to stdin/tty manually but for some reason rustty didn't
                     * acknowledge it.
                     */

                    /*
                     * tx sends to input-thread and it kills itself.
                     */
                    let tx = context.input_thread();
                    tx.send(true);
                }
                let mut f = create_temp_file(&new_draft(context), None);
                //let mut f = Box::new(std::fs::File::create(&dir).unwrap());

                // TODO: check exit status
                let mut output = Command::new("vim")
                    .arg("+/^$")
                    .arg(&f.path())
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .spawn()
                    .expect("failed to execute process");

                /*
                 * Main loop will wait on children and when they reap them the loop spawns a new
                 * input-thread
                 */
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::Fork(ForkType::NewDraft(f, output)),
                });
                context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: UIEventType::ChangeMode(UIMode::Fork),
                });
                return;
            }
            UIEventType::Input(Key::Char('i')) if self.unfocused == true => {
                self.unfocused = false;
                self.dirty = true;
                self.view = None;
            }
            UIEventType::Input(Key::Char(k @ 'J')) | UIEventType::Input(Key::Char(k @ 'K')) => {
                let folder_length = context.accounts[self.cursor_pos.0].len();
                let accounts_length = context.accounts.len();
                match k {
                    'J' if folder_length > 0 => {
                        if self.new_cursor_pos.1 < folder_length - 1 {
                            self.new_cursor_pos.1 = self.cursor_pos.1 + 1;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        } else if accounts_length > 0 && self.new_cursor_pos.0 < accounts_length - 1
                        {
                            self.new_cursor_pos.0 = self.cursor_pos.0 + 1;
                            self.new_cursor_pos.1 = 0;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        }
                    }
                    'K' => {
                        if self.cursor_pos.1 > 0 {
                            self.new_cursor_pos.1 = self.cursor_pos.1 - 1;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        } else if self.cursor_pos.0 > 0 {
                            self.new_cursor_pos.0 = self.cursor_pos.0 - 1;
                            self.new_cursor_pos.1 = 0;
                            self.dirty = true;
                            self.refresh_mailbox(context);
                        }
                    }
                    _ => {}
                }
            }
            UIEventType::Input(Key::Char(k @ 'h')) | UIEventType::Input(Key::Char(k @ 'l')) => {
                let accounts_length = context.accounts.len();
                match k {
                    'h' if accounts_length > 0 && self.new_cursor_pos.0 < accounts_length - 1 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 + 1;
                        self.new_cursor_pos.1 = 0;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    }
                    'l' if self.cursor_pos.0 > 0 => {
                        self.new_cursor_pos.0 = self.cursor_pos.0 - 1;
                        self.new_cursor_pos.1 = 0;
                        self.dirty = true;
                        self.refresh_mailbox(context);
                    }
                    _ => {}
                }
            }
            UIEventType::RefreshMailbox(_) => {
                self.dirty = true;
                self.view = None;
            }
            UIEventType::MailboxUpdate((ref idxa, ref idxf)) => {
                if *idxa == self.new_cursor_pos.1 && *idxf == self.new_cursor_pos.0 {
                    self.refresh_mailbox(context);
                    self.dirty = true;
                }
            }
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEventType::Resize => {
                self.dirty = true;
            }
            UIEventType::Action(ref action) => match action {
                Action::MailListing(MailListingAction::ToggleThreaded) => {
                    context.accounts[self.cursor_pos.0]
                        .runtime_settings
                        .threaded = !context.accounts[self.cursor_pos.0]
                        .runtime_settings
                        .threaded;
                    self.refresh_mailbox(context);
                    self.dirty = true;
                    return;
                },
                Action::ViewMailbox(idx) => {
                    eprintln!("listing got viewmailbox({})", idx);
                    self.new_cursor_pos.1 = *idx;
                    self.dirty = true;
                    self.refresh_mailbox(context);
                    return;
                }
                _ => {},
            },
            _ => {}
        }
        if let Some(ref mut v) = self.view {
            v.process_event(event, context);
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.view.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
}
