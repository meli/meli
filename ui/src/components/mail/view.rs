use super::*;


pub struct MailView {
    coordinates: (usize, usize, usize),
    pager: Option<Pager>,
    subview: Option<Box<MailView>>,
    dirty: bool,
}

impl MailView {
    pub fn new(coordinates: (usize, usize, usize), pager: Option<Pager>, subview: Option<Box<MailView>>) -> Self {

        MailView {
            coordinates: coordinates,
            pager: pager,
            subview: subview,
            dirty: true,
        }
    }
}


impl Component for MailView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let (envelope_idx, y): (usize, usize) = {
            let threaded = context.accounts[self.coordinates.0].runtime_settings.threaded;
            let mailbox = &mut context.accounts[self.coordinates.0][self.coordinates.1].as_ref().unwrap().as_ref().unwrap();
            let envelope_idx: usize = if threaded {
                mailbox.threaded_mail(self.coordinates.2)
            } else {
                self.coordinates.2
            };

            let envelope: &Envelope = &mailbox.collection[envelope_idx];

            let (x,y) = write_string_to_grid(&format!("Date: {}", envelope.date_as_str()),
                                            grid,
                                            Color::Byte(33),
                                            Color::Default,
                                            area,
                                            true);
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            let (x,y) = write_string_to_grid(&format!("From: {}", envelope.from()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y+1), bottom_right),
            true);
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            let (x,y) = write_string_to_grid(&format!("To: {}", envelope.to()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y+1), bottom_right),
            true);
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            let (x,y) = write_string_to_grid(&format!("Subject: {}", envelope.subject()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y+1), bottom_right),
            true);
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            let (x, y) = write_string_to_grid(&format!("Message-ID: {}", envelope.message_id_raw()),
            grid,
            Color::Byte(33),
            Color::Default,
            (set_y(upper_left, y+1), bottom_right),
            true);
            for x in x..=get_x(bottom_right) {
                grid[(x, y)].set_ch(' ');
                grid[(x, y)].set_bg(Color::Default);
                grid[(x, y)].set_fg(Color::Default);
            }
            clear_area(grid,
                       (set_y(upper_left, y+1), set_y(bottom_right, y+2)));
            context.dirty_areas.push_back((upper_left, set_y(bottom_right, y+1)));
            (envelope_idx, y + 1)
        };
        if self.dirty {
            self.pager = Some(Pager::from_envelope((self.coordinates.0, self.coordinates.1), envelope_idx, context));
            self.dirty = false;
        }
        self.pager.as_mut().map(|p| p.draw(grid, (set_y(upper_left, y + 1),bottom_right), context));
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        if let Some(ref mut sub) = self.subview {
            sub.process_event(event, context);
            
        } else {
            if let Some(ref mut p) = self.pager {
                p.process_event(event, context);
            }
        }
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.pager.as_ref().map(|p| p.is_dirty()).unwrap_or(false) ||
        self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
}
