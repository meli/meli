/*! Entities that handle Mail specific functions.
  */
use super::*;

pub mod listing;
pub use listing::*;


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
                name: a.name().to_string(),
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
    fn print_account(&self, grid: &mut CellBuffer, area: Area, a: &AccountMenuEntry) -> usize {
        if !is_valid_area!(area) {
            eprintln!("BUG: invalid area in print_account");
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);


        let highlight = self.cursor.map(|(x,_)| x == a.index).unwrap_or(false);

        let mut parents: Vec<Option<usize>> = vec!(None; a.entries.len());

        for (idx, e) in a.entries.iter().enumerate() {
            for c in e.1.children() {
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
        let mut s = String::from(format!("{}\n", a.name));
        fn pop(depth: &mut String) {
            depth.pop();
            depth.pop();
        }

        fn push(depth: &mut String, c: char) {
            depth.push(c);
        }

        fn print(root: usize, parents: &Vec<Option<usize>>, depth: &mut String, entries: &Vec<(usize, Folder)>, s: &mut String, inc: &mut usize) -> () {
            let len = s.len();
            s.insert_str(len, &format!("{} {}\n  ", *inc,  &entries[root].1.name()));
            *inc += 1;
            let children_no = entries[root].1.children().len();
            for (idx, child) in entries[root].1.children().iter().enumerate() {
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
            let color_fg = if highlight {
                if idx > 1 && self.cursor.unwrap().1 == idx - 2 {
                    Color::Byte(233)
                } else {
                    Color::Byte(15)
                }
            } else {
                Color::Default
            };

            let color_bg = if highlight {
                if idx > 1 && self.cursor.unwrap().1 == idx - 2 {
                    Color::Byte(15)
                } else {
                    Color::Byte(233)
                }
            } else {
                Color::Default
            };

            let (x, _) = write_string_to_grid(&s,
                                         grid,
                                         color_fg,
                                         color_bg,
                                         (set_y(upper_left, y), bottom_right),
                                         false);

            if highlight && idx > 1 && self.cursor.unwrap().1 == idx - 2 {
                change_colors(grid, ((x, y),(get_x(bottom_right)+1, y)), color_fg , color_bg);
            } else {
                change_colors(grid, ((x, y),set_y(bottom_right, y)), color_fg , color_bg);
            }
            idx += 1;
        }
        if idx == 0 {
            0
        } else {
            idx - 1
        }

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
            UIEventType::RefreshMailbox(c) => {
                self.cursor = Some(c);
                self.dirty = true;
            },
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            },
            UIEventType::Resize => {
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
