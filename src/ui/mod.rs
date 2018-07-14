/*
 * meli - ui module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

#[macro_use]
pub mod position;
pub mod components;
pub mod cells;

extern crate termion;
extern crate ncurses;
extern crate melib;

use std::collections::VecDeque;
use std::fmt;

pub use self::position::*;

/* Color pairs; foreground && background. */
/// Default color.
pub static COLOR_PAIR_DEFAULT: i16 = 1;
/// Highlighted cursor line in index view.
pub static COLOR_PAIR_CURSOR: i16 = 2;
/// Header colour in pager view.
pub static COLOR_PAIR_HEADERS: i16 = 3;
/// Indentation symbol color in index view.
pub static COLOR_PAIR_THREAD_INDENT: i16 = 4;
/// Line color for odd entries in index view.
pub static COLOR_PAIR_THREAD_ODD: i16 = 5;
/// Line color for even entries in index view.
pub static COLOR_PAIR_THREAD_EVEN: i16 = 6;
/// Line color for unread odd entries in index view.
pub static COLOR_PAIR_UNREAD_ODD: i16 = 7;
/// Line color for unread even entries in index view.
pub static COLOR_PAIR_UNREAD_EVEN: i16 = 8;

/// `ThreadEvent` encapsulates all of the possible values we need to transfer between our threads
/// to the main process.
pub enum ThreadEvent {
  /// User input.
  Input(Key),
  /// A watched folder has been refreshed.
  RefreshMailbox{ name: String },
  UIEventType(UIEventType),
  //Decode { _ }, // For gpg2 signature check
}

impl From<RefreshEvent> for ThreadEvent {
    fn from(event: RefreshEvent) -> Self {
        ThreadEvent::RefreshMailbox { name: event.folder }
    }
}








use melib::*;


use std;
use termion::{clear, style, cursor};
use termion::raw::IntoRawMode;
use termion::event::{Key as TermionKey, };



use std::io::{Write, };
use termion::input::TermRead;

use self::cells::*;
pub use self::components::*;

#[derive(Debug)]
pub enum UIEventType {
    Input(Key),
    ExInput(Key),
    RefreshMailbox(Mailbox),
    //Quit?
    Resize,
    ChangeMailbox(usize),
    ChangeMode(UIMode),
}


#[derive(Debug)]
pub struct UIEvent {
   pub id: u64,
   pub event_type: UIEventType,
}

#[derive(Debug, Clone, Copy)]
pub enum UIMode {
    Normal,
    Execute,
}

impl fmt::Display for UIMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            UIMode::Normal => { "NORMAL" },
            UIMode::Execute => { "EX" },
        })
    }
}

pub struct Context {
    pub accounts: Vec<Account>,
    settings: Settings,
    /// Areas of the screen that must be redrawn in the next render
    dirty_areas: VecDeque<Area>,
    backends: Backends,
}

pub struct State<W: Write> {
    cols: usize,
    rows: usize,

    grid: CellBuffer,
    stdout: termion::raw::RawTerminal<W>,
    entities: Vec<Entity>,
    pub context: Context,
}

impl<W: Write> Drop for State<W> {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        write!(self.stdout, "{}{}{}{}", clear::All, style::Reset, cursor::Goto(1, 1), cursor::Show).unwrap();
        self.stdout.flush().unwrap();
    }
}

impl<W: Write> State<W> {
    pub fn new(stdout: W) -> Self {
        let settings = Settings::new();
        let backends = Backends::new();

        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w,_)| w);
        let termrows = termsize.map(|(_,h)| h);
        let cols = termcols.unwrap_or(0) as usize;
        let rows = termrows.unwrap_or(0) as usize;
        let mut s = State {
            cols: cols,
            rows: rows,
            grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            stdout: stdout.into_raw_mode().unwrap(),
            entities: Vec::with_capacity(1),

            context: Context {
                accounts: settings.accounts.iter().map(|(n, a_s)| { Account::new(n.to_string(), a_s.clone(), &backends) }).collect(),
                backends: backends,
                settings: settings,
                dirty_areas: VecDeque::with_capacity(5),
            },
        };
        write!(s.stdout, "{}{}{}", cursor::Hide, clear::All, cursor::Goto(1,1)).unwrap();
        s.stdout.flush().unwrap();
        s
    }
    pub fn update_size(&mut self) {
        /* update dimensions. TODO: Only do that in size change events. ie SIGWINCH */
        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w,_)| w);
        let termrows = termsize.map(|(_,h)| h);
        if termcols.unwrap_or(72) as usize  != self.cols || termrows.unwrap_or(120) as usize != self.rows {
            eprintln!("Size updated, from ({}, {}) -> ({:?}, {:?})", self.cols, self.rows, termcols, termrows);

        }
        self.cols = termcols.unwrap_or(72) as usize;
        self.rows = termrows.unwrap_or(120) as usize;
        self.grid.resize(self.cols, self.rows, Cell::with_char(' '));
    }

    pub fn redraw(&mut self) {
        for i in 0..self.entities.len() {
            self.draw_entity(i);
        }
        let areas: Vec<Area> = self.context.dirty_areas.drain(0..).collect();
        eprintln!("redrawing {} areas", areas.len());
        /* draw each dirty area */
        for a in areas {
            self.draw_area(a);
        }
    }
    fn draw_area(&mut self, area: Area) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        for y in get_y(upper_left)..=get_y(bottom_right) {
            write!(self.stdout, "{}", cursor::Goto(get_x(upper_left) as u16 + 1,(y+1) as u16)).unwrap();
            for x in get_x(upper_left)..=get_x(bottom_right) {
                let c = self.grid[(x,y)];

                if c.get_bg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Bg(c.get_bg().as_termion())).unwrap();
                }
                if c.get_fg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Fg(c.get_fg().as_termion())).unwrap();
                }
                write!(self.stdout, "{}",c.ch()).unwrap();
                if c.get_bg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Bg(termion::color::Reset)).unwrap();
                }
                if c.get_fg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Fg(termion::color::Reset)).unwrap();
                }

            }
        }
        self.stdout.flush().unwrap();
    }
    pub fn render(&mut self) {
        self.update_size();

        /* draw each entity */
        for i in 0..self.entities.len() {
            self.draw_entity(i);
        }
        let cols = self.cols;
        let rows = self.rows;

        self.draw_area(((0, 0), (cols-1, rows-1)));
    }
    pub fn draw_entity(&mut self, idx: usize) {
        let entity = &mut self.entities[idx];
        let upper_left =  (0,0);
        let bottom_right = (self.cols-1, self.rows-1);

        if entity.component.is_dirty() {
            entity.component.draw(&mut self.grid,
                                  (upper_left, bottom_right),
                                  &mut self.context);
        }
    }
    pub fn register_entity(&mut self, entity: Entity) {
        self.entities.push(entity);
    }

    pub fn rcv_event(&mut self, event: UIEvent) {
        /* pass Context */
        /* inform each entity */ for i in 0..self.entities.len() {
            self.entities[i].rcv_event(&event, &mut self.context);
        }
    }
    /// Tries to load a mailbox's content
    pub fn refresh_mailbox(&mut self, account_idx: usize, folder_idx: usize) {
        let mailbox = match &mut self.context.accounts[account_idx][folder_idx] {
                Some(Ok(v)) => { Some(v.clone()) },
                Some(Err(e)) => { eprintln!("error {:?}", e); None },
                None => {  eprintln!("None"); None },
        };
        if let Some(m) = mailbox {
            self.rcv_event(UIEvent { id: 0, event_type: UIEventType::RefreshMailbox(m) });
        }

    }
}

pub fn convert_key(k: TermionKey ) -> Key {
    match k {
        TermionKey::Backspace => Key::Backspace,
        TermionKey::Left => Key::Left,
        TermionKey::Right => Key::Right,
        TermionKey::Up => Key::Up,
        TermionKey::Down => Key::Down,
        TermionKey::Home => Key::Home,
        TermionKey::End => Key::End,
        TermionKey::PageUp => Key::PageUp,
        TermionKey::PageDown => Key::PageDown,
        TermionKey::Delete => Key::Delete,
        TermionKey::Insert => Key::Insert,
        TermionKey::F(u) => Key::F(u),
        TermionKey::Char(c) => Key::Char(c),
        TermionKey::Alt(c) => Key::Alt(c),
        TermionKey::Ctrl(c) => Key::Ctrl(c),
        TermionKey::Null => Key::Null,
        TermionKey::Esc => Key::Esc,
        _ => Key::Char(' '),
    }
}

#[derive(Debug)]
pub enum Key {
    /// Backspace.
    Backspace,
    /// Left arrow.
    Left,
    /// Right arrow.
    Right,
    /// Up arrow.
    Up,
    /// Down arrow.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page Up key.
    PageUp,
    /// Page Down key.
    PageDown,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// Function keys.
    ///
    /// Only function keys 1 through 12 are supported.
    F(u8),
    /// Normal character.
    Char(char),
    /// Alt modified character.
    Alt(char),
    /// Ctrl modified character.
    ///
    /// Note that certain keys may not be modifiable with `ctrl`, due to limitations of terminals.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
}

pub fn get_events<F>(stdin: std::io::Stdin, mut closure: F) where F: FnMut(Key) -> (){
    let stdin = stdin.lock();
    for c in stdin.keys() {
        if let Ok(k) = c {
            let k = convert_key(k);
            closure(k);
        }
    }
}
