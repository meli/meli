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

pub mod components;
pub mod position;
pub mod cells;

extern crate termion;
extern crate ncurses;
extern crate melib;

use std::collections::VecDeque;

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
pub use self::position::*;

#[derive(Debug)]
pub enum UIEventType {
    Input(Key),
    RefreshMailbox(Mailbox),
    //Quit?
    Resize,
}


#[derive(Debug)]
pub struct UIEvent {
   pub id: u64,
   pub event_type: UIEventType,
}

pub struct State<W: Write> {
    width: usize,
    height: usize,

    grid: CellBuffer,
    pub stdout: termion::raw::RawTerminal<W>,
    entities: Vec<Entity>,

}

impl<W: Write> Drop for State<W> {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        write!(self.stdout, "{}{}{}{}", clear::All, style::Reset, cursor::Goto(1, 1), cursor::Show).unwrap();
    }
}

impl<W: Write> State<W> {
    pub fn new(stdout: W) -> Self {
        let termsize = termion::terminal_size().ok();
        let termwidth = termsize.map(|(w,_)| w);
        let termheight = termsize.map(|(_,h)| h);
        let width = termwidth.unwrap_or(0) as usize;
        let height = termheight.unwrap_or(0) as usize;
        let mut s = State {
            width: width,
            height: height,
            //queue: VecDeque::new();

            grid: CellBuffer::new(width+1, height+1, Cell::with_char(' ')),
            stdout: stdout.into_raw_mode().unwrap(),
            entities: Vec::with_capacity(2),
        };
        write!(s.stdout, "{}{}{}", cursor::Hide, clear::All, cursor::Goto(1,1)).unwrap();
        s
    }
    pub fn hello_w(&mut self) {
        write!(self.stdout, "Hey there.").unwrap();
    }
    fn update_size(&mut self) {
        /* update dimensions. TODO: Only do that in size change events. ie SIGWINCH */
        let termsize = termion::terminal_size().ok();
        let termwidth = termsize.map(|(w,_)| w);
        let termheight = termsize.map(|(_,h)| h);
        self.width = termwidth.unwrap_or(72) as usize;
        self.height = termheight.unwrap_or(120) as usize;
    }

    pub fn render(&mut self) {
        self.update_size();

        /* draw each entity */ for i in 0..self.entities.len() {
            self.draw_entity(i);
        }

        /* Only draw dirty areas */
        for y in 0..self.height {
            write!(self.stdout, "{}", cursor::Goto(1,y as u16)).unwrap();
            for x in 0..self.width {
                let c = self.grid[(x,y)];

                if c.get_bg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Bg(c.get_bg().as_termion()));
                }
                if c.get_fg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Fg(c.get_fg().as_termion()));
                }
                write!(self.stdout, "{}",c.ch()).unwrap();
                if c.get_bg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Bg(termion::color::Reset));
                }
                if c.get_fg() != cells::Color::Default {
                    write!(self.stdout, "{}", termion::color::Fg(termion::color::Reset));
                }

            }
        }
    }
    pub fn draw_entity(&mut self, idx: usize) {
        let ref mut entity = self.entities[idx];
        eprintln!("Entity is {:?}", entity);
        let upper_left =  (1,1);
        let bottom_right = (self.width, self.height);
        eprintln!("Upper left is {:?} and bottom_right is {:?}", upper_left, bottom_right);

        entity.component.draw(&mut self.grid, upper_left, bottom_right);
    }
    pub fn register_entity(&mut self, entity: Entity) {
        self.entities.push(entity);
    }

    pub fn rcv_event(&mut self, event: UIEvent) {
        /* pass a queue for replies */
            let mut queue : VecDeque<UIEvent> = VecDeque::new(); 
        /* inform each entity */ for i in 0..self.entities.len() {
            self.entities[i].rcv_event(&event, &mut queue);
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

pub fn get_events<F>(stdin: std::io::Stdin, closure: F) where F: Fn(Key) -> (){
    let stdin = stdin.lock();
    for c in stdin.keys() {
        if let Ok(k) = c {
            let k = convert_key(k);
            eprintln!("Received key: {:?}", k);
            closure(k);
        }
    }
}
