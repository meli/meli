extern crate termion;


use termion::{clear, cursor};
use termion::raw::IntoRawMode;
use termion::event::{Key as TermionKey, Event as TermionEvent, MouseEvent as TermionMouseEvent};



//use std::env;
use std::io::{Read, Write};
use termion::input::TermRead;
use std::io::{stdout, stdin, stderr};
//use std::collections::VecDeque;
//use std::process;


mod components;
mod position;
mod cells;
use cells::{Cell, CellBuffer};
use position::Pos;

pub use self::components::*;
pub use self::position::*;

#[derive(Debug)]
pub enum UIEventType {
    Input(Key),
    RefreshMailbox(String),
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
        write!(s.stdout, "{}{}", clear::All, cursor::Goto(1,1)).unwrap();
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

        for y in 1..self.height {
            write!(self.stdout, "{}", cursor::Goto(1,y as u16)).unwrap();
            for x in 1..self.width {
                let c = self.grid[(x,y)];
                if c.get_bg() == cells::Color::Default {
                    write!(self.stdout, "{}",c.ch()).unwrap();
                } else {
                    write!(self.stdout, "{}{}{}", termion::color::Bg(termion::color::LightBlack),c.ch(),termion::color::Bg(termion::color::Reset)).unwrap();
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

        entity.component.draw(upper_left, bottom_right, &mut self.grid);
    }
    pub fn register_entity(&mut self, entity: Entity) {
        self.entities.push(entity);
    }

    pub fn rcv_event(&mut self, event: UIEvent) {
        /* inform each entity */ for i in 0..self.entities.len() {
            self.entities[i].rcv_event(&event);
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
