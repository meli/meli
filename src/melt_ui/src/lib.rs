extern crate termion;


use termion::{clear, cursor};



//use std::env;
use std::io::{Read, Write};
//use std::collections::VecDeque;
//use std::process;


mod components;
mod position;
mod cells;
use cells::{Cell, CellBuffer};
use position::Pos;

pub use self::components::*;
pub use self::position::*;


pub struct UIEvent {
}

pub struct State<R, W: Write> {
    width: usize,
    height: usize,

    grid: CellBuffer,
    stdin: R,
    pub stdout: W,
    entities: Vec<Entity>,

}

impl<R: Read, W: Write> State<R,W> {
    pub fn new(stdout: W, stdin: R) -> Self {
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
            stdin: stdin,
            stdout: stdout,
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
}
