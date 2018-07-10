use std::fmt;
use cells::{Color, Cell, CellBuffer, CellAccessor};
use position::Pos;

/// The upper and lower boundary char.
const HORZ_BOUNDARY: char = '─';
/// The left and right boundary char.
const VERT_BOUNDARY: char = '│';

/// The top-left corner
const TOP_LEFT_CORNER: char = '┌';
/// The top-right corner
const TOP_RIGHT_CORNER: char = '┐';
/// The bottom-left corner
const BOTTOM_LEFT_CORNER: char = '└';
/// The bottom-right corner
const BOTTOM_RIGHT_CORNER: char = '┘';

const LIGHT_VERTICAL_AND_RIGHT: char = '├';

const LIGHT_VERTICAL_AND_LEFT: char = '┤';

const LIGHT_DOWN_AND_HORIZONTAL: char = '┬';

const LIGHT_UP_AND_HORIZONTAL: char = '┴';

pub struct Entity {
    //queue: VecDeque,
    pub component: Box<Component>, // more than one?
}

impl fmt::Debug for Entity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Entity", )
    }
}

pub trait Component {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer);
    fn process_event(&mut self);
}


///A simple box with borders and no content.
pub struct BoxPanel {
}

impl Component for BoxPanel {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer) {
        grid[upper_left].set_ch('u');
        grid[bottom_right].set_ch('b');
        let width = bottom_right.0 - upper_left.0;
        let height = bottom_right.1 - upper_left.1;

        grid[upper_left].set_ch('┌');
        grid[(upper_left.0, bottom_right.1)].set_ch(BOTTOM_LEFT_CORNER);
        grid[(bottom_right.0, upper_left.1)].set_ch('┐');
        grid[bottom_right].set_ch('┘');
        for i in upper_left.1 + 1..bottom_right.1 {
            grid[(upper_left.0, i)].set_ch('│');
            grid[(upper_left.0 + width, i)].set_ch('│');
        }
        for i in upper_left.0+1..bottom_right.0 {
            grid[(i, upper_left.1)].set_ch('─');
            grid[(i, upper_left.1 + height)].set_ch('─');
        }

        let width = bottom_right.0 - upper_left.0;
        let height = bottom_right.1 - upper_left.1;
    }
    fn process_event(&mut self) {
        unimplemented!();
    }
}

/// A horizontally split in half container.
pub struct HSplit {
    top: Entity,
    bottom: Entity,
    ratio: usize, // bottom/whole height * 100
}

impl HSplit {
    pub fn new(top: Entity, bottom: Entity, ratio: usize) -> Self {
        HSplit {
            top: top,
            bottom: bottom,
            ratio: ratio,
        }
    }
}


impl Component for HSplit {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer) {
        //eprintln!("grid {:?}", grid);
        grid[upper_left].set_ch('u');
        let (a,b) = upper_left;
        grid[(a+1,b)].set_ch('h');
        let width = bottom_right.0 - upper_left.0;
        let height = bottom_right.1 - upper_left.1;

        let total_rows = bottom_right.1 - upper_left.1;
        let bottom_entity_height = (self.ratio*total_rows )/100;
        let mid = upper_left.1 + total_rows - bottom_entity_height;

        for i in upper_left.0..bottom_right.0+1 {
            grid[(i, mid)].set_ch('─');
        }
        let _ = self.top.component.draw(upper_left, (bottom_right.0, upper_left.1 + mid-1), grid);
        let _ = self.bottom.component.draw((upper_left.0, upper_left.1 + mid), bottom_right, grid);
        grid[bottom_right].set_ch('b');
    }
    fn process_event(&mut self) {
        unimplemented!();
    }
}

/// A horizontally split in half container.
pub struct VSplit {
    left: Entity,
    right: Entity,
    ratio: usize, // right/(container width) * 100
}

impl VSplit {
    pub fn new(left: Entity, right: Entity, ratio: usize) -> Self {
        VSplit {
            left: left,
            right: right,
            ratio: ratio,
        }
    }
}


impl Component for VSplit {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer) {

        eprintln!("Upper left is {:?} and bottom_right is {:?}", upper_left, bottom_right);
        let width = bottom_right.0 - upper_left.0;
        let height = bottom_right.1 - upper_left.1;

        let total_cols = bottom_right.0 - upper_left.0;
        let right_entity_width = (self.ratio*total_cols )/100;
        let mid = bottom_right.0 - right_entity_width;
        eprintln!("total_cols {:?}, right_entity_width: {:?}, mid: {:?}",total_cols, right_entity_width, mid);
        if (upper_left.1> 1) {
            let c = grid.get(mid, upper_left.1-1).map(|a| a.ch()).unwrap_or_else(|| ' ');
            match c {
                HORZ_BOUNDARY => {
                    grid[(mid, upper_left.1-1)].set_ch(LIGHT_DOWN_AND_HORIZONTAL);
                },
                _ => {},
            }
        }

        for i in upper_left.1..bottom_right.1 {
            grid[(mid, i)].set_ch(VERT_BOUNDARY);
        }
        if (bottom_right.1> 1) {
            let c = grid.get(mid, bottom_right.1-1).map(|a| a.ch()).unwrap_or_else(|| ' ');
            match c {
                HORZ_BOUNDARY => {
                    grid[(mid, bottom_right.1+1)].set_ch(LIGHT_UP_AND_HORIZONTAL);
                },
                _ => {},
            }
        }
        let _ = self.left.component.draw(upper_left, (mid-1, bottom_right.1), grid);
        let _ = self.right.component.draw((mid+1, upper_left.1), bottom_right, grid);
    }
    fn process_event(&mut self) {
        unimplemented!();
    }
}

/// A box with a text content.
pub struct TextBox {
    content: String,
}

impl TextBox {
    pub fn new(s: String) -> Self {
        TextBox {
            content: s,
        }
    }
}

impl Component for TextBox {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer) {
        let mut x = upper_left.0;
        let mut y = upper_left.1;
        for c in self.content.chars() {
            grid[(x,y)].set_ch(c);
            //eprintln!("printed {} in ({}, {})", c, x, y);
            x += 1;
            if x == bottom_right.0 + 1 {
                x = upper_left.0;
            }

            if y == bottom_right.1 {
                break;
            }
        }
        //eprintln!("Upper left is {:?} and bottom_right is {:?}", upper_left, bottom_right);
        let width = bottom_right.0 - upper_left.0;
        let height = bottom_right.1 - upper_left.1;
    }
    fn process_event(&mut self) {
        unimplemented!();
    }
}

const max_width: usize = 500;

pub struct MailListing {
    cursor_pos: usize,
    length: usize,
    // sorting
    content: CellBuffer,
    unfocused: bool,
    // content (2-d vec of bytes) or Cells?
    // current view on top of content
    // active or not? for key events

}

impl MailListing {
    pub fn new(length: usize) -> Self {
        MailListing {
            cursor_pos: 0,
            length: length,
            content: CellBuffer::new(max_width, length+1, Cell::with_char(' ')),
            unfocused: false,
        }
    }
}

impl Component for MailListing {
    fn draw(&mut self, upper_left: Pos, bottom_right: Pos, grid:  &mut CellBuffer) {
        let mut height = 0;
        let mut stripe = false;
        for y in upper_left.1..bottom_right.1 {
            if height == self.length {
                /* for loop */
                for _y in y..bottom_right.1 {
                    for x in upper_left.0..bottom_right.0 {
                        grid[(x,y)].set_ch(' ');
                    }
                }
                break;
            }
            for x in upper_left.0..bottom_right.0 {
                //grid[(x,y)].set_ch(self.content[(x-upper_left.0+1, y-upper_left.1+1)].ch());
                grid[(x,y)].set_ch(if stripe { 't' } else { 'f'} );
                grid[(x,y)].set_bg(if stripe { Color::Byte(246) } else {Color::Default });
            }
            stripe = if stripe { false } else { true };
            height +1 ;
        }
    }
    fn process_event(&mut self) {
        unimplemented!();
    }
}
