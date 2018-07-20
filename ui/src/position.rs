/*!
  Simple type definitions and macro helper for a (x,y) position on the terminal and the areas they define.

  An `Area` consists of two points: the upper left and bottom right corners.
  */

/// A `(x, y)` position on screen.
pub type Pos = (usize, usize);

#[inline(always)]
pub fn get_x(p: Pos) -> usize {
    p.0
}
#[inline(always)]
pub fn get_y(p: Pos) -> usize {
    p.1
}
#[inline(always)]
pub fn set_x(p: Pos, new_x: usize) -> Pos {
    (new_x, p.1)
}
#[inline(always)]
pub fn set_y(p: Pos, new_y: usize) -> Pos {
    (p.0, new_y)
}

/// An `Area` consists of two points: the upper left and bottom right corners.
pub type Area = (Pos, Pos);

#[macro_export]
macro_rules! upper_left { ($a:expr) => ( $a.0 ) }
#[macro_export]
macro_rules! bottom_right { ($a:expr) => ( $a.1 ) }
#[macro_export]
macro_rules! is_valid_area { ($a:expr) =>
    {
        {
            let upper_left = upper_left!($a);
            let bottom_right = bottom_right!($a);
            if get_y(upper_left) > get_y(bottom_right) || get_x(upper_left) > get_x(bottom_right) {
                false
            } else {
                true
            }
        }
    }
}

/// A `(cols, rows)` size.
pub type Size = (usize, usize);

pub trait HasSize {
    fn size(&self) -> Size;
}

pub trait HasPosition {
    fn origin(&self) -> Pos;
    fn set_origin(&mut self, new_origin: Pos);
}
