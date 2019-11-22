pub mod grapheme_clusters;
pub mod line_break;
mod tables;
mod types;
pub use types::Reflow;
pub mod wcwidth;
pub use grapheme_clusters::*;
pub use line_break::*;
pub use wcwidth::*;

pub trait Truncate {
    fn truncate_at_boundary(self, new_len: usize);
}

impl Truncate for &mut String {
    fn truncate_at_boundary(self, mut new_len: usize) {
        while new_len > 0 && !self.is_char_boundary(new_len) {
            new_len -= 1;
        }
        String::truncate(self, new_len);
    }
}
