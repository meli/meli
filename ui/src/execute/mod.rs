/*! A parser module for user commands passed through the Ex mode.
*/
use nom::{digit, };
use std;
pub mod actions;
pub use actions::*;


named!(
    usize_c<usize>,
    map_res!(
        map_res!(ws!(digit), std::str::from_utf8),
        std::str::FromStr::from_str
    )
);

named!(goto<Action>,
       preceded!(tag!("b "),
       map!(call!(usize_c), |v| Action::ViewMailbox(v))
      ));

//named!(sort<&str>,
//       preceded!(tag!("sort "),
//       map_res!(call!(alpha), std::str::from_utf8))
//      );

named!(threaded<Action>,
       map!(ws!(tag!("threaded")), |_| Action::MailListing(MailListingAction::ToggleThreaded)));
named!(toggle<Action>,
       preceded!(tag!("toggle "),
       alt_complete!( threaded )));

named!(pub parse_command<Action>, 
    alt_complete!( goto | toggle)
        );
