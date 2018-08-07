/*! A parser module for user commands passed through the Ex mode.
*/
use nom::{digit, not_line_ending};
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

named!(sortfield<SortField>,
       map_res!(
           map_res!(take_until_s!(" "), std::str::from_utf8),
           std::str::FromStr::from_str));


named!(sortorder<SortOrder>,
       map_res!(
           map_res!(call!(not_line_ending), std::str::from_utf8),
           std::str::FromStr::from_str));



named!(goto<Action>,
       preceded!(tag!("b "),
       map!(call!(usize_c), |v| Action::ViewMailbox(v))
      ));

named!(subsort<Action>, do_parse!(
       tag!("subsort ") >>
       p: pair!(sortfield,sortorder) >>
       (
           Action::SubSort(p.0, p.1)

       )
      ));
named!(sort<Action>, do_parse!(
       tag!("sort ") >>
       p: separated_pair!(sortfield,tag!(" "), sortorder) >>
       (
           Action::Sort(p.0, p.1)

       )
      ));

named!(threaded<Action>,
       map!(ws!(tag!("threaded")), |_| Action::MailListing(MailListingAction::ToggleThreaded)));
named!(toggle<Action>,
       preceded!(tag!("toggle "),
       alt_complete!( threaded )));

named!(pub parse_command<Action>,
    alt_complete!( goto | toggle | sort | subsort)
        );
