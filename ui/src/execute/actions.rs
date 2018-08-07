/*!
 * User actions that need to be handled by the UI
 */

use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum MailListingAction {
    ToggleThreaded,
}

#[derive(Debug, Clone)]
pub enum SortOrder {
    Asc,
    Desc,
}

#[derive(Debug, Clone)]
pub enum SortField {
    Subject,
    Date,
}


impl FromStr for SortField {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        eprintln!("sortfield from_str {}", s);
        match s.trim() {
            "subject" | "s" | "sub" | "sbj" | "subj" => {
                eprintln!("parsed: subject");
            }
            "date" | "d"  => {
                eprintln!("parsed date");
            }
            _ => {
                eprintln!("error in parse");
            }
        }
        match s.trim() {
            "subject" | "s" | "sub" | "sbj" | "subj" => Ok(SortField::Subject),
            "date" | "d"  => Ok(SortField::Date),
            _ => Err(())
        }
    }
}

impl FromStr for SortOrder {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        eprintln!("sortoder from_str {}", s);
        match s.trim() {
            "asc" => Ok(SortOrder::Asc),
            "desc" => Ok(SortOrder::Desc),
            _ => Err(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum Action {
    MailListing(MailListingAction),
    ViewMailbox(usize),
    Sort(SortField, SortOrder),
    SubSort(SortField, SortOrder),
}
