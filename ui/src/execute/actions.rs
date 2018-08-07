/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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
            "date" | "d" => {
                eprintln!("parsed date");
            }
            _ => {
                eprintln!("error in parse");
            }
        }
        match s.trim() {
            "subject" | "s" | "sub" | "sbj" | "subj" => Ok(SortField::Subject),
            "date" | "d" => Ok(SortField::Date),
            _ => Err(()),
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
            _ => Err(()),
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
