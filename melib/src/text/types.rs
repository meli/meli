/*
 * meli - text crate.
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

macro_rules! define_line_break_class {
    ($($classname:ident),*$(,)?)=> {
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        pub enum LineBreakClass {
            $($classname,)*
        }

        impl From<&str> for LineBreakClass {
            fn from(val: &str) -> Self {
                match val {
                    $(stringify!($classname) => Self::$classname),*,
                    other => panic!("{other}"),
                }
            }
        }
    };
}

define_line_break_class! {
    BK,
    CM,
    CR,
    GL,
    LF,
    NL,
    SP,
    WJ,
    ZW,
    ZWJ,
    AI,
    AL,
    B2,
    BA,
    BB,
    CB,
    CJ,
    CL,
    CP,
    EB,
    EM,
    EX,
    H2,
    H3,
    HL,
    HY,
    ID,
    IN,
    IS,
    JL,
    JT,
    JV,
    NS,
    NU,
    OP,
    PO,
    PR,
    QU,
    RI,
    SA,
    SG,
    SY,
    XX,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Reflow {
    No,
    All,
    #[default]
    FormatFlowed,
}
