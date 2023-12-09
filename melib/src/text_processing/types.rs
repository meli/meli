/*
 * meli - text_processing crate.
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

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LineBreakClass {
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

use LineBreakClass::*;

impl From<&str> for LineBreakClass {
    fn from(val: &str) -> Self {
        match val {
            stringify!(BK) => BK,
            stringify!(CM) => CM,
            stringify!(CR) => CR,
            stringify!(GL) => GL,
            stringify!(LF) => LF,
            stringify!(NL) => NL,
            stringify!(SP) => SP,
            stringify!(WJ) => WJ,
            stringify!(ZW) => ZW,
            stringify!(ZWJ) => ZWJ,
            stringify!(AI) => AI,
            stringify!(AL) => AL,
            stringify!(B2) => B2,
            stringify!(BA) => BA,
            stringify!(BB) => BB,
            stringify!(CB) => CB,
            stringify!(CJ) => CJ,
            stringify!(CL) => CL,
            stringify!(CP) => CP,
            stringify!(EB) => EB,

            stringify!(EM) => EM,
            stringify!(EX) => EX,
            stringify!(H2) => H2,
            stringify!(H3) => H3,
            stringify!(HL) => HL,
            stringify!(HY) => HY,
            stringify!(ID) => ID,
            stringify!(IN) => IN,
            stringify!(IS) => IS,
            stringify!(JL) => JL,

            stringify!(JT) => JT,
            stringify!(JV) => JV,
            stringify!(NS) => NS,
            stringify!(NU) => NU,
            stringify!(OP) => OP,
            stringify!(PO) => PO,
            stringify!(PR) => PR,
            stringify!(QU) => QU,
            stringify!(RI) => RI,
            stringify!(SA) => SA,

            stringify!(SG) => SG,
            stringify!(SY) => SY,
            stringify!(XX) => XX,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Reflow {
    No,
    All,
    #[default]
    FormatFlowed,
}
