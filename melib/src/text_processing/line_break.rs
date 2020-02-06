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

extern crate unicode_segmentation;
use self::unicode_segmentation::UnicodeSegmentation;
use super::grapheme_clusters::TextProcessing;
use super::tables::LINE_BREAK_RULES;
use super::types::LineBreakClass;
use super::types::Reflow;
use core::cmp::Ordering;
use core::iter::Peekable;
use core::str::FromStr;
use LineBreakClass::*;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum LineBreakCandidate {
    MandatoryBreak,
    BreakAllowed,
    NoBreak, // Not used.
}

impl Default for LineBreakCandidate {
    fn default() -> Self {
        LineBreakCandidate::NoBreak
    }
}

use LineBreakCandidate::*;

pub struct LineBreakCandidateIter<'a> {
    text: &'a str,
    iter: Peekable<unicode_segmentation::GraphemeIndices<'a>>,
    pos: usize,
    /* Needed for rule LB30a */
    reg_ind_streak: u32,
}

impl<'a> LineBreakCandidateIter<'a> {
    pub fn new(text: &'a str) -> Self {
        LineBreakCandidateIter {
            text,
            pos: 0,
            iter: UnicodeSegmentation::grapheme_indices(text, true).peekable(),
            reg_ind_streak: 0,
        }
    }
}

macro_rules! get_base_character {
    ($grapheme:ident) => {{
        char::from_str($grapheme.get(0..1).unwrap_or_else(|| {
            $grapheme.get(0..2).unwrap_or_else(|| {
                $grapheme
                    .get(0..3)
                    .unwrap_or_else(|| $grapheme.get(0..4).unwrap())
            })
        }))
    }};
    ($grapheme:expr) => {{
        char::from_str($grapheme.get(0..1).unwrap_or_else(|| {
            $grapheme.get(0..2).unwrap_or_else(|| {
                $grapheme
                    .get(0..3)
                    .unwrap_or_else(|| $grapheme.get(0..4).unwrap())
            })
        }))
    }};
}

/// Side effects: none
macro_rules! get_class {
    ($grapheme:ident) => {{
        get_base_character!($grapheme)
            .map(|char| search_table(char as u32, LINE_BREAK_RULES))
            .unwrap_or(XX)
    }};
    ($grapheme:expr) => {{
        get_base_character!($grapheme)
            .map(|char| search_table(char as u32, LINE_BREAK_RULES))
            .unwrap_or(XX)
    }};
}

/// Side effects: Updates $graph_iter and potentially $idx and $grapheme
macro_rules! next_grapheme_class {
    ($graph_iter:ident, $grapheme:ident) => ({
        if let Some((_, g)) = $graph_iter.next() {
            $grapheme = g;
            Some(get_class!(g))
        } else { None }
    });
    (($next_char:ident is $class:expr)) => ({
        $next_char.is_some() && get_class!(($next_char.unwrap().1)) == $class
    });
    (($next_char:ident is $($class:ident),+)) => ({
        $next_char.is_some() && ($(get_class!(($next_char.unwrap().1)) == $class)||+)
    });
}

/// Returns positions where breaks can happen
/// Examples:
/// ```
/// use melib::text_processing::{self, LineBreakCandidate::{self, *}};
/// use melib::text_processing::line_break::LineBreakCandidateIter;
///
/// assert!(LineBreakCandidateIter::new("").collect::<Vec<(usize, LineBreakCandidate)>>().is_empty());
/// assert_eq!(&[(7, BreakAllowed), (12, MandatoryBreak)],
///            LineBreakCandidateIter::new("Sample Text.").collect::<Vec<(usize, LineBreakCandidate)>>().as_slice());
/// assert_eq!(&[(3, MandatoryBreak), (7, MandatoryBreak), (10, BreakAllowed), (17, MandatoryBreak)],
///            LineBreakCandidateIter::new("Sa\nmp\r\nle T(e)xt.").collect::<Vec<(usize, LineBreakCandidate)>>().as_slice());
/// ```
impl<'a> Iterator for LineBreakCandidateIter<'a> {
    type Item = (usize, LineBreakCandidate);
    fn next(&mut self) -> Option<Self::Item> {
        // After end of text, there are no breaks.
        if self.pos >= self.text.len() {
            return None;
        }
        // LB3 Always break at the end of text
        if self.pos + 1 == self.text.len() {
            self.pos += 1;
            return Some((self.pos, MandatoryBreak));
        }

        let (idx, mut grapheme) = self.iter.next().unwrap();
        let LineBreakCandidateIter {
            ref mut iter,
            ref text,
            ref mut reg_ind_streak,
            ref mut pos,
        } = self;
        let iter = iter.by_ref();

        debug_assert_eq!(idx, *pos);

        // LB2 Never break at the start of text
        if idx == 0 {
            *pos += grapheme.len();
            return self.next();
        }

        let class = get_class!(grapheme);

        if class != RI {
            *reg_ind_streak = 0;
        }

        /* LB1 Assign a line breaking class to each code point of the input. Resolve AI, CB, CJ,
         * SA, SG, and XX into other line breaking classes depending on criteria outside the scope
         * of this algorithm.
         *
         * In the absence of such criteria all characters with a specific combination of original
         * class and General_Category property value are resolved as follows:
         * Resolved Original     General_Category
         * AL       AI, SG, XX   Any
         * CM       SA           Only Mn or Mc
         * AL       SA           Any except Mn and Mc
         * NS       SJ           Any
         */

        // TODO: LB1

        /* Check if next character class allows breaks before it */
        let next_char: Option<&(usize, &str)> = iter.peek();

        match class {
            BK => {
                // LB4 Always Break after hard line breaks.
                *pos += grapheme.len();
                return Some((*pos, MandatoryBreak));
            }
            // LB5 Treat CR followed by LF, as well as CR, LF, and NL as hard line breaks
            CR if next_grapheme_class!((next_char is LF)) => {
                *pos += grapheme.len();
                assert!(Some(LF) == next_grapheme_class!(iter, grapheme));
                *pos += grapheme.len();
                return Some((*pos, MandatoryBreak));
            }
            CR | LF | NL => {
                *pos += grapheme.len();
                return Some((*pos, MandatoryBreak));
            }
            _ => {}
        }
        if let Some((_, next_grapheme)) = next_char {
            let next_class = get_class!(next_grapheme);
            match next_class {
                /* LB6 Do not break before hard line breaks.  × ( BK | CR | LF | NL ) */
                BK | CR | LF | NL => {
                    *pos += grapheme.len();
                    return self.next();
                }
                /* LB7 Do not break before spaces or zero width
                 * space. × SP × ZW */
                SP | ZW => {
                    *pos += grapheme.len();
                    return self.next();
                }
                _ => {}
            }
        }
        match class {
            ZW => {
                // LB8 Break before any character following a zero-width space, even if one or more
                // spaces intervene
                // ZW SP* ÷
                *pos += grapheme.len();
                while Some(SP) == next_grapheme_class!(iter, grapheme) {
                    *pos += grapheme.len();
                }
                return Some((*pos, MandatoryBreak));
            }
            ZWJ => {
                // LB8a Do not break after a zero width joiner.
                *pos += grapheme.len();
                return self.next();
            }

            CM => {
                // LB9 Do not break a combining character sequence; treat it as if it has the line
                // breaking class of the base character in all of the following rules. Treat ZWJ as
                // if it were CM.
                // Treat X (CM | ZWJ)* as if it were X.
                // where X is any line break class except BK, CR, LF, NL, SP, or ZW.

                /* Unreachable since we break lines based on graphemes, not characters */
                unreachable!();
            }
            WJ => {
                /*: LB11 Do not break before or after Word joiner and related characters.*/
                *pos += grapheme.len();
                /* Get next grapheme */
                if next_grapheme_class!(iter, grapheme).is_some() {
                    *pos += grapheme.len();
                }
                return self.next();
            }
            GL => {
                /*LB12 Non-breaking characters: LB12 Do not break after NBSP and related characters.*/
                *pos += grapheme.len();
                return self.next();
            }
            _ => {}
        }
        if let Some((next_idx, next_grapheme)) = next_char {
            let next_class = get_class!(next_grapheme);
            match next_class {
                GL if ![SP, BA, HY].contains(&class) => {
                    /* LB12a Do not break before NBSP and related characters, except after spaces and
                     * hyphens.  [^SP BA HY] × GL
                     * Also LB12 Do not break after NBSP and related characters */
                    *pos += grapheme.len();
                    return self.next();
                }
                /* LB13 Do not break before ‘]’ or ‘!’ or ‘;’ or ‘/’, even after spaces. */
                CL | CP | EX | IS | SY => {
                    *pos = *next_idx;
                    return self.next();
                }
                _ => {}
            }
        }

        match class {
            /* LB13 Do not break before ‘]’ or ‘!’ or ‘;’ or ‘/’, even after spaces. */
            SP if [CL, CP, EX, IS, SY].contains(&get_class!(text[idx..].trim_start())) => {
                *pos += grapheme.len();
                while ![CL, CP, EX, IS, SY].contains(&next_grapheme_class!(iter, grapheme).unwrap())
                {
                    *pos += grapheme.len();
                }
                *pos += grapheme.len();
                return self.next();
            }
            OP => {
                /* LB14 Do not break after ‘[’, even after spaces.
                 * OP SP* ×
                 */
                while let Some((idx, grapheme)) = self.iter.next() {
                    *pos = idx + grapheme.len();
                    if !(get_class!(grapheme) == SP) {
                        break;
                    }
                }
                return self.next();
            }
            QU if get_class!(text[idx..].trim_start()) == OP => {
                /* LB15 Do not break within ‘”[’, even with intervening spaces.
                 * QU SP* × OP */
                *pos += grapheme.len();
                while Some(SP) == next_grapheme_class!(iter, grapheme) {
                    *pos += grapheme.len();
                }
                *pos = idx;
                return self.next();
            }
            QU => {
                /* LB19 Do not break before or after quotation marks, such as ‘ ” ’. */
                *pos += grapheme.len();
                if let Some((_, g)) = self.iter.next() {
                    *pos += g.len();
                }
                return self.next();
            }
            LineBreakClass::CL | LineBreakClass::CP
                if get_class!(text[idx..].trim_start()) == NS =>
            {
                /* LB16 Do not break between closing punctuation and a nonstarter (lb=NS), even with
                 * intervening spaces.
                 * (CL | CP) SP* × NS */
                *pos += grapheme.len();
                while Some(SP) == next_grapheme_class!(iter, grapheme) {
                    *pos += grapheme.len();
                }
                return self.next();
            }
            B2 if get_class!(text[idx..].trim_start()) == B2 => {
                *pos += grapheme.len();
                while Some(SP) == next_grapheme_class!(iter, grapheme) {
                    *pos += grapheme.len();
                }
                return self.next();
            }
            SP => {
                /* LB18 Break after spaces.  SP ÷ */
                // Space 0x20 is 1 byte long.
                *pos += 1;
                return Some((*pos, BreakAllowed));
            }
            _ => {}
        }
        if let Some((next_idx, next_grapheme)) = next_char {
            let next_class = get_class!(next_grapheme);
            match next_class {
                QU if class != SP => {
                    /* LB19 Do not break before or after quotation marks, such as ‘ ” ’. */
                    *pos = *next_idx + next_grapheme.len();
                    self.iter.next();
                    return self.next();
                }
                _ => {}
            }
        }
        match class {
            CB => {
                /* LB20 Break before and after unresolved CB. */
                *pos += grapheme.len();
                return Some((*pos - 1, BreakAllowed));
            }
            /* LB21 Do not break before hyphen-minus, other hyphens, fixed-width spaces, small
             * kana, and other non-starters, or after acute accents.  × BA,  × HY, × NS,  BB × */
            BB => {
                *pos += grapheme.len();
                return self.next();
            }
            _ => {}
        }

        if let Some((_, next_grapheme)) = next_char {
            let next_class = get_class!(next_grapheme);
            match next_class {
                BA | HY | NS => {
                    /* LB21 Do not break before hyphen-minus, other hyphens, fixed-width spaces, small
                     * kana, and other non-starters, or after acute accents.  × BA,  × HY, × NS,  BB × */
                    *pos += grapheme.len();
                    return self.next();
                }
                _ => {}
            }
        }
        match class {
            HL if next_grapheme_class!((next_char is HY, BA)) => {
                /* LB21a Don’t break after Hebrew + Hyphen.  HL (HY | BA) × */
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB21b Don’t break between ,Solidus and Hebrew letters.  SY × HL */
            SY if next_grapheme_class!((next_char is HL)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                /* bypass next_char */
                self.iter.next().unwrap();
                if let Some((idx, next_grapheme)) = self.iter.next() {
                    *pos = idx + next_grapheme.len();
                }
                return self.next();
            }
            /*  LB22 Do not break between two ellipses, or between letters, numbers or excla-
             *  mations and ellipsis.
             *  Examples: ‘9...’, ‘a...’, ‘H...’
             *  (AL | HL) × IN */
            AL | HL if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /*  EX × IN */
            EX if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            EX => {
                // LB13
                *pos += grapheme.len();
                return self.next();
            }
            /*  (ID | EB | EM) × IN */
            ID | EB | EM if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /*  IN × IN */
            IN if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /*  NU × IN */
            NU if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB23 Do not break between digits and letters.
             * (AL | HL) × NU */
            AL | HL if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* NU × (AL | HL) */
            NU if next_grapheme_class!((next_char is AL, HL)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB23a Do not break between numeric prefixes and ideographs, or between ideographs
             * and numeric postfixes.
             * PR × (ID | EB | EM) */
            PR if next_grapheme_class!((next_char is ID, EB, EM)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* (ID | EB | EM) × PO */
            ID | EB | EM if next_grapheme_class!((next_char is PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* B24 Do not break between numeric prefix/postfix and letters, or between
            letters and prefix/postfix.
            (PR | PO) × (AL | HL)*/
            PR | PO if next_grapheme_class!((next_char is AL, HL)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /*(AL | HL) × (PR | PO) */
            AL | HL if next_grapheme_class!((next_char is PR, PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB25 Do not break between the following pairs of classes relevant to numbers:
             * CL × PO */
            CL if next_grapheme_class!((next_char is PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* CP × PO */
            CP if next_grapheme_class!((next_char is PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* CL × PR */
            CL if next_grapheme_class!((next_char is PR)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* CP × PR */
            CP if next_grapheme_class!((next_char is PR)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* NU × PO */
            NU if next_grapheme_class!((next_char is PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* NU × PR */
            NU if next_grapheme_class!((next_char is PR)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* PO × OP */
            PO if next_grapheme_class!((next_char is OP)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* PO × NU */
            PO if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* PR × OP */
            PR if next_grapheme_class!((next_char is OP)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* PR × NU */
            PR if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* HY × NU */
            HY if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* IS × NU */
            IS if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* NU × NU */
            NU if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* SY × NU */
            SY if next_grapheme_class!((next_char is NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB26 Do not break a Korean syllable.
             * JL × (JL | JV | H2 | H3) */
            JL if next_grapheme_class!((next_char is JL, JV, H2, H3)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* (JV | H2) × (JV | JT) */
            JV | H2 if next_grapheme_class!((next_char is JV, JT)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* (JT | H3) × JT */
            JT | H3 if next_grapheme_class!((next_char is JT)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB27 Treat a Korean Syllable Block the same as ID.
             * (JL | JV | JT | H2 | H3) × IN */
            JL | JV | JT | H2 | H3 if next_grapheme_class!((next_char is IN)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* (JL | JV | JT | H2 | H3) × PO */
            JL | JV | JT | H2 | H3 if next_grapheme_class!((next_char is PO)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* PR × (JL | JV | JT | H2 | H3) */
            PR if next_grapheme_class!((next_char is JL, JV, JT, H2, H3)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB28 Do not break between alphabetics (“at”).
            (AL | HL) × (AL | HL) */
            AL | HL if next_grapheme_class!((next_char is AL, HL)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB29 Do not break between numeric punctuation and alphabetics (“e.g.”).
            IS × (AL | HL) */
            IS if next_grapheme_class!((next_char is AL, HL)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* LB30 Do not break between letters, numbers, or ordinary symbols and opening
            or closing parentheses.
            (AL | HL | NU) × OP */
            AL | HL | NU if next_grapheme_class!((next_char is OP)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /* CP × (AL | HL | NU) */
            CP if next_grapheme_class!((next_char is AL, HL , NU)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            /*LB30b Do not break between an emoji base and an emoji modifier.
             * EB × EM */
            EB if next_grapheme_class!((next_char is EM)) => {
                let (idx, next_grapheme) = next_char.unwrap();
                *pos = idx + next_grapheme.len();
                self.iter.next();
                return self.next();
            }
            RI => {
                /* LB30a Break between two regional indicator symbols if and only if there are an
                 * even number of regional indicators preceding the position of the break.
                 * sot (RI RI)* RI × RI
                 * [^RI] (RI RI)* RI × RI */
                *reg_ind_streak += 1;
                *pos += grapheme.len();
                if *reg_ind_streak % 2 == 1 {
                    return Some((*pos - grapheme.len(), BreakAllowed));
                }
                self.iter.next();
                return self.next();
            }
            _ if next_char.is_none() => {
                return None;
            }
            _ => {
                *pos += grapheme.len();
                return Some((*pos - grapheme.len(), BreakAllowed));
            }
        }
    }
}

fn search_table(c: u32, t: &'static [(u32, u32, LineBreakClass)]) -> LineBreakClass {
    match t.binary_search_by(|&(lo, hi, _)| {
        if lo <= c && c <= hi {
            Ordering::Equal
        } else if hi < c {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }) {
        Ok(idx) => t[idx].2,
        Err(_) => XX,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_breaks() {
        let s = "Fell past it.\n\n‘Well!’ thought Alice to herself.";
        let breaks = LineBreakCandidateIter::new(s).collect::<Vec<(usize, LineBreakCandidate)>>();
        let mut prev = 0;
        for b in breaks {
            println!("{:?}", &s[prev..b.0]);
            prev = b.0;
        }
        println!("{:?}", &s[prev..]);

        let s = r#"Τ' άστρα τα κοντά -στη γλυκιά σελήνη
την ειδή των κρύβουν - τη διαμαντένια,
άμα φως λαμπρό -στη γή πάσα χύνει,
όλη ασημένια."#;
        let breaks = LineBreakCandidateIter::new(s).collect::<Vec<(usize, LineBreakCandidate)>>();
        let mut prev = 0;
        for b in breaks {
            println!("{:?}", &s[prev..b.0]);
            prev = b.0;
        }
        println!("{:?}", &s[prev..]);
    }
}

pub use alg::linear;

mod alg {
    use super::super::grapheme_clusters::TextProcessing;
    use super::super::*;
    fn cost(i: usize, j: usize, width: usize, minima: &Vec<usize>, offsets: &Vec<usize>) -> usize {
        let w = offsets[j] + j - offsets[i] - i - 1;
        if w > width {
            return 65536 * (w - width);
        }
        minima[i] + (width - w) * (width - w)
    }

    fn smawk(
        rows: &mut Vec<usize>,
        columns: &mut Vec<usize>,
        minima: &mut Vec<usize>,
        breaks: &mut Vec<usize>,
        width: usize,
        offsets: &Vec<usize>,
    ) {
        let mut stack = Vec::new();
        let mut i = 0;
        while i < rows.len() {
            if stack.len() > 0 {
                let c = columns[stack.len() - 1];
                if cost(*stack.iter().last().unwrap(), c, width, minima, offsets)
                    < cost(rows[i], c, width, minima, offsets)
                {
                    if stack.len() < columns.len() {
                        stack.push(rows[i]);
                    }
                    i += 1;
                } else {
                    stack.pop();
                }
            } else {
                stack.push(rows[i]);
                i += 1;
            }
        }
        let rows = &mut stack;
        if columns.len() > 1 {
            let mut odd_columns = columns.iter().skip(1).step_by(2).cloned().collect();
            smawk(rows, &mut odd_columns, minima, breaks, width, offsets);
            for (i, o) in odd_columns.into_iter().enumerate() {
                columns[2 * i + 1] = o;
            }
        }
        let mut i = 0;
        let mut j = 0;
        while j < columns.len() {
            let end = if j + 1 < columns.len() {
                breaks[columns[j + 1]]
            } else {
                *rows.iter().last().unwrap()
            };
            let c = cost(rows[i], columns[j], width, minima, offsets);
            if c < minima[columns[j]] {
                minima[columns[j]] = c;
                breaks[columns[j]] = rows[i];
            }
            if rows[i] < end {
                i += 1;
            } else {
                j += 2;
            }
        }
    }

    pub fn linear(text: &str, width: usize) -> Vec<String> {
        let mut words = Vec::new();
        let breaks =
            LineBreakCandidateIter::new(text).collect::<Vec<(usize, LineBreakCandidate)>>();
        {
            let mut prev = 0;
            for b in breaks {
                if text[prev..b.0].ends_with("\n") && text[b.0..].starts_with("\n") {
                    words.push(text[prev..b.0].trim_end_matches("\n"));
                    words.push("\n\n");
                } else if &text[prev..b.0] != "\n" {
                    words.push(text[prev..b.0].trim_end_matches("\n"));
                    if text[prev..b.0].ends_with("\n") {
                        words.push(" ");
                    }
                }
                prev = b.0;
            }
            if &text[prev..] != "\n" {
                words.push(text[prev..].trim_end_matches("\n"));
            }
        }
        let count = words.len();
        let mut minima = vec![std::usize::MAX - 1; count + 1];
        minima[0] = 0;
        let mut offsets = Vec::with_capacity(words.len());
        offsets.push(0);
        for w in words.iter() {
            if *w == "\n\n" {
                offsets.push(offsets.iter().last().unwrap() + width - 1);
            } else {
                offsets.push(offsets.iter().last().unwrap() + w.grapheme_len().saturating_sub(1));
            }
        }

        let mut breaks = vec![0; count + 1];

        let mut n = count + 1;
        let mut i = 1;
        let mut offset = 0;
        loop {
            let r = std::cmp::min(n, 2 * i);
            let edge = i + offset;
            smawk(
                &mut (offset..edge).collect(),
                &mut (edge..(r + offset)).collect(),
                &mut minima,
                &mut breaks,
                width,
                &offsets,
            );
            let x = minima[r - 1 + offset];
            let mut for_was_broken = false;
            for j in i..(r - 1) {
                let y = cost(j + offset, r - 1 + offset, width, &minima, &offsets);
                if y <= x {
                    n -= j;
                    i = 1;
                    offset += j;
                    for_was_broken = true;
                    break;
                }
            }

            if !for_was_broken {
                if r == n {
                    break;
                }
                i *= 2;
            }
        }
        let paragraphs = text.split("\n\n").count();
        let mut lines = Vec::new();
        let mut j = count;
        let mut p_i = 0;
        while j > 0 {
            let mut line = String::new();
            for i in breaks[j]..j {
                line.push_str(words[i]);
            }
            lines.push(line);
            if p_i + 1 < paragraphs {
                lines.push(String::new());
                p_i += 1;
            }
            j = breaks[j];
        }
        lines.reverse();
        lines
    }
}

pub fn split_lines_reflow(text: &str, reflow: Reflow, width: Option<usize>) -> Vec<String> {
    match reflow {
        Reflow::FormatFlowed => {
            /* rfc3676 - The Text/Plain Format and DelSp Parameters
             * https://tools.ietf.org/html/rfc3676 */

            let mut ret = Vec::new();
            /*
             * - Split lines with indices using str::match_indices()
             * - Iterate and reflow flow regions, and pass fixed regions through
             */
            let lines_indices: Vec<usize> = text.match_indices("\n").map(|(i, _)| i).collect();
            let mut prev_index = 0;
            let mut in_paragraph = false;
            let mut paragraph_start = 0;

            let mut prev_quote_depth = 0;
            for i in &lines_indices {
                let line = &text[prev_index..*i];
                let mut trimmed = line.trim_start().lines().next().unwrap_or("");
                let mut quote_depth = 0;
                let p_str: usize = trimmed
                    .as_bytes()
                    .iter()
                    .position(|&b| {
                        if b != b'>' {
                            /* position() is short-circuiting */
                            true
                        } else {
                            quote_depth += 1;
                            false
                        }
                    })
                    .unwrap_or(0);
                trimmed = &trimmed[p_str..];
                if trimmed.starts_with(" ") {
                    /* Remove space stuffing before checking for ending space character.
                     * [rfc3676#section-4.4] */
                    trimmed = &trimmed[1..];
                }

                if trimmed.ends_with(' ') {
                    if !in_paragraph {
                        in_paragraph = true;
                        paragraph_start = prev_index;
                    } else if prev_quote_depth == quote_depth {
                        /* This becomes part of the paragraph we're in */
                    } else {
                        /*Malformed line, different quote depths can't be in the same paragraph. */
                        let paragraph = &text[paragraph_start..prev_index];
                        reflow_helper(&mut ret, paragraph, prev_quote_depth, in_paragraph, width);

                        paragraph_start = prev_index;
                    }
                } else {
                    if prev_quote_depth == quote_depth || !in_paragraph {
                        let paragraph = &text[paragraph_start..*i];
                        reflow_helper(&mut ret, paragraph, quote_depth, in_paragraph, width);
                    } else {
                        /*Malformed line, different quote depths can't be in the same paragraph. */
                        let paragraph = &text[paragraph_start..prev_index];
                        reflow_helper(&mut ret, paragraph, prev_quote_depth, in_paragraph, width);
                        let paragraph = &text[prev_index..*i];
                        reflow_helper(&mut ret, paragraph, quote_depth, false, width);
                    }
                    paragraph_start = *i;
                    in_paragraph = false;
                }
                prev_quote_depth = quote_depth;
                prev_index = *i;
            }
            let paragraph = &text[paragraph_start..text.len()];
            reflow_helper(&mut ret, paragraph, prev_quote_depth, in_paragraph, width);
            ret
        }
        Reflow::All => {
            if let Some(width) = width {
                let mut ret = Vec::new();
                let width = width.saturating_sub(2);

                for line in text.lines() {
                    if line.grapheme_len() <= width {
                        ret.push(line.to_string());
                        continue;
                    }

                    let breaks = LineBreakCandidateIter::new(line)
                        .collect::<Vec<(usize, LineBreakCandidate)>>();
                    &breaks.iter().enumerate().collect::<Vec<_>>();
                    if breaks.len() < 2 {
                        split(&mut ret, line, width);
                        continue;
                    }

                    let mut prev = 0;
                    let mut prev_line_offset = 0;
                    while prev < breaks.len() {
                        let new_off = match breaks[prev..].binary_search_by(|(offset, _)| {
                            line[prev_line_offset..*offset].grapheme_len().cmp(&width)
                        }) {
                            Ok(v) => v,
                            Err(v) => v,
                        } + prev;
                        let end_offset = if new_off >= breaks.len() {
                            line.len()
                        } else {
                            breaks[new_off].0
                        };
                        if !line[prev_line_offset..end_offset].is_empty() {
                            if prev_line_offset == 0 {
                                ret.push(format!("{}", &line[prev_line_offset..end_offset]));
                            } else {
                                ret.push(format!("⤷{}", &line[prev_line_offset..end_offset]));
                            }
                        }
                        if prev_line_offset == end_offset && prev == new_off {
                            break;
                        }
                        prev_line_offset = end_offset;
                        prev = new_off;
                    }
                }
                ret
            } else {
                text.trim().split('\n').map(str::to_string).collect()
            }
        }
        Reflow::No => text.trim().split('\n').map(str::to_string).collect(),
    }
}

fn split(ret: &mut Vec<String>, mut line: &str, width: usize) {
    while !line.is_empty() {
        let mut chop_index = std::cmp::min(line.len().saturating_sub(1), width);
        while chop_index > 0 && !line.is_char_boundary(chop_index) {
            chop_index = chop_index - 1;
        }
        if chop_index == 0 {
            ret.push(format!("⤷{}", line));
            return;
        } else {
            ret.push(format!("⤷{}", &line[..chop_index]));
        }
        line = &line[chop_index..];
    }
}
fn reflow_helper(
    ret: &mut Vec<String>,
    paragraph: &str,
    quote_depth: usize,
    in_paragraph: bool,
    width: Option<usize>,
) {
    if quote_depth > 0 {
        let quotes: String = ">".repeat(quote_depth);
        let paragraph = paragraph
            .trim_start_matches(&quotes)
            .replace(&format!("\n{}", &quotes), "")
            .replace("\n", "")
            .replace("\r", "");
        if in_paragraph {
            if let Some(width) = width {
                ret.extend(
                    linear(&paragraph, width.saturating_sub(quote_depth))
                        .into_iter()
                        .map(|l| format!("{}{}", &quotes, l)),
                );
            } else {
                ret.push(format!("{}{}", &quotes, &paragraph));
            }
        } else {
            ret.push(format!("{}{}", &quotes, &paragraph));
        }
    } else {
        let paragraph = paragraph.replace("\n", "").replace("\r", "");

        if in_paragraph {
            if let Some(width) = width {
                let ex = linear(&paragraph, width);
                ret.extend(ex.into_iter());
            } else {
                ret.push(paragraph);
            }
        } else {
            ret.push(paragraph);
        }
    }
}

#[test]
fn test_reflow() {
    let text = r#"`Take some more tea,' the March Hare said to Alice, very 
earnestly.

`I've had nothing yet,' Alice replied in an offended tone, `so 
I can't take more.'

`You mean you can't take LESS,' said the Hatter: `it's very 
easy to take MORE than nothing.'"#;
    for l in split_lines_reflow(text, Reflow::FormatFlowed, Some(30)) {
        println!("{}", l);
    }
    println!("");
    for l in split_lines_reflow(text, Reflow::No, Some(30)) {
        println!("{}", l);
    }
    println!("");
    let text = r#">>>Take some more tea.
>>I've had nothing yet, so I can't take more.
>You mean you can't take LESS, it's very easy to take 
>MORE than nothing."#;
    for l in split_lines_reflow(text, Reflow::FormatFlowed, Some(20)) {
        println!("{}", l);
    }
    println!("");
    for l in split_lines_reflow(text, Reflow::No, Some(20)) {
        println!("{}", l);
    }
    println!("");
    let text = r#"CHAPTER I. Down the Rabbit-Hole

Alice was beginning to get very tired of sitting by her sister on the 
bank, and of having nothing to do: once or twice she had peeped into the 
book her sister was reading, but it had no pictures or conversations in 
it, ‘and what is the use of a book,’ thought Alice ‘without pictures or 
conversations?’

So she was considering in her own mind (as well as she could, for the 
hot day made her feel very sleepy and stupid), whether the pleasure 
of making a daisy-chain would be worth the trouble of getting up and 
picking the daisies, when suddenly a White Rabbit with pink eyes ran 
close by her.

>>There was nothing so VERY remarkable in that; nor did Alice think it so 
>>VERY much out of the way to hear the Rabbit say to itself, ‘Oh dear! 
>> Oh dear! I shall be late!’ (when she thought it over afterwards, it 
>>occurred to her that she ought to have wondered at this, but at the time 
>>it all seemed quite natural); but when the Rabbit actually TOOK A WATCH 
OUT OF ITS WAISTCOAT-POCKET, and looked at it, and then hurried on, 
>>Alice started to her feet, for it flashed across her mind that she had 
>>never before seen a rabbit with either a waistcoat-pocket, or a watch 
>>to take out of it, and burning with curiosity, she ran across the field
after it, and fortunately was just in time to see it pop down a large 
rabbit-hole under the hedge.

In another moment down went Alice after it, never once considering how 
in the world she was to get out again.

The rabbit-hole went straight on like a tunnel for some way, and then 
dipped suddenly down, so suddenly that Alice had not a moment to think 
about stopping herself before she found herself falling down a very deep 
well.

Either the well was very deep, or she fell very slowly, for she had 
plenty of time as she went down to look about her and to wonder what was 
going to happen next. First, she tried to look down and make out what 
she was coming to, but it was too dark to see anything; then she 
looked at the sides of the well, and noticed that they were filled with 
cupboards and book-shelves; here and there she saw maps and pictures 
hung upon pegs. She took down a jar from one of the shelves as 
she passed; it was labelled ‘ORANGE MARMALADE’, but to her great 
disappointment it was empty: she did not like to drop the jar for fear 
of killing somebody, so managed to put it into one of the cupboards as 
she fell past it.

‘Well!’ thought Alice to herself, ‘after such a fall as this, I shall 
think nothing of tumbling down stairs! How brave they’ll all think me at 
home! Why, I wouldn’t say anything about it, even if I fell off the top 
of the house!’ (Which was very likely true.)"#;
    for l in split_lines_reflow(text, Reflow::FormatFlowed, Some(72)) {
        println!("{}", l);
    }
}
