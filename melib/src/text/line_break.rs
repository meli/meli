/*
 * meli - text mod.
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

use std::{cmp::Ordering, collections::VecDeque, iter::Peekable, str::FromStr};

use unicode_segmentation::UnicodeSegmentation;
use LineBreakClass::*;

use super::{
    grapheme_clusters::TextProcessing,
    tables::LINE_BREAK_RULES,
    types::{LineBreakClass, Reflow},
};

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum LineBreakCandidate {
    MandatoryBreak,
    BreakAllowed,
    #[default]
    NoBreak, // Not used.
}

pub use alg::linear;
use LineBreakCandidate::*;

pub struct LineBreakCandidateIter<'a> {
    text: &'a str,
    iter: Peekable<unicode_segmentation::GraphemeIndices<'a>>,
    pos: usize,
    /* Needed for rule LB30a */
    reg_ind_streak: u32,
    /* Needed for break before and after opportunities */
    break_now: bool,
    last_break: usize,
}

impl<'a> LineBreakCandidateIter<'a> {
    pub fn new(text: &'a str) -> Self {
        LineBreakCandidateIter {
            text,
            pos: 0,
            iter: UnicodeSegmentation::grapheme_indices(text, true).peekable(),
            reg_ind_streak: 0,
            break_now: false,
            last_break: 0,
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

/// Side effects: Updates `$graph_iter` and potentially `$idx` and `$grapheme`
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

trait EvenAfterSpaces {
    fn even_after_spaces(&self) -> &Self;
}

impl EvenAfterSpaces for str {
    fn even_after_spaces(&self) -> &Self {
        let mut ret = self;
        while !ret.is_empty() && get_class!(ret) != SP {
            ret = &ret[get_base_character!(ret).unwrap().len_utf8()..];
        }
        ret
    }
}

/// Returns positions where breaks can happen
/// Examples:
/// ```
/// use melib::text::{
///     self,
///     line_break::LineBreakCandidateIter,
///     LineBreakCandidate::{self, *},
/// };
///
/// assert!(LineBreakCandidateIter::new("")
///     .collect::<Vec<(usize, LineBreakCandidate)>>()
///     .is_empty());
/// assert_eq!(
///     &[(7, BreakAllowed), (12, MandatoryBreak)],
///     LineBreakCandidateIter::new("Sample Text.")
///         .collect::<Vec<(usize, LineBreakCandidate)>>()
///         .as_slice()
/// );
/// assert_eq!(
///     &[
///         (3, MandatoryBreak),
///         (7, MandatoryBreak),
///         (10, BreakAllowed),
///         (17, MandatoryBreak)
///     ],
///     LineBreakCandidateIter::new("Sa\nmp\r\nle T(e)xt.")
///         .collect::<Vec<(usize, LineBreakCandidate)>>()
///         .as_slice()
/// );
/// ```
impl Iterator for LineBreakCandidateIter<'_> {
    type Item = (usize, LineBreakCandidate);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            macro_rules! set_last_break {
                ($last_break:expr, $pos:expr) => {
                    if $last_break == $pos {
                        continue;
                    }
                    $last_break = $pos;
                };
            }
            // After end of text, there are no breaks.
            if self.pos > self.text.len() {
                return None;
            }
            // LB3 Always break at the end of text
            if self.pos == self.text.len() {
                let ret = self.pos;
                self.pos += 1;
                set_last_break!(self.last_break, ret);
                return Some((ret, MandatoryBreak));
            }

            let LineBreakCandidateIter {
                ref mut iter,
                text,
                ref mut reg_ind_streak,
                ref mut break_now,
                ref mut last_break,
                ref mut pos,
            } = self;
            let (idx, mut grapheme) = iter.next().unwrap();
            let iter = iter.by_ref();

            debug_assert_eq!(idx, *pos);

            let class = get_class!(grapheme);

            if class != RI {
                *reg_ind_streak = 0;
            }

            /* LB1 Assign a line breaking class to each code point of the input. Resolve
             * AI, CB, CJ, SA, SG, and XX into other line breaking classes
             * depending on criteria outside the scope of this algorithm.
             *
             * In the absence of such criteria all characters with a specific combination
             * of original class and General_Category property value are
             * resolved as follows: Resolved Original     General_Category
             * AL       AI, SG, XX   Any
             * CM       SA           Only Mn or Mc
             * AL       SA           Any except Mn and Mc
             * NS       SJ           Any
             */

            // [ref:TODO]: LB1

            /* Check if next character class allows breaks before it */
            let mut next_char: Option<&(usize, &str)> = iter.peek();

            match class {
                BK => {
                    // LB4 Always Break after hard line breaks.
                    *pos += grapheme.len();
                    set_last_break!(*last_break, *pos);
                    return Some((*pos, MandatoryBreak));
                }
                // LB5 Treat CR followed by LF, as well as CR, LF, and NL as hard line breaks
                CR if next_grapheme_class!((next_char is LF)) => {
                    *pos += grapheme.len();
                    assert!(Some(LF) == next_grapheme_class!(iter, grapheme));
                    *pos += grapheme.len();
                    set_last_break!(*last_break, *pos);
                    return Some((*pos, MandatoryBreak));
                }
                CR | LF | NL => {
                    *pos += grapheme.len();
                    set_last_break!(*last_break, *pos);
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
                        continue;
                    }
                    /* LB7 Do not break before spaces or zero width
                     * space. × SP × ZW */
                    SP | ZW => {
                        *pos += grapheme.len();
                        continue;
                    }
                    WJ => {
                        /* : LB11 Do not break before or after Word joiner and related
                         * characters. */
                        *pos += grapheme.len();
                        continue;
                    }
                    _ if *break_now => {
                        *break_now = false;
                        let ret = *pos;
                        *pos += grapheme.len();
                        // LB2 Never break at the start of text
                        if ret == 0 {
                            continue;
                        }

                        set_last_break!(*last_break, ret);
                        return Some((ret, BreakAllowed));
                    }
                    _ => {}
                }
            }
            match class {
                ZW => {
                    // LB8 Break before any character following a zero-width space, even if one or
                    // more spaces intervene
                    // ZW SP* ÷
                    *pos += grapheme.len();
                    while next_grapheme_class!((next_char is SP)) {
                        let (_idx, grapheme) = iter.next().unwrap();
                        debug_assert_eq!(get_class!(grapheme), SP);
                        *pos += grapheme.len();
                        next_char = iter.peek();
                    }
                    set_last_break!(*last_break, *pos);
                    return Some((*pos, MandatoryBreak));
                }
                ZWJ => {
                    // LB8a Do not break after a zero width joiner.
                    *pos += grapheme.len();
                    continue;
                }

                CM => {
                    // LB9 Do not break a combining character sequence; treat it as if it has the
                    // line breaking class of the base character in all of the
                    // following rules. Treat ZWJ as if it were CM.
                    // Treat X (CM | ZWJ)* as if it were X.
                    // where X is any line break class except BK, CR, LF, NL, SP, or ZW.

                    *pos += grapheme.len();
                    continue;
                }
                WJ => {
                    /* : LB11 Do not break before or after Word joiner and related characters. */
                    *pos += grapheme.len();
                    /* Get next grapheme */
                    if next_grapheme_class!(iter, grapheme).is_some() {
                        *pos += grapheme.len();
                    }
                    continue;
                }
                GL => {
                    /* LB12 Non-breaking characters: LB12 Do not break after NBSP and related
                     * characters. */
                    *pos += grapheme.len();
                    continue;
                }
                _ => {}
            }
            if let Some((next_idx, next_grapheme)) = next_char {
                let next_class = get_class!(next_grapheme);
                match next_class {
                    GL if ![SP, BA, HY].contains(&class) => {
                        /* LB12a Do not break before NBSP and related characters, except after
                         * spaces and hyphens.  [^SP BA HY] × GL
                         * Also LB12 Do not break after NBSP and related characters */
                        *pos += grapheme.len();
                        continue;
                    }
                    /* LB13 Do not break before ‘]’ or ‘!’ or ‘;’ or ‘/’, even after spaces. */
                    CL | CP | EX | IS | SY => {
                        *pos = *next_idx;
                        continue;
                    }
                    _ => {}
                }
            }

            match class {
                /* LB13 Do not break before ‘]’ or ‘!’ or ‘;’ or ‘/’, even after spaces. */
                SP if !text[idx..].even_after_spaces().is_empty()
                    && [CL, CP, EX, IS, SY]
                        .contains(&get_class!(text[idx..].even_after_spaces())) =>
                {
                    *pos += grapheme.len();
                    while ![CL, CP, EX, IS, SY]
                        .contains(&next_grapheme_class!(iter, grapheme).unwrap())
                    {
                        *pos += grapheme.len();
                    }
                    *pos += grapheme.len();
                    continue;
                }
                OP => {
                    /* LB14 Do not break after ‘[’, even after spaces.
                     * OP SP* ×
                     */
                    *pos += grapheme.len();
                    while next_grapheme_class!((next_char is SP)) {
                        let (_idx, grapheme) = iter.next().unwrap();
                        debug_assert_eq!(get_class!(grapheme), SP);
                        *pos += grapheme.len();
                        next_char = iter.peek();
                    }
                    continue;
                }
                QU if !text[idx + grapheme.len()..].even_after_spaces().is_empty()
                    && get_class!(text[idx + grapheme.len()..].even_after_spaces()) == OP =>
                {
                    /* LB15 Do not break within ‘”[’, even with intervening spaces.
                     * QU SP* × OP */
                    *pos += grapheme.len();
                    while next_grapheme_class!((next_char is SP)) {
                        let (_idx, grapheme) = iter.next().unwrap();
                        debug_assert_eq!(get_class!(grapheme), SP);
                        *pos += grapheme.len();
                        next_char = iter.peek();
                    }
                    continue;
                }
                QU => {
                    /* LB19 Do not break before or after quotation marks, such as ‘ ” ’. */
                    *pos += grapheme.len();
                    if let Some((_, g)) = self.iter.next() {
                        *pos += g.len();
                    }
                    continue;
                }
                CL | CP
                    if !text[idx + grapheme.len()..].even_after_spaces().is_empty()
                        && get_class!(text[idx + grapheme.len()..].even_after_spaces()) == NS =>
                {
                    /* LB16 Do not break between closing punctuation and a nonstarter (lb=NS),
                     * even with intervening spaces.
                     * (CL | CP) SP* × NS */
                    *pos += grapheme.len();
                    while Some(SP) == next_grapheme_class!(iter, grapheme) {
                        *pos += grapheme.len();
                    }
                    continue;
                }
                B2 if !text[idx + grapheme.len()..].even_after_spaces().is_empty()
                    && get_class!(text[idx + grapheme.len()..].even_after_spaces()) == B2 =>
                {
                    /* LB17 Do not break within ‘——’, even with intervening spaces.
                     * B2 SP* × B2 */
                    *pos += grapheme.len();
                    continue;
                }
                SP => {
                    /* LB18 Break after spaces.  SP ÷ */
                    *pos += grapheme.len();
                    set_last_break!(*last_break, *pos);
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
                        continue;
                    }
                    _ => {}
                }
            }
            match class {
                CB => {
                    /* LB20 Break before and after unresolved CB. */
                    let ret = *pos;
                    *pos += grapheme.len();
                    *break_now = true;
                    // LB2 Never break at the start of text
                    if ret == 0 {
                        continue;
                    }
                    set_last_break!(*last_break, ret);
                    return Some((ret, BreakAllowed));
                }
                /* LB21 Do not break before hyphen-minus, other hyphens, fixed-width spaces,
                 * small kana, and other non-starters, or after acute accents.  ×
                 * BA,  × HY, × NS,  BB × */
                BB if !*break_now => {
                    *pos += grapheme.len();
                    continue;
                }
                _ => {}
            }

            if let Some((_, next_grapheme)) = next_char {
                let next_class = get_class!(next_grapheme);
                match next_class {
                    BA | HY | NS => {
                        /* LB21 Do not break before hyphen-minus, other hyphens, fixed-width
                         * spaces, small kana, and other non-starters, or
                         * after acute accents.  × BA,  × HY, × NS,  BB × */
                        *pos += grapheme.len();
                        //*pos += next_grapheme.len();
                        continue;
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
                    continue;
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
                    continue;
                }
                /*  LB22 Do not break between two ellipses, or between letters, numbers or excla-
                 *  mations and ellipsis.
                 *  Examples: ‘9...’, ‘a...’, ‘H...’
                 *  (AL | HL) × IN */
                AL | HL if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* EX × IN */
                EX if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                EX => {
                    // LB13
                    *pos += grapheme.len();
                    continue;
                }
                /* (ID | EB | EM) × IN */
                ID | EB | EM if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* IN × IN */
                IN if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* NU × IN */
                NU if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB23 Do not break between digits and letters.
                 * (AL | HL) × NU */
                AL | HL if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* NU × (AL | HL) */
                NU if next_grapheme_class!((next_char is AL, HL)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB23a Do not break between numeric prefixes and ideographs, or between
                 * ideographs and numeric postfixes.
                 * PR × (ID | EB | EM) */
                PR if next_grapheme_class!((next_char is ID, EB, EM)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* (ID | EB | EM) × PO */
                ID | EB | EM if next_grapheme_class!((next_char is PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* B24 Do not break between numeric prefix/postfix and letters, or between
                letters and prefix/postfix.
                (PR | PO) × (AL | HL)*/
                PR | PO if next_grapheme_class!((next_char is AL, HL)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* (AL | HL) × (PR | PO) */
                AL | HL if next_grapheme_class!((next_char is PR, PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB25 Do not break between the following pairs of classes relevant to numbers:
                 * CL × PO */
                CL if next_grapheme_class!((next_char is PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* CP × PO */
                CP if next_grapheme_class!((next_char is PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* CL × PR */
                CL if next_grapheme_class!((next_char is PR)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* CP × PR */
                CP if next_grapheme_class!((next_char is PR)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* NU × PO */
                NU if next_grapheme_class!((next_char is PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* NU × PR */
                NU if next_grapheme_class!((next_char is PR)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* PO × OP */
                PO if next_grapheme_class!((next_char is OP)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* PO × NU */
                PO if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* PR × OP */
                PR if next_grapheme_class!((next_char is OP)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* PR × NU */
                PR if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* HY × NU */
                HY if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* IS × NU */
                IS if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* NU × NU */
                NU if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* SY × NU */
                SY if next_grapheme_class!((next_char is NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB26 Do not break a Korean syllable.
                 * JL × (JL | JV | H2 | H3) */
                JL if next_grapheme_class!((next_char is JL, JV, H2, H3)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* (JV | H2) × (JV | JT) */
                JV | H2 if next_grapheme_class!((next_char is JV, JT)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* (JT | H3) × JT */
                JT | H3 if next_grapheme_class!((next_char is JT)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB27 Treat a Korean Syllable Block the same as ID.
                 * (JL | JV | JT | H2 | H3) × IN */
                JL | JV | JT | H2 | H3 if next_grapheme_class!((next_char is IN)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* (JL | JV | JT | H2 | H3) × PO */
                JL | JV | JT | H2 | H3 if next_grapheme_class!((next_char is PO)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* PR × (JL | JV | JT | H2 | H3) */
                PR if next_grapheme_class!((next_char is JL, JV, JT, H2, H3)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB28 Do not break between alphabetics (“at”).
                (AL | HL) × (AL | HL) */
                AL | HL if next_grapheme_class!((next_char is AL, HL)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB29 Do not break between numeric punctuation and alphabetics (“e.g.”).
                IS × (AL | HL) */
                IS if next_grapheme_class!((next_char is AL, HL)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* LB30 Do not break between letters, numbers, or ordinary symbols and opening
                or closing parentheses.
                (AL | HL | NU) × OP */
                AL | HL | NU if next_grapheme_class!((next_char is OP)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /* CP × (AL | HL | NU) */
                CP if next_grapheme_class!((next_char is AL, HL , NU)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                /*LB30b Do not break between an emoji base and an emoji modifier.
                 * EB × EM */
                EB if next_grapheme_class!((next_char is EM)) => {
                    let (idx, next_grapheme) = next_char.unwrap();
                    *pos = idx + next_grapheme.len();
                    self.iter.next();
                    continue;
                }
                RI => {
                    /* LB30a Break between two regional indicator symbols if and only if there
                     * are an even number of regional indicators preceding
                     * the position of the break. sot (RI RI)* RI × RI
                     * [^RI] (RI RI)* RI × RI */
                    *reg_ind_streak += 1;
                    *pos += grapheme.len();
                    if *reg_ind_streak % 2 == 1 {
                        let ret = *pos - grapheme.len();
                        // LB2 Never break at the start of text
                        if ret == 0 {
                            continue;
                        }
                        set_last_break!(*last_break, ret);
                        return Some((ret, BreakAllowed));
                    }
                    self.iter.next();
                    continue;
                }
                CL | CP | IS | SY => {
                    *pos += grapheme.len();
                    continue;
                }
                BK | CR | LF | NL => {
                    *pos += grapheme.len();
                    continue;
                }
                SP | ZW => {
                    *pos += grapheme.len();
                    continue;
                }
                BA | HY | NS => {
                    *pos += grapheme.len();
                    continue;
                }
                _ => {
                    /* LB31 Break everywhere else.
                     * ALL ÷
                     * ÷ ALL
                     */
                    let ret = *pos;
                    // ALL ÷
                    *break_now = true;
                    *pos += grapheme.len();
                    // LB2 Never break at the start of text
                    if ret == 0 {
                        continue;
                    }
                    // ÷ ALL
                    set_last_break!(*last_break, ret);
                    return Some((ret, BreakAllowed));
                }
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

mod alg {
    use crate::text::{grapheme_clusters::TextProcessing, *};

    fn cost(i: usize, j: usize, width: usize, minima: &[usize], offsets: &[usize]) -> usize {
        let w = offsets[j] + j - offsets[i] - i - 1;
        if w > width {
            return 65536 * (w - width);
        }
        minima[i] + (width - w) * (width - w)
    }

    fn smawk(
        rows: &[usize],
        columns: &mut [usize],
        minima: &mut Vec<usize>,
        breaks: &mut Vec<usize>,
        width: usize,
        offsets: &[usize],
    ) {
        let mut stack = Vec::new();
        let mut i = 0;
        while i < rows.len() {
            if !stack.is_empty() {
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
        let rows = &stack;
        if columns.len() > 1 {
            let mut odd_columns = columns
                .iter()
                .skip(1)
                .step_by(2)
                .cloned()
                .collect::<Vec<_>>();
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
                if text[prev..b.0].ends_with('\n') && text[b.0..].starts_with('\n') {
                    words.push(text[prev..b.0].trim_end_matches('\n'));
                    words.push("\n\n");
                } else if &text[prev..b.0] != "\n" {
                    words.push(text[prev..b.0].trim_end_matches('\n'));
                    if text[prev..b.0].ends_with('\n') {
                        words.push(" ");
                    }
                }
                prev = b.0;
            }
            if &text[prev..] != "\n" {
                words.push(text[prev..].trim_end_matches('\n'));
            }
        }
        let count = words.len();
        let mut minima = vec![usize::MAX - 1; count + 1];
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
                &(offset..edge).collect::<Vec<_>>(),
                &mut (edge..(r + offset)).collect::<Vec<_>>(),
                &mut minima,
                &mut breaks,
                width,
                &offsets,
            );
            let x = minima[r - 1 + offset];
            let mut for_was_broken = false;
            let i_copy = i;
            for j in i_copy..(r - 1) {
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
            for word in words.iter().take(j).skip(breaks[j]) {
                line.push_str(word);
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
            let lines_indices: Vec<usize> = text.match_indices('\n').map(|(i, _)| i).collect();
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
                if trimmed.starts_with(' ') {
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
                        /* Malformed line, different quote depths can't be in the same paragraph. */
                        let paragraph = &text[paragraph_start..prev_index];
                        reflow_helper(&mut ret, paragraph, prev_quote_depth, in_paragraph, width);

                        paragraph_start = prev_index;
                    }
                } else {
                    if prev_quote_depth == quote_depth || !in_paragraph {
                        let paragraph = &text[paragraph_start..*i];
                        reflow_helper(&mut ret, paragraph, quote_depth, in_paragraph, width);
                    } else {
                        /* Malformed line, different quote depths can't be in the same paragraph. */
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
                    if breaks.len() < 2 {
                        split(&mut ret, line, width);
                        continue;
                    }
                    let segment_tree = {
                        let mut t: smallvec::SmallVec<[usize; 1024]> =
                            smallvec::SmallVec::from_iter(std::iter::repeat(0).take(line.len()));
                        for (idx, _g) in UnicodeSegmentation::grapheme_indices(line, true) {
                            t[idx] = 1;
                        }
                        Box::new(segment_tree::SegmentTree::new(t))
                    };

                    let mut prev = 0;
                    let mut prev_line_offset = 0;
                    while prev < breaks.len() {
                        let new_off = match breaks[prev..].binary_search_by(|(offset, _)| {
                            segment_tree
                                .get_sum(prev_line_offset, offset.saturating_sub(1))
                                .cmp(&width)
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
                                ret.push(line[prev_line_offset..end_offset].to_string());
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
            chop_index -= 1;
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
            .replace(['\n', '\r'], "");
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
        let paragraph = paragraph.replace(['\n', '\r'], "");

        if in_paragraph {
            if let Some(width) = width {
                let ex = linear(&paragraph, width);
                ret.extend(ex);
            } else {
                ret.push(paragraph);
            }
        } else {
            ret.push(paragraph);
        }
    }
}

mod segment_tree {
    //! Simple segment tree implementation for maximum in range queries. This
    //! is useful if given an  array of numbers you want to get the
    //! maximum value inside an interval quickly.
    use std::{convert::TryFrom, iter::FromIterator};

    use smallvec::SmallVec;

    #[derive(Clone, Debug, Default)]
    pub(super) struct SegmentTree {
        array: SmallVec<[usize; 1024]>,
        tree: SmallVec<[usize; 1024]>,
    }

    impl SegmentTree {
        pub(super) fn new(val: SmallVec<[usize; 1024]>) -> Self {
            if val.is_empty() {
                return Self {
                    array: val.clone(),
                    tree: val,
                };
            }

            let height = (f64::from(u32::try_from(val.len()).unwrap_or(0)))
                .log2()
                .ceil() as u32;
            let max_size = 2 * (2_usize.pow(height));

            let mut segment_tree: SmallVec<[usize; 1024]> =
                SmallVec::from_iter(std::iter::repeat(0).take(max_size));
            for i in 0..val.len() {
                segment_tree[val.len() + i] = val[i];
            }

            for i in (1..val.len()).rev() {
                segment_tree[i] = segment_tree[2 * i] + segment_tree[2 * i + 1];
            }

            Self {
                array: val,
                tree: segment_tree,
            }
        }

        /// (left, right) is inclusive
        pub(super) fn get_sum(&self, mut left: usize, mut right: usize) -> usize {
            if self.array.is_empty() {
                return 0;
            }

            let len = self.array.len();
            if left > right {
                return 0;
            }
            if right >= len {
                right = len.saturating_sub(1);
            }

            left += len;
            right += len + 1;

            let mut sum = 0;

            while left < right {
                if (left & 1) > 0 {
                    sum += self.tree[left];
                    left += 1;
                }

                if (right & 1) > 0 {
                    right -= 1;
                    sum += self.tree[right];
                }

                left /= 2;
                right /= 2;
            }
            sum
        }
    }
}

/// A lazy stateful iterator for line breaking text. Useful for very long text
/// where you don't want to linebreak it completely before user requests
/// specific lines.
#[derive(Clone, Debug)]
pub struct LineBreakText {
    text: String,
    reflow: Reflow,
    paragraph: VecDeque<String>,
    paragraph_start_index: usize,
    width: Option<usize>,
    state: ReflowState,
}

#[derive(Clone, Debug)]
enum ReflowState {
    No {
        cur_index: usize,
    },
    AllWidth {
        width: usize,
        state: LineBreakTextState,
    },
    All {
        cur_index: usize,
    },
    FormatFlowed {
        cur_index: usize,
    },
}

impl ReflowState {
    fn new(reflow: Reflow, width: Option<usize>, cur_index: usize) -> Self {
        match reflow {
            Reflow::All if width.is_some() => Self::AllWidth {
                width: width.unwrap(),
                state: LineBreakTextState::AtLine { cur_index },
            },
            Reflow::All => Self::All { cur_index },
            Reflow::FormatFlowed => Self::FormatFlowed { cur_index },
            Reflow::No => Self::No { cur_index },
        }
    }
}

#[derive(Clone, Debug)]
enum LineBreakTextState {
    AtLine {
        cur_index: usize,
    },
    WithinLine {
        line_index: usize,
        line_length: usize,
        within_line_index: usize,
        breaks: Vec<(usize, LineBreakCandidate)>,
        prev_break: usize,
        segment_tree: Box<segment_tree::SegmentTree>,
    },
}

impl Default for LineBreakText {
    fn default() -> Self {
        Self::new(String::new(), Reflow::default(), None)
    }
}

impl LineBreakText {
    pub fn new(text: String, reflow: Reflow, width: Option<usize>) -> Self {
        Self {
            text,
            state: ReflowState::new(reflow, width, 0),
            paragraph: VecDeque::new(),
            paragraph_start_index: 0,
            reflow,
            width,
        }
    }

    pub fn width(&self) -> Option<usize> {
        self.width
    }

    pub fn set_reflow(&mut self, new_val: Reflow) -> &mut Self {
        self.reflow = new_val;
        self.paragraph.clear();
        self.state = ReflowState::new(self.reflow, self.width, self.paragraph_start_index);
        self
    }

    pub fn set_width(&mut self, new_val: Option<usize>) -> &mut Self {
        self.width = new_val;
        self.paragraph.clear();
        self.state = ReflowState::new(self.reflow, self.width, self.paragraph_start_index);
        self
    }

    pub fn set_text(&mut self, new_val: String) -> &mut Self {
        self.text = new_val;
        self.reset()
    }

    pub fn reset(&mut self) -> &mut Self {
        self.paragraph.clear();
        self.state = ReflowState::new(self.reflow, self.width, 0);
        self.paragraph_start_index = 0;
        self
    }

    pub fn is_finished(&self) -> bool {
        match self.state {
            ReflowState::No { cur_index }
            | ReflowState::All { cur_index }
            | ReflowState::FormatFlowed { cur_index }
            | ReflowState::AllWidth {
                width: _,
                state: LineBreakTextState::AtLine { cur_index },
            } => cur_index >= self.text.len(),
            ReflowState::AllWidth {
                width: _,
                state: LineBreakTextState::WithinLine { .. },
            } => false,
        }
    }
}

impl Iterator for LineBreakText {
    type Item = String;
    fn next(&mut self) -> Option<Self::Item> {
        if !self.paragraph.is_empty() {
            return self.paragraph.pop_front();
        }
        if self.is_finished() {
            return None;
        }
        match self.state {
            ReflowState::FormatFlowed { ref mut cur_index } => {
                /* rfc3676 - The Text/Plain Format and DelSp Parameters
                 * https://tools.ietf.org/html/rfc3676 */

                /*
                 * - Split lines with indices using str::match_indices()
                 * - Iterate and reflow flow regions, and pass fixed regions through
                 */
                self.paragraph_start_index = *cur_index;
                let line_indices_iter = self.text[*cur_index..].match_indices('\n').map(|(i, _)| i);
                let start_offset = *cur_index;
                let mut prev_index = *cur_index;
                let mut in_paragraph = false;
                let mut paragraph_start = *cur_index;

                let mut prev_quote_depth = 0;
                let mut paragraph = VecDeque::new();
                for i in line_indices_iter {
                    let i = i + start_offset + 1;
                    let line = &self.text[prev_index..i];
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
                    if trimmed.starts_with(' ') {
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
                            /* Malformed line, different quote depths can't be in the same
                             * paragraph. */
                            let paragraph_s = &self.text[paragraph_start..prev_index];
                            reflow_helper2(
                                &mut paragraph,
                                paragraph_s,
                                prev_quote_depth,
                                in_paragraph,
                                self.width,
                            );

                            paragraph_start = prev_index;
                        }
                    } else {
                        if prev_quote_depth == quote_depth || !in_paragraph {
                            let paragraph_s = &self.text[paragraph_start..i];
                            reflow_helper2(
                                &mut paragraph,
                                paragraph_s,
                                quote_depth,
                                in_paragraph,
                                self.width,
                            );
                        } else {
                            /* Malformed line, different quote depths can't be in the same
                             * paragraph. */
                            let paragraph_s = &self.text[paragraph_start..prev_index];
                            reflow_helper2(
                                &mut paragraph,
                                paragraph_s,
                                prev_quote_depth,
                                in_paragraph,
                                self.width,
                            );
                            let paragraph_s = &self.text[prev_index..i];
                            reflow_helper2(
                                &mut paragraph,
                                paragraph_s,
                                quote_depth,
                                false,
                                self.width,
                            );
                        }
                        *cur_index = i;
                        std::mem::swap(&mut self.paragraph, &mut paragraph);
                        paragraph_start = i;
                        in_paragraph = false;
                        break;
                    }
                    *cur_index = i;
                    prev_quote_depth = quote_depth;
                    prev_index = i;
                }
                if in_paragraph {
                    let paragraph_s = &self.text[paragraph_start..self.text.len()];
                    *cur_index = self.text.len();
                    reflow_helper2(
                        &mut paragraph,
                        paragraph_s,
                        prev_quote_depth,
                        in_paragraph,
                        self.width,
                    );
                    self.paragraph = paragraph;
                }
                self.paragraph.pop_front()
            }
            ReflowState::AllWidth {
                width,
                ref mut state,
            } => {
                let width = width.saturating_sub(2);

                loop {
                    let line: &str;
                    let cur_index: &mut usize;
                    let within_line_index: &mut usize;
                    let prev_break: &mut usize;
                    let segment_tree: &segment_tree::SegmentTree;
                    let breaks: &Vec<(usize, LineBreakCandidate)>;
                    match state {
                        LineBreakTextState::AtLine {
                            cur_index: ref mut _cur_index,
                        } => {
                            line = if let Some(line) = self
                                .text
                                .get(*_cur_index..)
                                .and_then(|slice| slice.split('\n').next())
                            {
                                line
                            } else {
                                *_cur_index = self.text.len();
                                return None;
                            };
                            let _cur_index = *_cur_index;
                            *state = LineBreakTextState::WithinLine {
                                line_index: _cur_index,
                                line_length: line.len(),
                                within_line_index: 0,
                                breaks: LineBreakCandidateIter::new(line).collect::<Vec<(
                                    usize,
                                    LineBreakCandidate,
                                )>>(
                                ),
                                prev_break: 0,
                                segment_tree: {
                                    let mut t: smallvec::SmallVec<[usize; 1024]> =
                                        smallvec::SmallVec::from_iter(
                                            std::iter::repeat(0).take(line.len()),
                                        );
                                    for (idx, _g) in
                                        UnicodeSegmentation::grapheme_indices(line, true)
                                    {
                                        t[idx] = 1;
                                    }
                                    Box::new(segment_tree::SegmentTree::new(t))
                                },
                            };
                            if let LineBreakTextState::WithinLine {
                                ref mut line_index,
                                line_length: _,
                                within_line_index: ref mut _within_line_index,
                                breaks: ref _breaks,
                                prev_break: ref mut _prev_break,
                                segment_tree: ref _segment_tree,
                            } = state
                            {
                                cur_index = line_index;
                                within_line_index = _within_line_index;
                                breaks = _breaks;
                                prev_break = _prev_break;

                                segment_tree = _segment_tree;
                            } else {
                                unreachable!()
                            }
                        }
                        LineBreakTextState::WithinLine {
                            ref mut line_index,
                            ref line_length,
                            within_line_index: ref mut _within_line_index,
                            breaks: ref _breaks,
                            prev_break: ref mut _prev_break,
                            segment_tree: ref _segment_tree,
                        } => {
                            line = &self.text[*line_index..(*line_index + *line_length)];
                            cur_index = line_index;
                            within_line_index = _within_line_index;
                            breaks = _breaks;
                            prev_break = _prev_break;
                            segment_tree = _segment_tree;
                        }
                    }

                    if segment_tree.get_sum(0, line.len()) <= width {
                        *state = LineBreakTextState::AtLine {
                            cur_index: *cur_index + line.len() + 1,
                        };
                        return Some(line.trim_end_matches(['\r', '\n']).to_string());
                    }
                    if breaks.len() < 2 {
                        let mut line = line;
                        while !line.is_empty() {
                            let mut chop_index = std::cmp::min(line.len().saturating_sub(1), width);
                            while chop_index > 0 && !line.is_char_boundary(chop_index) {
                                chop_index -= 1;
                            }
                            if chop_index == 0 {
                                self.paragraph.push_back(format!("⤷{}", line));
                                *cur_index += line.len();
                                break;
                            } else {
                                self.paragraph
                                    .push_back(format!("⤷{}", &line[..chop_index]));
                                *cur_index += chop_index;
                            }
                            line = &line[chop_index..];
                        }
                        *state = LineBreakTextState::AtLine {
                            cur_index: *cur_index,
                        };
                        if !self.paragraph.is_empty() {
                            return self.paragraph.pop_front();
                        }
                        continue;
                    }

                    while *prev_break < breaks.len() {
                        let new_off = match breaks[*prev_break..].binary_search_by(|(offset, _)| {
                            segment_tree
                                .get_sum(*within_line_index, offset.saturating_sub(1))
                                .cmp(&width)
                        }) {
                            Ok(v) => v,
                            Err(v) => v,
                        } + *prev_break;
                        let end_offset = if new_off >= breaks.len() {
                            line.len()
                        } else {
                            breaks[new_off].0
                        };
                        if !line[*within_line_index..end_offset].is_empty() {
                            if *within_line_index == 0 {
                                let ret = line[*within_line_index..end_offset]
                                    .trim_end_matches(['\r', '\n']);
                                *within_line_index = end_offset;
                                return Some(ret.to_string());
                            } else {
                                let ret = format!(
                                    "⤷{}",
                                    &line[*within_line_index..end_offset]
                                        .trim_end_matches(['\r', '\n'])
                                );
                                *within_line_index = end_offset;
                                return Some(ret);
                            }
                        }
                        if *within_line_index == end_offset && *prev_break == new_off {
                            break;
                        }
                        *within_line_index = end_offset + 1;
                        *prev_break = new_off;
                    }
                    *state = LineBreakTextState::AtLine {
                        cur_index: *cur_index + line.len() + 1,
                    };
                }
            }
            ReflowState::No { ref mut cur_index } | ReflowState::All { ref mut cur_index } => {
                if let Some(line) = self.text[*cur_index..].split('\n').next() {
                    let ret = line.to_string();
                    let mut chop_index = line.len() + 1;
                    while *cur_index + chop_index < self.text.len()
                        && !self.text.is_char_boundary(*cur_index + chop_index)
                    {
                        chop_index += 1;
                    }
                    *cur_index += chop_index;
                    return Some(ret);
                }
                None
            }
        }
    }
}

fn reflow_helper2(
    ret: &mut VecDeque<String>,
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
            .replace(['\n', '\r'], "");
        if in_paragraph {
            if let Some(width) = width {
                ret.extend(
                    linear(&paragraph, width.saturating_sub(quote_depth))
                        .into_iter()
                        .map(|l| format!("{}{}", &quotes, l)),
                );
            } else {
                ret.push_back(format!("{}{}", &quotes, &paragraph));
            }
        } else {
            ret.push_back(format!("{}{}", &quotes, &paragraph));
        }
    } else {
        let paragraph = paragraph.replace(['\n', '\r'], "");

        if in_paragraph {
            if let Some(width) = width {
                let ex = linear(&paragraph, width);
                ret.extend(ex);
            } else {
                ret.push_back(paragraph);
            }
        } else {
            ret.push_back(paragraph);
        }
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
        println!();
        for l in split_lines_reflow(text, Reflow::No, Some(30)) {
            println!("{}", l);
        }
        println!();
        let text = r#">>>Take some more tea.
>>I've had nothing yet, so I can't take more.
>You mean you can't take LESS, it's very easy to take 
>MORE than nothing."#;
        for l in split_lines_reflow(text, Reflow::FormatFlowed, Some(20)) {
            println!("{}", l);
        }
        println!();
        for l in split_lines_reflow(text, Reflow::No, Some(20)) {
            println!("{}", l);
        }
        println!();
        use crate::text::_ALICE_CHAPTER_1;
        for l in split_lines_reflow(_ALICE_CHAPTER_1, Reflow::FormatFlowed, Some(72)) {
            println!("{}", l);
        }
    }
}
