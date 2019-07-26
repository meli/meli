extern crate unicode_segmentation;
use self::unicode_segmentation::UnicodeSegmentation;
use crate::tables::LINE_BREAK_RULES;
use crate::types::LineBreakClass;
use core::cmp::Ordering;
use core::iter::Peekable;
use core::str::FromStr;
use LineBreakClass::*;

#[derive(Debug, PartialEq)]
pub enum LineBreakCandidate {
    MandatoryBreak,
    BreakAllowed,
    // NoBreak, Not used.
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
/// use text_processing::{self, LineBreakCandidate::{self, *}};
/// use text_processing::line_break::LineBreakCandidateIter;
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
    }
}
