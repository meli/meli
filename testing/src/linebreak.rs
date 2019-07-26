extern crate melib;
use melib::Result;
use melib::StackVec;

extern crate text_processing;
use text_processing::line_break::*;

fn cost(i: usize, j: usize, width: usize, minima: &Vec<usize>, offsets: &Vec<usize>) -> usize {
    let w = offsets[j] - offsets[i] + j - i - 1;
    if w > width {
        return 65536 * (w - width);
    }
    minima[i] + (width - w) * (width - w)
}

fn smawk(
    rows: &mut StackVec<usize>,
    columns: &mut StackVec<usize>,
    minima: &mut Vec<usize>,
    breaks: &mut Vec<usize>,
    width: usize,
    offsets: &Vec<usize>,
) {
    let mut stack = StackVec::new();
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
            columns.set(2 * i + 1, o);
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

fn linear(text: &str, width: usize) -> Vec<String> {
    let mut words = Vec::new();
    let breaks = LineBreakCandidateIter::new(text).collect::<Vec<(usize, LineBreakCandidate)>>();
    {
        let mut prev = 0;
        for b in breaks {
            if &text[prev..b.0] != "\n" {
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
        offsets.push(offsets.iter().last().unwrap() + w.len());
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

        if !for_was_broken || i >= (r - 1) {
            if r == n {
                break;
            }
            i *= 2;
        }
    }
    let mut lines = Vec::new();
    let mut j = count;
    while j > 0 {
        let mut line = String::new();
        for i in breaks[j]..j {
            line.push_str(words[i]);
        }
        lines.push(line);
        j = breaks[j];
    }
    lines.reverse();
    lines
}

fn main() -> Result<()> {
    let text = std::fs::read_to_string(std::env::args().nth(1).unwrap())?;
    let paragraphs = text.split("\n\n").collect::<Vec<&str>>();
    for (i, p) in paragraphs.iter().enumerate() {
        for l in linear(&p, 72) {
            println!("{}", l.trim());
        }
        if i + 1 < paragraphs.len() {
            println!("");
        }
    }
    Ok(())
}
