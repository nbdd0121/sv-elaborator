use std::fmt;
use std::cmp;
use std::rc::Rc;
use super::{Span, SrcMgr};

use colored::{Color, Colorize};

#[derive(Debug)]
pub enum Severity {
    Remark,
    Info,
    Warning,
    Error,
    Fatal,
}

impl Severity {
    fn color(&self) -> Color {
        match self {
            Severity::Remark => Color::Blue,
            Severity::Info => Color::Black,
            Severity::Warning => Color::Magenta,
            Severity::Error | Severity::Fatal => Color::Red,
        }
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Severity::Remark => "remark",
            Severity::Info => "info",
            Severity::Warning => "warning",
            Severity::Error => "error",
            Severity::Fatal => "fatal error",
        };
        write!(f, "{}", str)
    }
}

pub struct FixItHint {
    span: Span,
    replace: String
}

impl FixItHint {
    pub fn new(span: Span, replace: String) -> FixItHint {
        FixItHint {
            span: span,
            replace: replace,
        }
    }
}

pub struct DiagMsg {
    pub severity: Severity,
    pub message: String,
    pub span: Vec<Span>,
    pub hint: Vec<FixItHint>
}

// Helper class for printing column number in a file with tabs and non-ASCII characters.
struct VisualString {
    str: String,
    columns: Vec<usize>,
}

impl VisualString {
    fn new(str: &str, tab: usize) -> VisualString {
        let mut columns = Vec::with_capacity(str.len() + 1);
        columns.push(0);

        // Current visual string and visual length
        let mut vstr = String::new();
        let mut vlen = 0;
        
        for ch in str.chars() {
            match ch {
                '\r' | '\n' => (),
                '\t' => {
                    let newlen = (vlen + tab) / tab * tab;
                    for _ in vlen..newlen {
                        vstr.push(' ');
                    }
                    vlen = newlen
                }
                _ => {
                    vstr.push(ch);
                    vlen += 1
                }
            }

            for _ in 0..ch.len_utf8() {
                columns.push(vlen);
            }
        }

        // Reserve a column for end-of-line character
        columns.push(vlen + 1);

        VisualString {
            str: vstr,
            columns: columns,
        }
    }

    fn visual_column(&self, pos: usize) -> usize {
        self.columns[pos]
    }

    fn visual_length(&self) -> usize {
        self.columns[self.columns.len() - 1]
    }

    fn visual_text(&self) -> &str {
        &self.str
    }
}

impl DiagMsg {
    pub fn print(&self, mgr: &SrcMgr, color: bool, tab: usize) {
        // Stringify and color severity
        let mut severity = format!("{}: ", self.severity);
        if color {
            severity = severity.color(self.severity.color()).to_string();
        }

        // Convert all spans to fat spans
        let spans: Vec<_> = self.span.iter().flat_map(|x| mgr.find_span(*x)).collect();

        // If the message has no associated file, just print it
        if spans.is_empty() {
            if color {
                println!("{}{}", severity, self.message.bold());
            } else {
                println!("{}{}", severity, self.message);
            }
            return
        }

        // Obtain line map
        let first_span = spans.first().unwrap();
        let src = &first_span.source;
        let linemap = src.linemap();

        // Get line number (starting from 0)
        let line = linemap.line_number(first_span.start);
        // Get position within the line
        let line_start = linemap.line_start_pos(line);
        // Get source code line for handling
        let line_text = linemap.line(src, line);
        let vstr = VisualString::new(line_text, tab);

        // Get colored severity string
        // Generate the error message line
        let mut msg = format!("{}:{}: {}{}", src.filename(), line + 1, severity, self.message);
        if color {
            msg = msg.bold().to_string();
        }

        // Allocate a char vector to hold indicators
        // Make this 1 longer for possibility to point to the line break character.
        let mut indicators = vec![' '; vstr.visual_length() + 1];
        let mut character = '^';

        // Fill in ^ and ~ characters for all spans
        for span in &spans {
            // Unlikely event, we cannot display this
            if !Rc::ptr_eq(&span.source, &first_span.source) {
                continue
            }

            // Get start and end position, clamped within the line.
            let start = cmp::min(
                cmp::max(span.start as isize - line_start as isize, 0) as usize,
                line_text.len()
            );
            let end = cmp::min(
                cmp::max(span.end as isize - line_start as isize, 0) as usize,
                line_text.len() + 1
            );

            // Nothing to display
            if start == end {
                continue
            }

            for i in vstr.visual_column(start)..vstr.visual_column(end) {
                indicators[i] = character;
            }

            character = '~';
        }

        let mut indicator_line: String = indicators.into_iter().collect();
        if color {
            indicator_line = indicator_line.green().bold().to_string();
        }

        // If there is any fix-it-hints, we will have an additional line
        if !self.hint.is_empty() {
            let mut hints = vec![' '; vstr.visual_length()];

            for hint in &self.hint {
                let span = match mgr.find_span(hint.span) {
                    None => continue,
                    Some(v) => v,
                };

                // Unlikely event, we cannot display this
                if !Rc::ptr_eq(&span.source, &first_span.source) {
                    continue
                }

                let start = span.start as isize - line_start as isize;
                let end = span.end as isize - line_start as isize;
                // We can only display it if it partially covers this line
                if end < 0 || start > line_text.len() as isize {
                    continue;
                }
                let mut vptr = cmp::min(cmp::max(start, 0) as usize, line_text.len());
                // Now replace the part in vector with the replacement suggestion
                for ch in hint.replace.chars() {
                    if vptr >= hints.len() {
                        hints.push(ch);
                    } else {
                        hints[vptr] = ch;
                    }
                    vptr += 1;
                }
            }

            let mut line: String = hints.into_iter().collect();
            if color {
                line = line.green().to_string();
            }

            println!("{}\n{}\n{}\n{}", msg, vstr.visual_text(), indicator_line, line);
        } else {
            println!("{}\n{}\n{}", msg, vstr.visual_text(), indicator_line);
        }
    }
}
