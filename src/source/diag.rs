use std::fmt;
use std::cmp;
use std::rc::Rc;
use super::{Pos, Span};

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
    pub pos: Option<Pos>,
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
        columns.push(vlen);

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
    pub fn print(&self, color: bool, tab: usize) {
        // Stringify and color severity
        let mut severity = format!("{}: ", self.severity);
        if color {
            severity = severity.color(self.severity.color()).to_string();
        }

        // If the message has no associated file, just print it
        if self.pos.is_none() {
            if color {
                println!("{}{}", severity, self.message.bold());
            } else {
                println!("{}{}", severity, self.message);
            }
            return
        }

        // Obtain line map
        let pos = &self.pos.as_ref().unwrap();
        let src = &pos.source;
        let linemap = src.linemap();

        // Get line number (starting from 0)
        let line = linemap.line_number(pos.pos);
        // Get position within the line
        let line_start = linemap.line_start_pos(line);
        let line_offset = pos.pos - line_start;
        // Get source code line for handling
        let line_text = linemap.line(src, line);
        let vstr = VisualString::new(line_text, tab);

        // Get colored severity string
        // Generate the error message line
        let mut msg = format!("{}:{}:{}: {}{}", src.filename(), line + 1, line_offset + 1, severity, self.message);
        if color {
            msg = msg.bold().to_string();
        }

        // Allocate a char vector to hold indicators
        // Make this 1 longer for possibility to point to the line break character.
        let mut indicators = vec![' '; vstr.visual_length() + 1];
        // Fill in ~ characters
        for span in &self.span {
            // Unlikely event, we cannot display this
            if !Rc::ptr_eq(&span.source, &pos.source) {
                continue
            }

            // Get start and end position, clamped within the line.
            let start = cmp::min(
                cmp::max(span.start as isize - line_start as isize, 0) as usize,
                line_text.len()
            );
            let end = cmp::min(
                cmp::max(span.end as isize - line_start as isize, 0) as usize,
                line_text.len()
            );

            // Nothing to display
            if start == end {
                continue
            }

            for i in vstr.visual_column(start)..vstr.visual_column(end) {
                indicators[i] = '~';
            }
        }

        // In case the pos is something wider then 1, fill with ~
        let vcolumn = vstr.visual_column(line_offset);
        for i in (vcolumn + 1)..vstr.visual_column(line_offset + 1) {
            indicators[i] = '~';
        }
        // Insert ^ character
        indicators[vcolumn] = '^';
        let mut indicator_line: String = indicators.into_iter().collect();
        if color {
            indicator_line = indicator_line.green().bold().to_string();
        }

        // If there is any fix-it-hints, we will have an additional line
        if !self.hint.is_empty() {
            let mut hints = vec![' '; vstr.visual_length()];

            for hint in &self.hint {
                // Unlikely event, we cannot display this
                if !Rc::ptr_eq(&hint.span.source, &pos.source) {
                    continue
                }
                let start = hint.span.start as isize - line_start as isize;
                let end = hint.span.end as isize - line_start as isize;
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