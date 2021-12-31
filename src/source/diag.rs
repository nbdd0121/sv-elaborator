// Diagnostics engine

use super::{Span, SrcMgr};
use std::cell::RefCell;
use std::cmp;
use std::fmt;
use std::rc::Rc;

use colored::{Color, Colorize};

/// Severity of the diagnostic message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Remark,
    Info,
    Warning,
    Error,
    Fatal,
}

impl Severity {
    /// Get the color corresponding to this severity.
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

/// A note for detailed message or suggesting how to fix it.
pub struct Note {
    pub span: Span,
    pub fix: Option<String>,
    pub message: Option<String>,
}

/// A diagnostic message.
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    /// This is the primary span that causes the issue. This will not be displayed.
    /// `new` function will automatically add the span to notes for it to be displayed.
    pub span: Option<Span>,
    pub notes: Vec<Note>,
}

/// Helpers for building diagnostic message. Intended to be called in chains.
impl Diagnostic {
    pub fn new(severity: Severity, msg: impl Into<String>, span: Span) -> Self {
        Diagnostic {
            severity,
            message: msg.into(),
            span: Some(span),
            notes: vec![Note {
                span,
                fix: None,
                message: None,
            }],
        }
    }

    pub fn fix_primary(mut self, fix: impl Into<String>) -> Self {
        self.notes[0].fix = Some(fix.into());
        self
    }

    pub fn fix(mut self, span: Span, fix: impl Into<String>) -> Self {
        self.notes.push(Note {
            span,
            fix: Some(fix.into()),
            message: None,
        });
        self
    }
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

impl Diagnostic {
    pub fn print(&self, mgr: &SrcMgr, color: bool, tab: usize) {
        // Stringify and color severity
        let mut severity = format!("{}: ", self.severity);
        if color {
            severity = severity.color(self.severity.color()).to_string();
        }

        // Convert spans to fat spans
        let primary_span = match self.notes.first().and_then(|x| mgr.find_span(x.span)) {
            None => {
                // If the message has no associated file, just print it
                if color {
                    eprintln!("{}{}", severity.bold(), self.message.bold());
                } else {
                    eprintln!("{}{}", severity, self.message);
                }
                return;
            }
            Some(v) => v,
        };

        // Obtain line map
        let src = &primary_span.source;
        let linemap = src.linemap();

        // Get line number (starting from 0)
        let line = linemap.line_number(primary_span.start);
        // Get position within the line
        let line_start = linemap.line_start_pos(line);
        // Get source code line for handling
        let line_text = linemap.line(src, line);
        let vstr = VisualString::new(line_text, tab);

        // Get colored severity string
        // Generate the error message line
        let mut msg = format!(
            "{}:{}: {}{}",
            src.filename(),
            line + 1,
            severity,
            self.message
        );
        if color {
            msg = msg.bold().to_string();
        }

        // Allocate char vectors to hold indicators and hints
        // Make this 1 longer for possibility to point to the line break character.
        let mut indicators = vec![' '; vstr.visual_length() + 1];
        let mut fixes = vec![' '; vstr.visual_length()];
        let mut character = '^';
        let mut has_fix = false;

        // Fill in ^ and ~ characters for all spans
        for note in &self.notes {
            let span = match mgr.find_span(note.span) {
                // The span is non-existent, continue instead
                None => continue,
                Some(v) => v,
            };

            // Unlikely event, we cannot display this
            if !Rc::ptr_eq(&span.source, &primary_span.source) {
                continue;
            }

            // Get start and end position, clamped within the line.
            let start = span.start as isize - line_start as isize;
            let start_clamp = cmp::min(cmp::max(start, 0) as usize, line_text.len());
            let end = span.end as isize - line_start as isize;
            let end_clamp = cmp::min(cmp::max(end, 0) as usize, line_text.len() + 1);

            for i in vstr.visual_column(start_clamp)..vstr.visual_column(end_clamp) {
                indicators[i] = character;
            }

            // We can only display it if it partially covers this line
            if note.fix.is_some() && end >= 0 && start <= line_text.len() as isize {
                let mut vptr = cmp::min(cmp::max(start, 0) as usize, line_text.len());
                // Now replace the part in vector with the replacement suggestion
                for ch in note.fix.as_ref().unwrap().chars() {
                    if vptr >= fixes.len() {
                        fixes.push(ch);
                    } else {
                        fixes[vptr] = ch;
                    }
                    vptr += 1;
                }
                has_fix = true;
            }

            // For non-primary notes, the character is different.
            character = '~';
        }

        let mut indicator_line: String = indicators.into_iter().collect();
        if color {
            indicator_line = indicator_line.green().bold().to_string();
        }

        if has_fix {
            let mut line: String = fixes.into_iter().collect();
            if color {
                line = line.green().to_string();
            }

            eprintln!(
                "{}\n{}\n{}\n{}",
                msg,
                vstr.visual_text(),
                indicator_line,
                line
            );
        } else {
            eprintln!("{}\n{}\n{}", msg, vstr.visual_text(), indicator_line);
        }
    }
}

/// Diagnostic manager
struct DiagMgrMut {
    src: Rc<SrcMgr>,
    diagnostics: Vec<Diagnostic>,
}

pub struct DiagMgr {
    mutable: RefCell<DiagMgrMut>,
}

impl DiagMgr {
    /// Create a new diagnostics manager
    pub fn new(mgr: Rc<SrcMgr>) -> Self {
        Self {
            mutable: RefCell::new(DiagMgrMut {
                src: mgr,
                diagnostics: Vec::new(),
            }),
        }
    }

    /// Add a new diagnostic.
    pub fn report(&self, diag: Diagnostic) {
        let mut m = self.mutable.borrow_mut();
        diag.print(&m.src, true, 4);
        m.diagnostics.push(diag);
    }

    /// Create a errpr diagnostic from message and span and report it.
    pub fn report_span<M: Into<String>>(&self, severity: Severity, msg: M, span: Span) {
        self.report(Diagnostic::new(severity, msg.into(), span));
    }

    /// Create a errpr diagnostic from message and span and report it.
    pub fn report_error<M: Into<String>>(&self, msg: M, span: Span) {
        self.report(Diagnostic::new(Severity::Error, msg.into(), span));
    }

    /// Create a fatal diagnostic from message and span and report it. In addition, abort
    /// execution with a panic.
    pub fn report_fatal<M: Into<String>>(&self, msg: M, span: Span) -> ! {
        self.report(Diagnostic::new(Severity::Fatal, msg.into(), span));
        std::panic::panic_any(Severity::Fatal);
    }

    /// Clear exsting diagnostics
    pub fn clear(&self) {
        let mut m = self.mutable.borrow_mut();
        m.diagnostics.clear();
    }

    /// Check if there is any fatal error.
    pub fn has_fatal(&self) -> bool {
        let m = self.mutable.borrow();
        m.diagnostics
            .iter()
            .any(|diag| diag.severity == Severity::Fatal)
    }

    /// Check if there is any error.
    pub fn has_error(&self) -> bool {
        let m = self.mutable.borrow();
        m.diagnostics
            .iter()
            .any(|diag| diag.severity == Severity::Error || diag.severity == Severity::Fatal)
    }
}
