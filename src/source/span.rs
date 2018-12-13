//! Types representing positions and spans inside source file.

use super::Source;

use std::rc::Rc;
use std::cmp;
use std::usize;

/// Represent a unique position within all source managed files.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos(pub usize);

impl Pos {
    /// Create a span between current position and target
    pub fn span_to(self, to: Pos) -> Span {
        Span::new(self, to)
    }
}

/// Represent a span within all source managed files.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos
}

impl Span {
    /// Creata a new span
    pub fn new(a: Pos, b: Pos) -> Span {
        Span {
            start: a, 
            end: b,
        }
    }

    /// Create a dummy span.
    pub fn none() -> Span {
        Span {
            start: Pos(usize::MAX),
            end: Pos(usize::MAX),
        }
    }

    /// Check if this span is meaningful.
    pub fn is_none(&self) -> bool {
        self.start.0 == usize::MAX
    }

    /// Merge with another span
    pub fn merge(self, other: Span) -> Span {
        let start = cmp::min(self.start.0, other.start.0);
        let end = cmp::max(self.end.0, other.end.0);
        Span::new(Pos(start), Pos(end))
    }
}

/// Represent a position within a single source file.
pub struct FatPos {
    pub source: Rc<Source>,
    pub pos: usize,
}

impl FatPos {
    pub fn new(src: Rc<Source>, pos: usize) -> FatPos {
        FatPos {
            source: src,
            pos: pos
        }
    }
}

/// Represent a span within a single source file.
pub struct FatSpan {
    pub source: Rc<Source>,
    pub start: usize,
    pub end: usize,
}

impl FatSpan {
    pub fn new(src: Rc<Source>, start: usize, end: usize) -> FatSpan {
        FatSpan {
            source: src,
            start: start,
            end: end,
        }
    }
}
