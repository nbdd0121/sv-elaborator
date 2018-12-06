//! Types representing positions and spans inside source file.

use super::Source;

use std::rc::Rc;
use std::ops::{Deref, DerefMut};
use std::fmt;
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

/// Represent an object with span information
#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned {
            value: value,
            span: span,
        }
    }

    pub fn new_unspanned(value: T) -> Spanned<T> {
        Spanned {
            value: value,
            span: Span::none(),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
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
#[derive(Clone)]
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
