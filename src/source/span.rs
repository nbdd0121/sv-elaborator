use super::Source;

use std::rc::Rc;
use std::ops::{Deref, DerefMut};
use std::fmt;
use std::usize;

/// Represent a unique position within all source managed files.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Pos(pub usize);

/// Represent a span within all source managed files.
#[derive(Clone, Copy)]
pub struct Span(pub Pos, pub Pos);

impl Span {
    /// Create a dummy span.
    pub fn none() -> Span {
        Span(Pos(usize::MAX), Pos(usize::MAX))
    }

    /// Check if this span is meaningful.
    pub fn is_none(&self) -> bool {
        let Span(Pos(a), _) = self;
        *a == usize::MAX
    }

    /// Join with another span
    pub fn join(&self, span: Span) -> Span {
        Span(self.0, span.1)
    }
}

/// Represent an object with span information
#[derive(Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Spanned<T> {
        Spanned {
            node: node,
            span: span,
        }
    }

    pub fn new_unspanned(node: T) -> Spanned<T> {
        Spanned {
            node: node,
            span: Span::none(),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.node
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.node
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.node.fmt(f)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.node.fmt(f)
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
