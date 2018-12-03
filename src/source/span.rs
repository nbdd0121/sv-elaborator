use super::Source;

use std::rc::Rc;
use std::ops::{Deref, DerefMut};
use std::fmt;

pub struct Pos {
    pub source: Rc<Source>,
    pub pos: usize,
}

impl Pos {
    pub fn new(src: Rc<Source>, pos: usize) -> Pos {
        Pos {
            source: src,
            pos: pos
        }
    }

    pub fn as_span(&self) -> Span {
        Span {
            source: self.source.clone(),
            start: self.pos,
            end: self.pos,
        }
    }
}

#[derive(Clone)]
pub struct Span {
    pub source: Rc<Source>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(src: Rc<Source>, start: usize, end: usize) -> Span {
        Span {
            source: src,
            start: start,
            end: end,
        }
    }

    pub fn start_pos(&self) -> Pos {
        Pos {
            source: self.source.clone(),
            pos: self.start,
        }
    }

    pub fn join(&self, r: &Span) -> Span {
        Span {
            source: self.source.clone(),
            start: self.start,
            end: r.end,
        }
    }
}

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
