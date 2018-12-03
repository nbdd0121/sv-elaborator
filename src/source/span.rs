use super::Source;

use std::rc::Rc;

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
}
