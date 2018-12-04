use super::{Source, Pos, Span, FatPos, FatSpan};

use std::rc::Rc;
use std::cell::RefCell;
use std::cmp;

/// The source manager. It can manage multiple source files, and track Pos & Span's actual location in
/// the file. Source manager is an append-only structure which can be safely mutated even when
/// only immutable reference is held.
struct SrcMgrMut {
    /// Files managed by this source manager
    files: Vec<Rc<Source>>,
    /// Offsets where this source end
    end: Vec<usize>,
}

impl SrcMgrMut {
    fn new() -> SrcMgrMut {
        SrcMgrMut {
            files: Vec::new(),
            end: Vec::new(),
        }
    }

    /// Add a new source file into the source manager.
    fn add_source(&mut self, src: Rc<Source>) {
        // For each source file we will extra 1 in addition to its original length.
        // this allows end-of-file to be given a proper position.
        let new_end = self.end.last().unwrap_or(&0) + src.content().len() + 1;
        self.end.push(new_end);
        self.files.push(src);
    }

    /// Find the start and end index of a source file.
    fn find_src(&self, src: &Rc<Source>) -> Option<Span> {
        for i in 0..self.files.len() {
            if Rc::ptr_eq(&self.files[i], src) {
                let start = if i == 0 { 0 } else { self.end[i - 1] };
                let end = self.end[i];
                return Some(Span(Pos(start), Pos(end)));
            }
        }
        None
    }

    /// Find the source file and offset from position. If the position is out of bound,
    /// calling find_pos will cause panic.
    fn find_pos(&self, pos: Pos) -> FatPos {
        let Pos(ipos) = pos;
        let file_id = match self.end.binary_search(&ipos) {
            // When we hit exactly, this is the start of next file
            Ok(result) => result + 1,
            // Otherwise it is in the middle of a file (or end of a file)
            Err(result) => result,
        };
        if file_id == self.end.len() {
            panic!("position out of bound");
        }
        let file_begin = if file_id == 0 { 0 } else { self.end[file_id - 1] };
        FatPos::new(self.files[file_id].clone(), ipos - file_begin)
    }

    /// Find the source file and offsets from span. Panic if span is out-of-bound, returns None
    /// if the span is a none span, and returns a span of length 1 if begin and end are not wihtin
    /// same file.
    fn find_span(&self, span: Span) -> Option<FatSpan> {
        if span.is_none() {
            return None;
        }
        let begin = self.find_pos(span.0);
        let end = self.find_pos(span.1);
        if Rc::ptr_eq(&begin.source, &end.source) {
            Some(FatSpan::new(begin.source, begin.pos, end.pos))
        } else {
            let end = cmp::min(begin.pos + 1, begin.source.content().len());
            Some(FatSpan::new(begin.source, begin.pos, end))
        }
    }
}

pub struct SrcMgr {
    mutable: RefCell<SrcMgrMut>,
}

impl SrcMgr {
    pub fn new() -> SrcMgr {
        SrcMgr {
            mutable: RefCell::new(SrcMgrMut::new()),
        }
    }

    pub fn add_source(&self, src: Rc<Source>) {
        self.mutable.borrow_mut().add_source(src);
    }

    pub fn find_src(&self, src: &Rc<Source>) -> Option<Span> {
        self.mutable.borrow().find_src(src)
    }

    pub fn find_pos(&self, pos: Pos) -> FatPos {
        self.mutable.borrow().find_pos(pos)
    }

    pub fn find_span(&self, span: Span) -> Option<FatSpan> {
        self.mutable.borrow().find_span(span)
    }
}