//! Overall source file management

use super::{FatPos, FatSpan, Pos, Span};

use once_cell::unsync::OnceCell;
use std::cell::RefCell;
use std::cmp;
use std::io::{self, Read};
use std::path::PathBuf;
use std::rc::Rc;

/// Represent a single source file
pub struct Source {
    filename: String,
    content: Rc<String>,
    linemap: OnceCell<LineMap>,
}

impl Source {
    pub fn new(filename: String, content: String) -> Source {
        Source {
            filename: filename,
            content: Rc::new(content),
            linemap: OnceCell::new(),
        }
    }

    /// Create a new `Source` object with modified file name and line number. Useful for
    /// implementing `line` compiler directive.
    ///
    /// You need to specify a byte position starting from which the line number will be changed to
    /// the given line number.
    pub fn fake_source(&self, pos: usize, file: String, line: i32) -> Source {
        // Calculate bias needed and create a new linemap
        let cur_map = self.linemap();
        let cur_line = cur_map.line_number(pos);
        let line_offset = line - cur_line;
        let linemap = cur_map.clone_with_bias(line_offset);

        // Pre-populate the lazy cell
        let lazy = OnceCell::new();
        lazy.set(linemap).unwrap_or_else(|_| unreachable!());

        Source {
            filename: file,
            content: self.content.clone(),
            linemap: lazy,
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn content(&self) -> &Rc<String> {
        &self.content
    }

    pub fn linemap(&self) -> &LineMap {
        self.linemap.get_or_init(|| LineMap::new(&self.content, 0))
    }
}

pub struct LineMap {
    lines: Rc<Vec<usize>>,
    bias: i32,
}

impl LineMap {
    /// Construct a new `LineMap`.
    pub fn new(src: &str, bias: i32) -> LineMap {
        let mut lines = Vec::new();
        lines.push(0);

        let src_bytes = src.as_bytes();
        for i in 0..src.len() {
            if src_bytes[i] == '\n' as u8 {
                lines.push(i + 1);
            }
        }

        LineMap {
            lines: Rc::new(lines),
            bias: bias,
        }
    }

    /// Given a existing `LineMap`, create a copy with only bias changed.
    pub fn clone_with_bias(&self, bias: i32) -> LineMap {
        LineMap {
            lines: self.lines.clone(),
            bias: self.bias + bias,
        }
    }

    /// Given a byte position, return its corresponding line number.
    pub fn line_number(&self, pos: usize) -> i32 {
        match self.lines.binary_search(&pos) {
            Ok(v) => v as i32 + self.bias,
            Err(v) => (v - 1) as i32 + self.bias,
        }
    }

    /// Given a line number and corresponding `Source`, return the text of the line.
    /// The newline character is not included.
    pub fn line<'a>(&self, src: &'a Source, line: i32) -> &'a str {
        let offset = (line - self.bias) as usize;
        if offset == self.lines.len() - 1 {
            &src.content()[self.lines[offset]..]
        } else {
            // Strip away end-of-line character
            &src.content()[self.lines[offset]..(self.lines[offset + 1] - 1)]
        }
    }

    /// Given a line number, return its starting byte position.
    pub fn line_start_pos(&self, line: i32) -> usize {
        self.lines[(line - self.bias) as usize]
    }
}

/// The source manager. It can manage multiple source files, and track Pos & Span's actual location in
/// the file. Source manager is an append-only structure which can be safely mutated even when
/// only immutable reference is held.
struct SrcMgrMut {
    /// Files managed by this source manager
    files: Vec<Rc<Source>>,
    /// Offsets where this source end
    end: Vec<usize>,
    /// Paths to search
    search_path: Vec<PathBuf>,
}

impl SrcMgrMut {
    /// Lookup a file, load it, add it to source manager and return a Rc to it.
    fn load_source(&mut self, filename: &str) -> Result<Rc<Source>, io::Error> {
        let mut file = 'find_file: loop {
            let mut err = None;
            for path in &self.search_path {
                let newpath = path.join(filename);
                match ::std::fs::File::open(newpath) {
                    Ok(f) => break 'find_file Ok(f),
                    Err(e) => err = Some(e),
                }
            }
            break 'find_file Err(err.unwrap());
        }?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)?;

        let src = Rc::new(Source::new((*filename).to_owned(), contents));
        self.add_source(src.clone());
        Ok(src)
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
                return Some(Pos(start).span_to(Pos(end)));
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
        let file_begin = if file_id == 0 {
            0
        } else {
            self.end[file_id - 1]
        };
        FatPos::new(self.files[file_id].clone(), ipos - file_begin)
    }

    /// Find the source file and offsets from span. Panic if span is out-of-bound, returns None
    /// if the span is a none span, and returns a span of length 1 if begin and end are not wihtin
    /// same file.
    fn find_span(&self, span: Span) -> Option<FatSpan> {
        if span.is_none() {
            return None;
        }
        let begin = self.find_pos(span.start);
        let end = self.find_pos(span.end);
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
    pub fn new(search_path: Vec<PathBuf>) -> SrcMgr {
        SrcMgr {
            mutable: RefCell::new(SrcMgrMut {
                files: Vec::new(),
                end: Vec::new(),
                search_path,
            }),
        }
    }

    pub fn load_source(&self, filename: &str) -> Result<Rc<Source>, io::Error> {
        self.mutable.borrow_mut().load_source(filename)
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
