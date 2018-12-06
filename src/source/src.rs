use super::{LineMap};

use std::rc::Rc;
use lazycell::LazyCell;

pub struct Source {
    filename: String,
    content: Rc<String>,
    linemap: LazyCell<LineMap>,
}

impl Source {
    pub fn new(filename: String, content: String) -> Source {
        Source {
            filename: filename,
            content: Rc::new(content),
            linemap: LazyCell::new()
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
        let lazy = LazyCell::new();
        lazy.fill(linemap).unwrap_or_else(|_| unreachable!());

        Source {
            filename: file,
            content: self.content.clone(),
            linemap: lazy
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    pub fn content(&self) -> &Rc<String> {
        &self.content
    }

    pub fn linemap(&self) -> &LineMap {
        if !self.linemap.filled() {
            self.linemap.fill(LineMap::new(&self.content, 0)).unwrap_or_else(|_| unreachable!());
        }
        self.linemap.borrow().unwrap()
    }
}

