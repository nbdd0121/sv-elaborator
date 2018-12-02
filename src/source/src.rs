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

    pub fn filename(&self) -> &String {
        &self.filename
    }

    pub fn content(&self) -> Rc<String> {
        self.content.clone()
    }

    pub fn content_noclone(&self) -> &String {
        self.content.as_ref()
    }

    pub fn linemap(&self) -> &LineMap {
        if !self.linemap.filled() {
            self.linemap.fill(LineMap::new(&self.content)).unwrap_or_else(|_| unreachable!());
        }
        self.linemap.borrow().unwrap()
    }
}

