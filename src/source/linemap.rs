use super::Source;

use std::rc::Rc;

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
            bias: self.bias + bias
        }
    }

    /// Given a byte position, return its corresponding line number.
    pub fn line_number(&self, pos: usize) -> i32 {
        let mut start = 0;
        let mut end = self.lines.len();

        // Binary search for line number
        while start < end {
            let mid = (start + end) / 2;
            let i = self.lines[mid];
            if pos == i {
                return (mid as i32) + self.bias;
            } else if pos > i {
                start = mid + 1;
            } else {
                end = mid;
            }
        }

        (start - 1) as i32 + self.bias
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