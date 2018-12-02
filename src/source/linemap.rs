use super::Source;

pub struct LineMap {
    lines: Vec<usize>
}

impl LineMap {
    pub fn new(src: &String) -> LineMap {
        let mut lines = Vec::new();
        lines.push(0);

        let src_bytes = src.as_bytes();
        for i in 0..src.len() {
            if src_bytes[i] == '\n' as u8 {
                lines.push(i + 1);
            }
        }

        LineMap {
            lines: lines
        }
    }

    pub fn line_number(&self, pos: usize) -> usize {
        let mut start = 0;
        let mut end = self.lines.len();

        // Binary search for line number
        while start < end {
            let mid = (start + end) / 2;
            let i = self.lines[mid];
            if pos == i {
                return mid;
            } else if pos > i {
                start = mid + 1;
            } else {
                end = mid;
            }
        }

        start - 1
    }

    pub fn line<'a>(&self, src: &'a Source, line: usize) -> &'a str {
        if line == self.lines.len() - 1 {
            &src.content_noclone()[self.lines[line]..]
        } else {
            // Strip away end-of-line character
            &src.content_noclone()[self.lines[line]..(self.lines[line + 1] - 1)]
        }
    }

    pub fn line_start_pos(&self, line: usize) -> usize {
        self.lines[line]
    }
}