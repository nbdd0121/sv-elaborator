mod kw;
mod kw_map;
mod token;

pub use self::kw::Keyword;
pub use self::token::{Token, Operator};

use self::kw_map::HASHMAP;
use super::source::{Source, DiagMsg, Severity, Pos, Span, Spanned, FixItHint};
use super::number::{LogicValue, LogicNumber};

use std::rc::Rc;
use std::cmp;
use num::{BigUint, Zero, One, Num};

pub struct Tokenizer {
    pub src: Rc<Source>,
    // Current index pointer
    pub pos: usize,
    // The source code to tokenize
    pub src_text: Rc<String>,
    // Start of current token
    pub start: usize,
    // 1, 2, 3, 4 -> Verilog 95, 01, 01-noconfig, 05
    // 5, 6, 7, 8 -> SystemVerilog 05, 09, 12, 17
    pub keyword: u8,
    pub keyword_stack: Vec<u8>,
    pub attr: bool,
}

impl Tokenizer {
    pub fn new(src: Rc<Source>) -> Tokenizer {
        Tokenizer {
            src_text: src.content().clone(),
            src: src,
            pos: 0,
            start: 0,
            keyword: 8,
            keyword_stack: Vec::new(),
            attr: false,
        }
    }

    // Character stream handling
    fn peekch(&self) -> Option<char> {
        if self.pos >= self.src_text.len() {
            None
        } else {
            Some(self.src_text[self.pos..].chars().next().unwrap())
        }
    }

    fn nextch(&mut self) -> Option<char> {
        let ret = self.peekch();
        if let Some(v) = ret {
            self.pos += v.len_utf8();
        }
        ret
    }

    fn nextch_if(&mut self, c: char) -> bool {
        let ret = self.peekch();
        if Some(c) == ret {
            self.pos += c.len_utf8();
            true
        } else {
            false
        }
    }

    fn pushback(&mut self, c: char) {
        self.pos -= c.len_utf8();
    }

    // Error reporting
    fn report_pos<M: Into<String>>(&self, severity: Severity, msg: M, pos: usize) {
        self.report_diag(DiagMsg {
            severity: severity,
            message: msg.into(),
            pos: Some(Pos::new(self.src.clone(), pos)),
            span: Vec::new(),
            hint: Vec::new()
        })
    }

    fn report_span<M: Into<String>>(&self, severity: Severity, msg: M, pos: usize, rstart: usize, rend: usize) {
        self.report_diag(DiagMsg {
            severity: severity,
            message: msg.into(),
            pos: Some(Pos::new(self.src.clone(), pos)),
            span: vec![Span::new(self.src.clone(), rstart, rend)],
            hint: Vec::new()
        })
    }

    fn report_diag(&self, msg: DiagMsg) {
        msg.print(true, 4)
    }

    // Skip CRLF (the CR is already consumed)
    fn skip_crlf(&mut self) {
        if let Some('\n') = self.peekch() {
            self.nextch();
        } else {
            self.report_pos(
                Severity::Warning,
                "found line ending with cr but without lf",
                self.pos
            );
        }
    }

    // Skip through whitespaces. We need this as SystemVerilog allows whitespaces between size,
    // base and number literals. Sigh!
    fn skip_whitspace(&mut self) {
        loop {
            let next = self.nextch();
            match next {
                None => return,
                Some(v) => match v {
                    '\r' => self.skip_crlf(),
                    ' ' | '\t' | '\x0c' | '\n' => (),
                    _ => {
                        self.pushback(v);
                        return
                    }
                }
            }
        }
    }

    // Skip line comment (the leading / is already consumed)
    fn skip_line_comment(&mut self) {
        loop {
            let next = self.nextch();
            match next {
                None => return,
                Some('\r') => {
                    self.skip_crlf();
                    return
                }
                Some('\n') => return,
                _ => (),
            }
        }
    }

    // Skip block comment (the leading / is already consumed)
    fn skip_block_comment(&mut self) {
        loop {
            let next = self.nextch();
            match next {
                None => {
                    self.report_pos(Severity::Error, "unterminated /* comment", self.start);
                    return
                },
                Some('\r') => self.skip_crlf(),
                Some('*') => {
                    if self.nextch_if('/') {
                        return
                    }
                }
                _ => (),
            }
        }
    }

    // Parse all simple identifiers, keywords and system tasks
    fn parse_identifier(&mut self, ch: char) -> Token {
        let mut name = String::new();
        name.push(ch);
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v
            };
            match next {
                'a'...'z' | 'A'...'Z' | '0'...'9' | '_' | '$' => {
                    name.push(next);
                }
                _ => {
                    self.pushback(next);
                    break
                }
            }
        }
        if ch == '$' {
            Token::SystemTask(name)
        } else {
            match HASHMAP.get::<str>(&name) {
                Some(&(kw, v)) => if v <= self.keyword { return Token::Keyword(kw) },
                _ => (),
            }
            Token::Id(name)
        }
    }

    // Parse escaped identifiers (\ is already consumed)
    fn parse_esc_id(&mut self) -> Token {
        let mut name = String::new();
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v
            };
            if next.is_ascii_graphic() {
                name.push(next);
            } else {
                self.pushback(next);
                break
            }
        }
        Token::Id(name)
    }

    // Parse escape sequence (\ is already consumed)
    fn parse_escape_seq(&mut self) -> Option<char> {
        let next = match self.nextch() {
            None => {
                self.report_pos(Severity::Error, "string literal is not terminated", self.start);
                return None
            }
            Some(v) => v
        };
        match next {
            // SystemVerilog spec does not mention about this, so
            // this is our extension.
            '\r' => {
                self.skip_crlf();
                None
            }
            '\n' => None,
            'r' => Some('\r'),
            'n' => Some('\n'),
            '\\' => Some('\\'),
            '"' => Some('"'),
            'v' => Some(11 as char),
            'f' => Some(12 as char),
            'a' => Some(7 as char),
            // Octal escape sequence
            '0' ... '7' => {
                self.pushback(next);
                let mut codepoint: u8 = 0;
                for _ in 0..3 {
                    let digit = match self.nextch() {
                        None => break,
                        Some(v) => v
                    };
                    match digit.to_digit(8) {
                        Some(v) => codepoint = codepoint * 8 + v as u8,
                        _ => {
                            self.pushback(digit);
                            break
                        }
                    }
                }
                Some(codepoint as char)
            }
            // Hexadecimal escape sequence
            'x' => {
                let start = self.pos;
                let mut codepoint: u8 = 0;
                for _ in 0..2 {
                    let digit = self.peekch().and_then(|v| v.to_digit(16));
                    match digit {
                        None => {
                            self.report_span(
                                Severity::Error,
                                "\\x should be followed by two hex digits",
                                self.pos,
                                start - 2,
                                self.pos
                            );
                            return None
                        }
                        Some(v) => {
                            self.nextch();
                            codepoint = codepoint * 16 + v as u8
                        }
                    }
                }
                Some(codepoint as char)
            }
            _ => {
                let span = Span::new(self.src.clone(), self.pos - 2, self.pos);
                self.report_diag(DiagMsg {
                    severity: Severity::Error,
                    message: format!("unknown escape sequence '{0}'; do you want to literally represent '\\{0}'?", next),
                    pos: Some(Pos::new(self.src.clone(), self.pos - 1)),
                    span: vec![span.clone()],
                    hint: vec![FixItHint::new(span, format!("\\\\{}", next))],
                });
                None
            }
        }
    }

    // Parse string (" is already consumed)
    fn parse_string(&mut self) -> Token {
        let mut content = String::new();
        loop {
            let next = match self.nextch() {
                None => {
                    self.report_pos(Severity::Error, "string literal is not terminated", self.start);
                    break
                }
                Some(v) => v
            };
            match next {
                '"' => break,
                '\r' | '\n' => {
                    self.report_pos(Severity::Error, "string literal is not terminated", self.start);
                    break
                },
                '\\' => match self.parse_escape_seq() {
                    None => continue,
                    Some(v) => content.push(v)
                }
                _ => content.push(next)
            }
        }
        Token::StringLiteral(content)
    }

    // Parse decimals. Report error when X and Zs are encountered, and discard all _ characters.
    fn parse_decimal(&mut self) -> String {
        let mut str = String::new();
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v,
            };
            match next {
                '_' => (),
                '0' ... '9' => {
                    str.push(next);
                },
                'x' | 'X' | 'z' | 'Z' => {
                    self.report_pos(
                        Severity::Error,
                        "X or Zs are not allowed when base is decimal",
                        self.pos - 1
                    );
                    str.push('0');
                }
                _ => {
                    self.pushback(next);
                    break
                }
            }
        }
        str
    }

    // Parse based number (assume ' is already consumed).
    fn parse_based_number(&mut self) -> LogicNumber {
        // Parse sign
        let signed = match self.peekch() {
            Some('s') | Some('S') => {
                self.nextch();
                true
            }
            _ => false
        };

        // Parse radix
        let (radix, maxch, log2) = match self.peekch() {
            Some('d') | Some('D') => (10, '9', 0),
            Some('b') | Some('B') => (2, '1', 1),
            Some('o') | Some('O') => (8, '7', 2),
            Some('h') | Some('H') => (16, 'f', 4),
            _ => {
                self.report_pos(
                    Severity::Error,
                    "missing base specifier",
                    self.pos,
                );

                // Error recovery, treat it as zero
                return LogicNumber {
                    width: 1,
                    sized: false,
                    signed: signed,
                    value: BigUint::zero(),
                    xz: BigUint::zero(),
                }
            }
        };
        self.nextch();

        // We will need to skip over whitespaces for this task
        let pos = self.pos;
        self.skip_whitspace();

        let num_after_ws = match self.peekch() {
            None => false,
            Some(v) => match v {
                '0' ... '9' | 'a' ... 'f' | 'A' ... 'F' | 'x' | 'X' | 'z' | 'Z' => true,
                // TODO: If + or - is specified here, probably give a better suggestion
                _ => false
            }
        };

        if !num_after_ws {
            self.pos = pos;
            self.report_pos(
                Severity::Error,
                "missing digits after base specifier",
                pos,
            );

            // Error recovery, treat it as zero
            return LogicNumber {
                width: 1,
                sized: false,
                signed: signed,
                value: BigUint::zero(),
                xz: BigUint::zero(),
            }
        }

        // Radix 10 is special: no X or Z allowed it other digits are present
        if radix == 10 {
            return match self.peekch().unwrap() {
                'x' | 'X' => {
                    self.nextch();
                    // Consume extra _ if there are any
                    while self.nextch_if('_') {}
                    LogicNumber {
                        width: 1,
                        sized: false,
                        signed: signed,
                        value: BigUint::one(),
                        xz: BigUint::one(),
                    }
                }
                'z' | 'Z' => {
                    self.nextch();
                    // Consume extra _ if there are any
                    while self.nextch_if('_') {}
                    LogicNumber {
                        width: 1,
                        sized: false,
                        signed: signed,
                        value: BigUint::zero(),
                        xz: BigUint::one(),
                    }
                }
                '0' ... '9' => {
                    let str = self.parse_decimal();
                    let num = BigUint::from_str_radix(&str, 10).unwrap();
                    LogicNumber {
                        width: cmp::min(num.bits(), 1),
                        sized: false,
                        signed: signed,
                        value: num,
                        xz: BigUint::zero(),
                    }
                }
                _ => unreachable!(),
            }
        }

        let mut str = String::new();
        let mut xz = String::new();
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v,
            };
            match next {
                '_' => (),
                '0' ... '9' | 'a' ... 'f' | 'A' ... 'F' => {
                    if !next.is_digit(radix) {
                        self.report_pos(
                            Severity::Error,
                            format!("digit '{}' is too large for radix {}", next, radix),
                            self.pos - 1,
                        );
                        str.push('0');
                        xz.push('0');
                    } else {
                        str.push(next);
                        xz.push('0');
                    }
                },
                'x' | 'X' => {
                    str.push(maxch);
                    xz.push(maxch);
                }
                'z' | 'Z' => {
                    str.push('0');
                    xz.push(maxch);
                }
                _ => {
                    self.pushback(next);
                    break
                }
            }
        }

        return LogicNumber {
            width: str.len() * log2,
            sized: false,
            signed: signed,
            value: BigUint::from_str_radix(&str, radix).unwrap(),
            xz: BigUint::from_str_radix(&xz, radix).unwrap(),
        }
    }

    // Try parse time literal suffix. Returns the parsed time unit in terms of seconds
    fn try_parse_time_unit(&mut self) -> Option<f64> {
        let ch = match self.nextch() {
            None => return None,
            Some('s') => return Some(1e0),
            Some(v) => v,
        };
        if !self.nextch_if('s') {
            self.pushback(ch);
            return None;
        }
        match ch {
            'm' => Some(1e-3),
            'u' => Some(1e-6),
            'n' => Some(1e-9),
            'p' => Some(1e-12),
            'f' => Some(1e-15),
            _ => {
                self.pushback('s');
                self.pushback(ch);
                None
            }
        }
    }

    // Parse when we encountered a digit. This can either be an integer without base specifier or
    // a real number.
    fn parse_number(&mut self) -> Token {
        let start = self.pos;
        let mut str = self.parse_decimal();

        if self.nextch_if('.') {
            // Check if the thing after dot is a digit. SystemVerilog does not allow a trailing dot
            // to represent .0
            let num_after_dot = match self.peekch() {
                None => false,
                Some(v) => match v {
                    '0' ... '9' | 'x' | 'X' | 'z' | 'Z' => true,
                    _ => false
                }
            };

            if !num_after_dot {
                let span = Span::new(self.src.clone(), start, self.pos);
                self.report_diag(DiagMsg {
                    severity: Severity::Error,
                    message: "no digit after dot in real number literal; do you want to mean '.0'?".to_owned(),
                    pos: Some(Pos::new(self.src.clone(), self.pos)),
                    span: vec![span.clone()],
                    hint: vec![FixItHint::new(span, format!("{}0", &self.src_text[start..self.pos]))],
                });
                return Token::RealLiteral(str.parse::<f64>().unwrap())
            }

            str.push('.');
            str.push_str(&self.parse_decimal());

            // with exponent part
            let mut has_exp = false;
            if self.nextch_if('e') || self.nextch_if('E') {
                has_exp = true;
                str.push('e');

                if self.nextch_if('+') {}
                else if self.nextch_if('-') {
                    str.push('-');
                }

                // Check for exponent's existence
                let num_after_exp = match self.peekch() {
                    None => false,
                    Some(v) => match v {
                        '0' ... '9' | 'x' | 'X' | 'z' | 'Z' => true,
                        _ => false
                    }
                };

                if !num_after_exp {
                    self.report_span(
                        Severity::Error,
                        "expected exponent in real number literal",
                        self.pos, start, self.pos,
                    );

                    // Error recovery: assume exponent part is actually 0
                    str.push('0');
                    return Token::RealLiteral(str.parse::<f64>().unwrap())
                }

                str.push_str(&self.parse_decimal());
            }

            // Parse time unit
            let parsed: f64 = str.parse().unwrap();
            let index_before_time = self.pos;
            match self.try_parse_time_unit() {
                Some(v) => {
                    if has_exp {
                        let span = Span::new(self.src.clone(), start, index_before_time);
                        self.report_diag(DiagMsg {
                            severity: Severity::Error,
                            message: "time unit can only be applied to fixed point literal".to_owned(),
                            pos: Some(Pos::new(self.src.clone(), self.pos - 1)),
                            span: vec![span.clone()],
                            hint: vec![FixItHint::new(span, format!("{}", parsed))],
                        });
                    }
                    return Token::TimeLiteral(parsed * v)
                }
                None => (),
            }

            return Token::RealLiteral(parsed)
        }

        // Parsing time unit
        match self.try_parse_time_unit() {
            Some(v) => {
                return Token::TimeLiteral(str.parse::<f64>().unwrap() * v)
            }
            None => (),
        }

        let size_pos = self.pos;
        self.skip_whitspace();
        if self.nextch_if('\'') {
            // Check if we're beginning a based integer literal
            let has_base = match self.peekch() {
                None => false,
                Some(v) => match v {
                    's' | 'S' | 'd' | 'D' | 'b' | 'B' | 'o' | 'O' | 'h' | 'H' => true,
                    _ => false,
                }
            };

            if has_base {
                let mut size: usize = str.parse().unwrap();

                // The spec says not only the size cannot be zero, it cannot begin with zero
                if str.as_bytes()[0] == b'0' {
                    if size == 0 {
                        self.report_span(
                            Severity::Error,
                            "size specifier cannot be zero",
                            start, start, size_pos
                        );

                        // Error recovery
                        size = 1;
                    } else {
                        // This is a separate diagnostics since we have a clue about how to fix.
                        let span = Span::new(self.src.clone(), start, size_pos);
                        self.report_diag(DiagMsg {
                            severity: Severity::Error,
                            message: "size specifier cannot begin with zero".to_owned(),
                            pos: Some(Pos::new(self.src.clone(), start)),
                            span: vec![span.clone()],
                            hint: vec![FixItHint::new(span, format!("{}", size))],
                        });
                    }
                }

                let mut num = self.parse_based_number();
                num.x_extend(size);
                num.sized = true;
                return Token::IntegerLiteral(num);
            }

            // This is only a number, restore position
            self.pos = size_pos;
        }

        let num: BigUint = str.parse().unwrap();
        Token::IntegerLiteral(LogicNumber {
            width: cmp::min(num.bits(), 32),
            sized: false,
            signed: true,
            value: num,
            xz: BigUint::zero()
        })
    }

    pub fn next_tk(&mut self) -> Token {
        self.start = self.pos;

        // Early return if result is EOF
        let ch = match self.nextch() {
            None => return Token::Eof,
            Some(v) => v
        };

        match ch {
            // Whitespaces
            ' ' | '\t' => Token::Whitespace,
            // Line terminators
            '\r' => {
                self.skip_crlf();
                Token::NewLine
            }
            '\n' => Token::NewLine,
            // Comments
            '/' => {
                match self.peekch() {
                    Some('/') => {
                        self.skip_line_comment();
                        Token::LineComment
                    }
                    Some('*') =>{
                        self.skip_block_comment();
                        Token::BlockComment
                    }
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::DivEq)
                    }
                    _ => Token::Operator(Operator::Div)
                }
            }
            // Identifiers
            'a'...'z' | 'A'...'Z' | '_' | '$' => {
                self.parse_identifier(ch)
            }
            '\\' => {
                self.parse_esc_id()
            }
            '`' => {
                self.report_pos(Severity::Warning, "compiler directive not yet supported", self.start);
                Token::Whitespace
            }
            // Literals
            '0' ... '9' => {
                self.pushback(ch);
                self.parse_number()
            }
            '\'' => {
                match self.peekch().unwrap_or(' ') {
                    's' | 'S' | 'd' | 'D' | 'b' | 'B' | 'o' | 'O' | 'h' | 'H' => {
                        Token::IntegerLiteral(self.parse_based_number())
                    }
                    '{' => {
                        self.nextch();
                        Token::Operator(Operator::TickBrace)
                    }
                    '0' => Token::UnbasedLiteral(LogicValue::Zero),
                    '1' => Token::UnbasedLiteral(LogicValue::One),
                    'z' | 'Z' => Token::UnbasedLiteral(LogicValue::Z),
                    'x' | 'X' => Token::UnbasedLiteral(LogicValue::X),
                    _ => Token::Operator(Operator::Tick)
                }
            }
            '"' => self.parse_string(),
            // Delimiters
            '(' => {
                if self.nextch_if('*') {
                    if self.attr {
                        self.report_span(Severity::Error, "attribute (* cannot be nested", self.start, self.start, self.pos);
                    }
                    self.attr = true;
                    Token::Operator(Operator::OpenAttr)
                } else {
                    Token::Operator(Operator::OpenParen)
                }
            }
            ')' => Token::Operator(Operator::CloseParen),
            '[' => Token::Operator(Operator::OpenBracket),
            ']' => Token::Operator(Operator::CloseBracket),
            '{' => Token::Operator(Operator::OpenBrace),
            '}' => Token::Operator(Operator::CloseBrace),
            // Operators
            '+' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::AddEq)
                    }
                    Some('+') => {
                        self.nextch();
                        Token::Operator(Operator::Inc)
                    }
                    Some(':') => {
                        self.nextch();
                        Token::Operator(Operator::PlusColon)
                    }
                    _ => Token::Operator(Operator::Add)
                }
            }
            '-' => {
                match self.peekch() {
                    Some(':') => {
                        self.nextch();
                        Token::Operator(Operator::MinusColon)
                    }
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::SubEq)
                    }
                    Some('-') => {
                        self.nextch();
                        Token::Operator(Operator::Dec)
                    }
                    Some('>') => {
                        self.nextch();
                        if self.nextch_if('>') {
                            Token::Operator(Operator::NonblockTrigger)
                        } else {
                            Token::Operator(Operator::Implies)
                        }
                    }
                    _ => Token::Operator(Operator::Sub)
                }
            }
            '*' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::MulEq)
                    }
                    Some('*') => {
                        self.nextch();
                        Token::Operator(Operator::Power)
                    }
                    Some('>') => {
                        self.nextch();
                        Token::Operator(Operator::FullConnect)
                    }
                    Some(')') => {
                        self.nextch();
                        if !self.attr {
                            self.report_span(Severity::Error, "attribute *) without corresponding (*", self.start, self.start, self.pos);
                        }
                        self.attr = false;
                        Token::Operator(Operator::CloseAttr)
                    }
                    _ => Token::Operator(Operator::Mul)
                }
            }
            '%' => {
                if self.nextch_if('=') {
                    Token::Operator(Operator::ModEq)
                } else {
                    Token::Operator(Operator::Mod)
                }
            }
            '&' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::AndEq)
                    }
                    Some('&') => {
                        self.nextch();
                        if self.nextch_if('&') {
                            Token::Operator(Operator::TripleAnd)
                        } else {
                            Token::Operator(Operator::LAnd)
                        }
                    }
                    _ => Token::Operator(Operator::And)
                }
            }
            '^' => {
                if self.nextch_if('|') {
                    Token::Operator(Operator::Xnor)
                } else {
                    Token::Operator(Operator::Xor)
                }
            }
            '|' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        if self.nextch_if('>') {
                            Token::Operator(Operator::NonOverlapImply)
                        } else {
                            Token::Operator(Operator::OrEq)
                        }
                    }
                    Some('|') => {
                        self.nextch();
                        Token::Operator(Operator::LOr)
                    }
                    Some('-') => {
                        self.nextch();
                        if self.nextch_if('>') {
                            Token::Operator(Operator::OverlapImply)
                        } else {
                            self.pushback('-');
                            Token::Operator(Operator::Or)
                        }
                    }
                    _ => Token::Operator(Operator::Or)
                }
            }
            '=' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        match self.peekch() {
                            Some('=') => {
                                self.nextch();
                                Token::Operator(Operator::CaseEq)
                            }
                            Some('?') => {
                                self.nextch();
                                Token::Operator(Operator::WildEq)
                            }
                            _ => Token::Operator(Operator::Eq)
                        }
                    }
                    Some('>') => {
                        self.nextch();
                        Token::Operator(Operator::ParConnect)
                    }
                    _ => Token::Operator(Operator::Assign)
                }
            }
            '!' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        match self.peekch() {
                            Some('=') => {
                                self.nextch();
                                Token::Operator(Operator::CaseNeq)
                            }
                            Some('?') => {
                                self.nextch();
                                Token::Operator(Operator::WildNeq)
                            }
                            _ => Token::Operator(Operator::Neq)
                        }
                    }
                    _ => Token::Operator(Operator::LNot)
                }
            }
            '~' => {
                match self.peekch() {
                    Some('&') => {
                        self.nextch();
                        Token::Operator(Operator::Nand)
                    }
                    Some('|') => {
                        self.nextch();
                        Token::Operator(Operator::Nor)
                    }
                    Some('^') => {
                        self.nextch();
                        Token::Operator(Operator::Xnor)
                    }
                    _ => Token::Operator(Operator::Not)
                }
            }
            '#' => {
                if self.nextch_if('#') {
                    Token::Operator(Operator::CycleDelay)
                } else {
                    Token::Operator(Operator::Hash)
                }
            }
            ',' => Token::Operator(Operator::Comma),
            '.' => {
                if self.nextch_if('*') {
                    Token::Operator(Operator::WildPattern)
                } else {
                    Token::Operator(Operator::Dot)
                }
            }
            ':' => {
                match self.peekch() {
                    Some(':') => {
                        self.nextch();
                        Token::Operator(Operator::ScopeSep)
                    }
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::DistEq)
                    }
                    Some('/') => {
                        self.nextch();
                        Token::Operator(Operator::DistDiv)
                    }
                    _ => Token::Operator(Operator::Colon)
                }
            }
            ';' => Token::Operator(Operator::Semicolon),
            '<' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::Leq)
                    }
                    Some('<') => {
                        self.nextch();
                        match self.peekch() {
                            Some('<') => {
                                self.nextch();
                                if self.nextch_if('=') {
                                    Token::Operator(Operator::AShlEq)
                                } else {
                                    Token::Operator(Operator::AShl)
                                }
                            }
                            Some('=') => {
                                self.nextch();
                                Token::Operator(Operator::LShlEq)
                            }
                            _ => Token::Operator(Operator::LShl)
                        }
                    }
                    Some('-') => {
                        self.nextch();
                        if self.nextch_if('>') {
                            Token::Operator(Operator::Equiv)
                        } else {
                            self.pushback('-');
                            Token::Operator(Operator::Lt)
                        }
                    }
                    _ => Token::Operator(Operator::Lt)
                }
            }
            '>' => {
                match self.peekch() {
                    Some('=') => {
                        self.nextch();
                        Token::Operator(Operator::Geq)
                    }
                    Some('>') => {
                        self.nextch();
                        match self.peekch() {
                            Some('>') => {
                                self.nextch();
                                if self.nextch_if('=') {
                                    Token::Operator(Operator::AShrEq)
                                } else {
                                    Token::Operator(Operator::AShr)
                                }
                            }
                            Some('=') => {
                                self.nextch();
                                Token::Operator(Operator::LShrEq)
                            }
                            _ => Token::Operator(Operator::LShr)
                        }
                    }
                    _ => Token::Operator(Operator::Gt)
                }
            }
            '?' => Token::Operator(Operator::Question),
            '@' => Token::Operator(Operator::At),
            _ => {
                self.report_pos(Severity::Error, "unknown character in source file", self.start);
                Token::Unknown
            }
        }
    }

    pub fn next_span(&mut self) -> Spanned<Token> {
        loop {
            let tok = self.next_tk();
            match tok {
                Token::Whitespace |
                Token::NewLine |
                Token::LineComment |
                Token::BlockComment => continue,
                _ => ()
            }
            return Spanned::new(tok, Span::new(self.src.clone(), self.start, self.pos));
        }
    }
}
