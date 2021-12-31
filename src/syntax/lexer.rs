use super::ast::*;
use super::tokens::*;

use super::super::number::{LogicNumber, LogicValue, LogicVec};
use super::super::source::{DiagMgr, Diagnostic, Pos, Severity, Source, SrcMgr};
use super::kw_map::HASHMAP;

use num::{BigUint, Num, One, Zero};

use std::cmp;
use std::rc::Rc;

pub struct Lexer<'a> {
    mgr: &'a SrcMgr,
    diag: &'a DiagMgr,
    src_offset: Pos,
    // Current index pointer
    pos: usize,
    // The source code to tokenize
    src_text: Rc<String>,
    // Start of current token
    start: usize,
    // 0 -> nothing should be treated as keyword
    // 1, 2, 3, 4 -> Verilog 95, 01, 01-noconfig, 05
    // 5, 6, 7, 8 -> SystemVerilog 05, 09, 12, 17
    keyword: u8,
    keyword_stack: Vec<u8>,
    attr: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(mgr: &'a SrcMgr, diag: &'a DiagMgr, src: &Rc<Source>) -> Lexer<'a> {
        Lexer {
            diag,
            src_offset: mgr.find_src(src).unwrap().start,
            src_text: src.content().clone(),
            mgr: mgr,
            pos: 0,
            start: 0,
            keyword: 8,
            keyword_stack: Vec::new(),
            attr: false,
        }
    }

    /// Enter a new keyword scope
    pub fn enter_kw_scope(&mut self, v: u8) {
        self.keyword_stack.push(self.keyword);
        self.keyword = v;
    }

    /// Leave a keyword scope
    pub fn leave_kw_scope(&mut self) {
        self.keyword = self.keyword_stack.pop().unwrap();
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
        self.report_span(severity, msg, pos, pos + 1)
    }

    fn report_span<M: Into<String>>(&self, severity: Severity, msg: M, start: usize, end: usize) {
        self.report_diag(Diagnostic::new(
            severity,
            msg.into(),
            Pos(self.src_offset.0 + start).span_to(Pos(self.src_offset.0 + end)),
        ));
    }

    fn report_span_with_hint<M: Into<String>>(
        &self,
        severity: Severity,
        msg: M,
        hint: String,
        start: usize,
        end: usize,
    ) {
        self.report_diag(
            Diagnostic::new(
                severity,
                msg.into(),
                Pos(self.src_offset.0 + start).span_to(Pos(self.src_offset.0 + end)),
            )
            .fix_primary(hint),
        );
    }

    fn report_diag(&self, msg: Diagnostic) {
        self.diag.report(msg)
    }

    // Skip CRLF (the CR is already consumed)
    fn skip_crlf(&mut self) {
        if let Some('\n') = self.peekch() {
            self.nextch();
        } else {
            self.report_pos(
                Severity::Warning,
                "found line ending with cr but without lf",
                self.pos,
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
                        return;
                    }
                },
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
                    return;
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
                    return;
                }
                Some('\r') => self.skip_crlf(),
                Some('*') => {
                    if self.nextch_if('/') {
                        return;
                    }
                }
                _ => (),
            }
        }
    }

    // Parse all simple identifiers, keywords and system tasks
    fn parse_identifier(&mut self) -> String {
        let mut name = String::new();
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v,
            };
            match next {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '$' => {
                    name.push(next);
                }
                _ => {
                    self.pushback(next);
                    break;
                }
            }
        }
        name
    }

    // Parse escaped identifiers (\ is already consumed)
    fn parse_esc_id(&mut self) -> String {
        let mut name = String::new();
        loop {
            let next = match self.nextch() {
                None => break,
                Some(v) => v,
            };
            if next.is_ascii_graphic() {
                name.push(next);
            } else {
                self.pushback(next);
                break;
            }
        }
        name
    }

    // Parse escape sequence (\ is already consumed)
    fn parse_escape_seq(&mut self) -> Option<char> {
        let next = match self.nextch() {
            None => {
                self.report_pos(
                    Severity::Error,
                    "string literal is not terminated",
                    self.start,
                );
                return None;
            }
            Some(v) => v,
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
            '0'..='7' => {
                self.pushback(next);
                let mut codepoint: u8 = 0;
                for _ in 0..3 {
                    let digit = match self.nextch() {
                        None => break,
                        Some(v) => v,
                    };
                    match digit.to_digit(8) {
                        Some(v) => codepoint = codepoint * 8 + v as u8,
                        _ => {
                            self.pushback(digit);
                            break;
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
                                start - 2,
                                self.pos + 1,
                            );
                            return None;
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
                self.report_span_with_hint(
                    Severity::Error,
                    format!("unknown escape sequence '{0}'; do you want to literally represent '\\{0}'?", next),
                    format!("\\\\{}", next),
                    self.pos - 2,
                    self.pos,
                );
                None
            }
        }
    }

    // Parse string (" is already consumed)
    fn parse_string(&mut self) -> TokenKind {
        let mut content = String::new();
        loop {
            let next = match self.nextch() {
                None => {
                    self.report_pos(
                        Severity::Error,
                        "string literal is not terminated",
                        self.start,
                    );
                    break;
                }
                Some(v) => v,
            };
            match next {
                '"' => break,
                '\r' | '\n' => {
                    self.report_pos(
                        Severity::Error,
                        "string literal is not terminated",
                        self.start,
                    );
                    break;
                }
                '\\' => match self.parse_escape_seq() {
                    None => continue,
                    Some(v) => content.push(v),
                },
                _ => content.push(next),
            }
        }
        TokenKind::StringLiteral(content)
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
                '0'..='9' => {
                    str.push(next);
                }
                'x' | 'X' | 'z' | 'Z' | '?' => {
                    self.report_pos(
                        Severity::Error,
                        "X or Zs are not allowed when base is decimal",
                        self.pos - 1,
                    );
                    str.push('0');
                }
                _ => {
                    self.pushback(next);
                    break;
                }
            }
        }
        str
    }

    // Parse based number (assume ' is already consumed).
    fn parse_based_number(&mut self) -> LogicVec {
        // Parse sign
        let signed = match self.peekch() {
            Some('s') | Some('S') => {
                self.nextch();
                true
            }
            _ => false,
        };

        // Parse radix
        let (radix, maxch, log2) = match self.peekch() {
            Some('d') | Some('D') => (10, '9', 0),
            Some('b') | Some('B') => (2, '1', 1),
            Some('o') | Some('O') => (8, '7', 2),
            Some('h') | Some('H') => (16, 'f', 4),
            _ => {
                self.report_pos(Severity::Error, "missing base specifier", self.pos);

                // Error recovery, treat it as zero
                return LogicVec::new_xz(1, signed, BigUint::zero(), BigUint::zero());
            }
        };
        self.nextch();

        // We will need to skip over whitespaces for this task
        let pos = self.pos;
        self.skip_whitspace();

        let num_after_ws = match self.peekch() {
            None => false,
            Some(v) => match v {
                '0'..='9' | 'a'..='f' | 'A'..='F' | 'x' | 'X' | 'z' | 'Z' | '?' => true,
                // TODO: If + or - is specified here, probably give a better suggestion
                _ => false,
            },
        };

        if !num_after_ws {
            self.pos = pos;
            self.report_pos(Severity::Error, "missing digits after base specifier", pos);

            // Error recovery, treat it as zero
            return LogicVec::new_xz(1, signed, BigUint::zero(), BigUint::zero());
        }

        // Radix 10 is special: no X or Z allowed it other digits are present
        if radix == 10 {
            return match self.peekch().unwrap() {
                'x' | 'X' => {
                    self.nextch();
                    // Consume extra _ if there are any
                    while self.nextch_if('_') {}
                    LogicVec::new_xz(1, signed, BigUint::one(), BigUint::one())
                }
                'z' | 'Z' | '?' => {
                    self.nextch();
                    // Consume extra _ if there are any
                    while self.nextch_if('_') {}
                    LogicVec::new_xz(1, signed, BigUint::zero(), BigUint::one())
                }
                '0'..='9' => {
                    let str = self.parse_decimal();
                    let num = BigUint::from_str_radix(&str, 10).unwrap();
                    LogicVec::new_xz(
                        cmp::max(num.bits() as usize, 1),
                        signed,
                        num,
                        BigUint::zero(),
                    )
                }
                _ => unreachable!(),
            };
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
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
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
                }
                'x' | 'X' => {
                    str.push(maxch);
                    xz.push(maxch);
                }
                'z' | 'Z' | '?' => {
                    str.push('0');
                    xz.push(maxch);
                }
                _ => {
                    self.pushback(next);
                    break;
                }
            }
        }

        return LogicVec::new_xz(
            str.len() * log2,
            signed,
            BigUint::from_str_radix(&str, radix).unwrap(),
            BigUint::from_str_radix(&xz, radix).unwrap(),
        );
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
    fn parse_number(&mut self) -> TokenKind {
        let start = self.pos;
        let mut str = self.parse_decimal();

        if self.nextch_if('.') {
            // Check if the thing after dot is a digit. SystemVerilog does not allow a trailing dot
            // to represent .0
            let num_after_dot = match self.peekch() {
                None => false,
                Some(v) => match v {
                    '0'..='9' | 'x' | 'X' | 'z' | 'Z' => true,
                    _ => false,
                },
            };

            if !num_after_dot {
                self.report_span_with_hint(
                    Severity::Error,
                    "no digit after dot in real number literal; do you want to mean '.0'?",
                    format!("{}0", &self.src_text[start..self.pos]),
                    start,
                    self.pos,
                );
                return TokenKind::RealLiteral(str.parse::<f64>().unwrap());
            }

            str.push('.');
            str.push_str(&self.parse_decimal());

            // with exponent part
            let mut has_exp = false;
            if self.nextch_if('e') || self.nextch_if('E') {
                has_exp = true;
                str.push('e');

                if self.nextch_if('+') {
                } else if self.nextch_if('-') {
                    str.push('-');
                }

                // Check for exponent's existence
                let num_after_exp = match self.peekch() {
                    None => false,
                    Some(v) => match v {
                        '0'..='9' | 'x' | 'X' | 'z' | 'Z' => true,
                        _ => false,
                    },
                };

                if !num_after_exp {
                    self.report_span(
                        Severity::Error,
                        "expected exponent in real number literal",
                        start,
                        self.pos + 1,
                    );

                    // Error recovery: assume exponent part is actually 0
                    str.push('0');
                    return TokenKind::RealLiteral(str.parse::<f64>().unwrap());
                }

                str.push_str(&self.parse_decimal());
            }

            // Parse time unit
            let parsed: f64 = str.parse().unwrap();
            let index_before_time = self.pos;
            match self.try_parse_time_unit() {
                Some(v) => {
                    if has_exp {
                        self.report_span_with_hint(
                            Severity::Error,
                            "time unit can only be applied to fixed point literal",
                            format!("{}", parsed),
                            start,
                            index_before_time,
                        );
                    }
                    return TokenKind::TimeLiteral(parsed * v);
                }
                None => (),
            }

            return TokenKind::RealLiteral(parsed);
        }

        // Parsing time unit
        match self.try_parse_time_unit() {
            Some(v) => return TokenKind::TimeLiteral(str.parse::<f64>().unwrap() * v),
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
                },
            };

            if has_base {
                let mut size: usize = str.parse().unwrap();

                // The spec says not only the size cannot be zero, it cannot begin with zero
                if str.as_bytes()[0] == b'0' {
                    if size == 0 {
                        self.report_span(
                            Severity::Error,
                            "size specifier cannot be zero",
                            start,
                            size_pos,
                        );

                        // Error recovery
                        size = 1;
                    } else {
                        // This is a separate diagnostics since we have a clue about how to fix.
                        self.report_span_with_hint(
                            Severity::Error,
                            "size specifier cannot begin with zero",
                            format!("{}", size),
                            start,
                            size_pos,
                        );
                    }
                }

                let mut num = self.parse_based_number();
                num = num.xz_extend_or_trunc(size);
                return TokenKind::IntegerLiteral(LogicNumber {
                    sized: true,
                    value: num,
                });
            }
        }

        // This is only a number, restore position
        self.pos = size_pos;

        let num: BigUint = str.parse().unwrap();
        TokenKind::IntegerLiteral(LogicNumber {
            sized: false,
            value: LogicVec::new_xz(
                // +1 here to account for the sign bit.
                cmp::max(num.bits() as usize + 1, 32),
                true,
                num,
                BigUint::zero(),
            ),
        })
    }

    fn next_tk(&mut self) -> TokenKind {
        self.start = self.pos;

        // Early return if result is EOF
        let ch = match self.nextch() {
            None => return TokenKind::Eof,
            Some(v) => v,
        };

        match ch {
            // Whitespaces
            ' ' | '\t' => TokenKind::Whitespace,
            // Line terminators
            '\r' => {
                self.skip_crlf();
                TokenKind::NewLine
            }
            '\n' => TokenKind::NewLine,
            // Comments
            '/' => match self.peekch() {
                Some('/') => {
                    self.skip_line_comment();
                    TokenKind::LineComment
                }
                Some('*') => {
                    self.skip_block_comment();
                    TokenKind::BlockComment
                }
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::Div)
                }
                _ => TokenKind::BinaryOp(BinaryOp::Div),
            },
            // Identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                self.pos = self.start;
                let name = self.parse_identifier();
                // We only recognise keywords outside attributes
                if !self.attr {
                    match HASHMAP.get::<str>(&name) {
                        Some(&(ref kw, v)) => {
                            if v <= self.keyword {
                                kw.clone()
                            } else {
                                TokenKind::Id(name)
                            }
                        }
                        _ => TokenKind::Id(name),
                    }
                } else {
                    TokenKind::Id(name)
                }
            }
            '$' => {
                let name = self.parse_identifier();
                match &name as &str {
                    "" => TokenKind::Dollar,
                    "unit" => TokenKind::Keyword(Keyword::Unit),
                    "root" => TokenKind::Keyword(Keyword::Root),
                    _ => TokenKind::SystemTask(name),
                }
            }
            '\\' => TokenKind::Id(self.parse_esc_id()),
            '`' => {
                match self.peekch().unwrap_or(' ') {
                    // Special symbols meaningful inside substitution text.
                    '"' => {
                        self.nextch();
                        TokenKind::Directive("\"".to_owned())
                    }
                    '`' => {
                        self.nextch();
                        TokenKind::Directive("`".to_owned())
                    }
                    '\\' => {
                        self.nextch();
                        if self.peekch() == Some('`') && self.peekch() == Some('"') {
                            TokenKind::Directive("\\`\"".to_owned())
                        } else {
                            self.pos = self.start + 1;
                            TokenKind::Directive(self.parse_esc_id())
                        }
                    }
                    'a'..='z' | 'A'..='Z' | '_' | '$' => {
                        TokenKind::Directive(self.parse_identifier())
                    }
                    _ => {
                        // Warn about a ` without name following
                        self.report_pos(Severity::Error, "` without directive name", self.pos);
                        TokenKind::Unknown
                    }
                }
            }
            // Literals
            '0'..='9' => {
                self.pushback(ch);
                self.parse_number()
            }
            '\'' => match self.peekch().unwrap_or(' ') {
                's' | 'S' | 'd' | 'D' | 'b' | 'B' | 'o' | 'O' | 'h' | 'H' => {
                    let num = self.parse_based_number();
                    let size = cmp::max(num.width() + num.signed() as usize, 32);
                    let num = num.xz_extend_or_trunc(size);
                    TokenKind::IntegerLiteral(LogicNumber {
                        sized: false,
                        value: num,
                    })
                }
                '{' => {
                    self.nextch();
                    TokenKind::OpenDelim(Delim::TickBrace)
                }
                '0' => {
                    self.nextch();
                    TokenKind::UnbasedLiteral(LogicValue::Zero)
                }
                '1' => {
                    self.nextch();
                    TokenKind::UnbasedLiteral(LogicValue::One)
                }
                'z' | 'Z' | '?' => {
                    self.nextch();
                    TokenKind::UnbasedLiteral(LogicValue::Z)
                }
                'x' | 'X' => {
                    self.nextch();
                    TokenKind::UnbasedLiteral(LogicValue::X)
                }
                _ => TokenKind::Tick,
            },
            '"' => self.parse_string(),
            // Delimiters
            '(' => {
                if self.nextch_if('*') {
                    if self.nextch_if(')') {
                        TokenKind::ParenedStar
                    } else {
                        if self.attr {
                            self.report_span(
                                Severity::Error,
                                "attribute (* cannot be nested",
                                self.start,
                                self.pos,
                            );
                        }
                        self.attr = true;
                        TokenKind::OpenDelim(Delim::Attr)
                    }
                } else {
                    TokenKind::OpenDelim(Delim::Paren)
                }
            }
            ')' => TokenKind::CloseDelim(Delim::Paren),
            '[' => TokenKind::OpenDelim(Delim::Bracket),
            ']' => TokenKind::CloseDelim(Delim::Bracket),
            '{' => TokenKind::OpenDelim(Delim::Brace),
            '}' => TokenKind::CloseDelim(Delim::Brace),
            // Operators
            '+' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::Add)
                }
                Some('+') => {
                    self.nextch();
                    TokenKind::IncDec(IncDec::Inc)
                }
                Some(':') => {
                    self.nextch();
                    TokenKind::PlusColon
                }
                _ => TokenKind::BinaryOp(BinaryOp::Add),
            },
            '-' => match self.peekch() {
                Some(':') => {
                    self.nextch();
                    TokenKind::MinusColon
                }
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::Sub)
                }
                Some('-') => {
                    self.nextch();
                    TokenKind::IncDec(IncDec::Dec)
                }
                Some('>') => {
                    self.nextch();
                    if self.nextch_if('>') {
                        TokenKind::NonblockTrigger
                    } else {
                        TokenKind::BinaryOp(BinaryOp::Imply)
                    }
                }
                _ => TokenKind::BinaryOp(BinaryOp::Sub),
            },
            '*' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::Mul)
                }
                Some('*') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::Power)
                }
                Some('>') => {
                    self.nextch();
                    TokenKind::FullConnect
                }
                Some(')') => {
                    self.nextch();
                    if !self.attr {
                        self.report_span(
                            Severity::Error,
                            "attribute *) without corresponding (*",
                            self.start,
                            self.pos,
                        );
                    }
                    self.attr = false;
                    TokenKind::CloseDelim(Delim::Attr)
                }
                _ => TokenKind::BinaryOp(BinaryOp::Mul),
            },
            '%' => {
                if self.nextch_if('=') {
                    TokenKind::BinaryOpAssign(BinaryOp::Mod)
                } else {
                    TokenKind::BinaryOp(BinaryOp::Mod)
                }
            }
            '&' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::And)
                }
                Some('&') => {
                    self.nextch();
                    if self.nextch_if('&') {
                        TokenKind::TripleAnd
                    } else {
                        TokenKind::BinaryOp(BinaryOp::LAnd)
                    }
                }
                _ => TokenKind::BinaryOp(BinaryOp::And),
            },
            '^' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOpAssign(BinaryOp::Xor)
                }
                Some('|') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::Xnor)
                }
                _ => TokenKind::BinaryOp(BinaryOp::Xor),
            },
            '|' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    if self.nextch_if('>') {
                        TokenKind::NonOverlapImply
                    } else {
                        TokenKind::BinaryOpAssign(BinaryOp::Or)
                    }
                }
                Some('|') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::LOr)
                }
                Some('-') => {
                    self.nextch();
                    if self.nextch_if('>') {
                        TokenKind::OverlapImply
                    } else {
                        self.pushback('-');
                        TokenKind::BinaryOp(BinaryOp::Or)
                    }
                }
                _ => TokenKind::BinaryOp(BinaryOp::Or),
            },
            '=' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    match self.peekch() {
                        Some('=') => {
                            self.nextch();
                            TokenKind::BinaryOp(BinaryOp::CaseEq)
                        }
                        Some('?') => {
                            self.nextch();
                            TokenKind::BinaryOp(BinaryOp::WildEq)
                        }
                        _ => TokenKind::BinaryOp(BinaryOp::Eq),
                    }
                }
                Some('>') => {
                    self.nextch();
                    TokenKind::ParConnect
                }
                _ => TokenKind::Assign,
            },
            '!' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    match self.peekch() {
                        Some('=') => {
                            self.nextch();
                            TokenKind::BinaryOp(BinaryOp::CaseNeq)
                        }
                        Some('?') => {
                            self.nextch();
                            TokenKind::BinaryOp(BinaryOp::WildNeq)
                        }
                        _ => TokenKind::BinaryOp(BinaryOp::Neq),
                    }
                }
                _ => TokenKind::UnaryOp(UnaryOp::LNot),
            },
            '~' => match self.peekch() {
                Some('&') => {
                    self.nextch();
                    TokenKind::UnaryOp(UnaryOp::Nand)
                }
                Some('|') => {
                    self.nextch();
                    TokenKind::UnaryOp(UnaryOp::Nor)
                }
                Some('^') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::Xnor)
                }
                _ => TokenKind::UnaryOp(UnaryOp::Not),
            },
            '#' => {
                if self.nextch_if('#') {
                    TokenKind::CycleDelay
                } else {
                    TokenKind::Hash
                }
            }
            ',' => TokenKind::Comma,
            '.' => {
                if self.nextch_if('*') {
                    TokenKind::WildPattern
                } else {
                    TokenKind::Dot
                }
            }
            ':' => match self.peekch() {
                Some(':') => {
                    self.nextch();
                    TokenKind::ScopeSep
                }
                Some('=') => {
                    self.nextch();
                    TokenKind::DistEq
                }
                Some('/') => {
                    self.nextch();
                    TokenKind::DistDiv
                }
                _ => TokenKind::Colon,
            },
            ';' => TokenKind::Semicolon,
            '<' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::Leq)
                }
                Some('<') => {
                    self.nextch();
                    match self.peekch() {
                        Some('<') => {
                            self.nextch();
                            if self.nextch_if('=') {
                                TokenKind::BinaryOpAssign(BinaryOp::Shl)
                            } else {
                                TokenKind::BinaryOp(BinaryOp::Shl)
                            }
                        }
                        Some('=') => {
                            self.nextch();
                            TokenKind::BinaryOpAssign(BinaryOp::Shl)
                        }
                        _ => TokenKind::LShl,
                    }
                }
                Some('-') => {
                    self.nextch();
                    if self.nextch_if('>') {
                        TokenKind::BinaryOp(BinaryOp::Equiv)
                    } else {
                        self.pushback('-');
                        TokenKind::BinaryOp(BinaryOp::Lt)
                    }
                }
                _ => TokenKind::BinaryOp(BinaryOp::Lt),
            },
            '>' => match self.peekch() {
                Some('=') => {
                    self.nextch();
                    TokenKind::BinaryOp(BinaryOp::Geq)
                }
                Some('>') => {
                    self.nextch();
                    match self.peekch() {
                        Some('>') => {
                            self.nextch();
                            if self.nextch_if('=') {
                                TokenKind::BinaryOpAssign(BinaryOp::AShr)
                            } else {
                                TokenKind::BinaryOp(BinaryOp::AShr)
                            }
                        }
                        Some('=') => {
                            self.nextch();
                            TokenKind::BinaryOpAssign(BinaryOp::LShr)
                        }
                        _ => TokenKind::BinaryOp(BinaryOp::LShr),
                    }
                }
                _ => TokenKind::BinaryOp(BinaryOp::Gt),
            },
            '?' => TokenKind::Question,
            '@' => match self.peekch() {
                Some('@') => {
                    self.nextch();
                    TokenKind::AtAt
                }
                Some('*') => {
                    self.nextch();
                    TokenKind::AtStar
                }
                _ => TokenKind::At,
            },
            _ => {
                self.report_pos(
                    Severity::Error,
                    "unknown character in source file",
                    self.start,
                );
                TokenKind::Unknown
            }
        }
    }

    pub fn next_span(&mut self) -> Option<Token> {
        loop {
            let tok = self.next_tk();
            match tok {
                TokenKind::Eof => return None,
                // Ignore space and block comment. New line and line comments are not handled here
                // as preprocessor needs them to know about how to process directives.
                TokenKind::Whitespace | TokenKind::BlockComment => continue,
                _ => (),
            }
            return Some(Spanned::new(
                tok,
                Pos(self.src_offset.0 + self.start).span_to(Pos(self.src_offset.0 + self.pos)),
            ));
        }
    }
}
