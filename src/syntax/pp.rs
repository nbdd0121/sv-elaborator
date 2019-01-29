use super::tokens::*;
use super::ast::*;

use source::{Source, SrcMgr, DiagMgr, Severity, Span};
use super::lexer::Lexer;

use std::rc::Rc;
use std::collections::VecDeque;
use std::collections::HashMap;

pub fn pp<'a>(mgr: &'a SrcMgr, diag: &'a DiagMgr, src: &Rc<Source>) -> VecDeque<Token> {
    Preprocessor::new(mgr, diag).all(src)
}

struct Preprocessor<'a> {
    mgr: &'a SrcMgr,
    diag: &'a DiagMgr,
    /// Buffer for tokens pushed back
    pushback: Vec<Token>,
    stacks: Vec<Lexer<'a>>,
    macros: HashMap<String, (Span, Option<Vec<(Spanned<String>, Option<Vec<Token>>)>>, VecDeque<Token>)>,
    /// A branch stack indicating whether previous branch is taken and whether an else is encountered
    branch_stack: Vec<(bool, bool)>,
}

impl<'a> Preprocessor<'a> {
    fn new(mgr: &'a SrcMgr, diag: &'a DiagMgr) -> Preprocessor<'a> {
        Preprocessor {
            mgr,
            diag,
            pushback: Vec::new(),
            stacks: Vec::new(),
            macros: HashMap::new(),
            branch_stack: Vec::new(),
        }
    }

    fn peek_raw(&mut self) -> Option<&Token> {
        if self.pushback.is_empty() {
            if let Some(token) = self.next_raw() {
                self.pushback.push(token);
            }
        }
        self.pushback.last()
    }

    /// Retrieve next raw, unprocessed token
    fn next_raw(&mut self) -> Option<Token> {
        if !self.pushback.is_empty() {
            return self.pushback.pop();
        }
        loop {
            match self.stacks.last_mut() {
                None => return None,
                Some(v) => match v.next_span() {
                    None => (),
                    Some(v) => return Some(v),
                }
            }
            self.stacks.pop();
        }
    }

    fn pushback_raw(&mut self, tok: Token) {
        self.pushback.push(tok);
    }

    /// Check if a name is one of built-in directive.
    fn is_directive(name: &str) -> bool {
        match name {
            "resetall" |
            "include" |
            "define" |
            "undef" |
            "undefineall" |
            "ifdef" |
            "else" |
            "elsif" |
            "endif" |
            "ifndef" |
            "timescale" |
            "default_nettype" |
            "unconnected_drive" |
            "nounconnected_drive" |
            "celldefine" |
            "endcelldefine" |
            "pragma" |
            "line" |
            "__FILE__" |
            "__LINE__" |
            "begin_keywords" |
            "end_keywords" => true,
            _ => false,
        }
    }

    fn process(&mut self) -> Option<Token> {
        let mut after_newline = false;
        loop {
            let (name, span) = match self.next_raw() {
                // Found a directive
                Some(Spanned{value: TokenKind::Directive(name), span}) => (name, span),
                // Newline token, set after_newline and continue
                Some(Spanned{value: TokenKind::NewLine, ..}) |
                Some(Spanned{value: TokenKind::LineComment, ..}) => {
                    after_newline = true;
                    continue;
                }
                // Not a directive, just return as-is
                v => return v,
            };

            match name.as_ref() {
                "resetall" => {
                    self.diag.report_span(Severity::Warning, "compiler directive not yet supported", span);
                }
                "include" => {
                    if !after_newline {
                        self.diag.report_error("`include must be on its own line", span);
                    }
                    self.parse_include(span);
                }
                "define" => self.parse_define(span),
                "undef" |
                "undefineall" => {
                    self.diag.report_span(Severity::Warning, "compiler directive not yet supported", span);
                }
                "ifdef" => self.parse_ifdef(span, true),
                "ifndef" => self.parse_ifdef(span, false),
                "else" => self.parse_else(span),
                "elsif" => self.parse_elsif(span),
                "endif" => self.parse_endif(span),
                "timescale" |
                "default_nettype" |
                "unconnected_drive" |
                "nounconnected_drive" |
                "celldefine" |
                "endcelldefine" |
                "pragma" |
                "line" |
                "__FILE__" |
                "__LINE__" |
                "begin_keywords" |
                "end_keywords" => {
                    self.diag.report_span(Severity::Warning, "compiler directive not yet supported", span);
                }
                _ => {
                    let function_like = match self.macros.get(&name) {
                        None => {
                            self.diag.report_error(
                                format!("cannot find macro {}", name),
                                span
                            );
                            continue
                        }
                        Some((_, Some(_), _)) => true,
                        _ => false,
                    };
                    if function_like {
                        // For function-like macro also get its argument list
                        let args = self.parse_macro_args();
                        // TODO: Replace macro within macro and handle `", ``, etc
                        match self.macros.get(&name) {
                            Some((_, Some(params), list)) => {
                                let newlist: Vec<_> = list.iter().flat_map(|x| {
                                    match x.value {
                                        TokenKind::Id(ref id) => {
                                            if let Some(pos) = params.iter().position(|(x, _)| &x.value == id) {
                                                return args[pos].clone().into_iter()
                                            }
                                        }
                                        _ => (),
                                    }
                                    let mut x = x.clone();
                                    x.span = span;
                                    vec![x].into_iter()
                                }).collect();
                                for tok in newlist.into_iter().rev() {
                                    self.pushback.push(tok);
                                }
                            }
                            _ => unreachable!(),
                        }
                    }else {
                        // TODO: Replace macro within macro and handle `", ``, etc
                        match self.macros.get(&name) {
                            Some((_, _, list)) => {
                                for tok in list.iter().rev() {
                                    let mut tok = tok.clone();
                                    tok.span = span;
                                    self.pushback.push(tok);
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }

            after_newline = false;
        }
    }

    /// Parse a actual argument list of macro
    fn parse_macro_args(&mut self) -> Vec<Vec<Token>> {
        // Expect to see a opening paranthesis
        match self.peek_raw() {
            Some(Spanned{value: TokenKind::OpenDelim(Delim::Paren), ..}) => (),
            _ => {
                self.diag.report_error("Expected actual arguments for function-like macro", self.peek_raw().unwrap().span);
                // Error recovery
                return Vec::new();
            },
        }
        self.next_raw();

        let mut list = Vec::new();
        loop {
            let mut tokens = Vec::new();
            let mut level = 0;

            // Read all tokens, skipping over delimited parenthesis.
            loop {
                let tok = self.next_raw().unwrap();
                let end = match tok.value {
                    TokenKind::OpenDelim(Delim::Paren) => {
                        level += 1;
                        false
                    }
                    TokenKind::CloseDelim(Delim::Paren) => {
                        if level > 0 {
                            level -= 1;
                            false
                        } else {
                            true
                        }
                    }
                    TokenKind::Comma => {
                        level == 0
                    }
                    _ => false,
                };
                if end {
                    self.pushback_raw(tok);
                    break;
                }
                tokens.push(tok);
            }

            list.push(tokens);

            // Break out from the loop if not comma
            match self.peek_raw() {
                Some(Spanned{value: TokenKind::Comma, ..}) => (),
                _ => break,
            }
            // Consume the comma
            self.next_raw();
        }

        // Expecting to see a closing parenthesis
        match self.peek_raw() {
            Some(Spanned{value: TokenKind::CloseDelim(Delim::Paren), ..}) => (),
            _ => {
                self.diag.report_error("Expected closing parenthesis", self.peek_raw().unwrap().span);
                // Error recovery
                return list;
            },
        }
        self.next_raw();

        return list;
    }

    /// Read all tokens until the next newline (new line will be consumed but not returned)
    fn read_until_newline(&mut self) -> VecDeque<Token> {
        // Keep adding tokens to the list and stop when eof or eol is encountered.
        let mut list = VecDeque::new();
        loop {
            let tok = match self.next_raw() {
                None => break,
                Some(v) => v,
            };
            match tok.value {
                TokenKind::NewLine |
                TokenKind::LineComment => break,
                _ => (),
            }
            list.push_back(tok);
        }
        list
    }

    /// Read an identifier
    fn expect_id(&mut self) -> Option<Spanned<String>> {
        match self.next_raw() {
            Some(Spanned{value: TokenKind::Id(id), span}) => Some(Spanned::new(id, span)),
            Some(v) => {
                self.pushback_raw(v);
                None
            },
            None => None,
        }
    }

    /// Parse a macro definition
    /// The span here is only for diagnostic purposes.
    fn parse_define(&mut self, span: Span) {
        // The identifier after define can be a keyword
        self.stacks.last_mut().unwrap().enter_kw_scope(0);
        let token = self.expect_id();
        self.stacks.last_mut().unwrap().leave_kw_scope();
        // Read the name of this macro
        let (name, span) = match token {
            Some(v) => (v.value, v.span),
            None => {
                self.diag.report_error("expected identifier name after `define", span);
                // Error recovery: Discard until newline
                self.read_until_newline();
                return;
            }
        };

        if Self::is_directive(&name) {
            self.diag.report_error("directive name cannot be used as macro names", span);
            // Error recovery: Discard until newline
            self.read_until_newline();
            return;
        }

        // Check if this macro is function-like.
        let paren = match self.peek_raw().unwrap() {
            // If this is a parenthesis that immediately follows the name
            Spanned{value: TokenKind::OpenDelim(Delim::Paren), span: p_span} if p_span.start == span.end => true,
            _ => false,
        };

        let args = if paren {
            Some(self.parse_define_args())
        } else {
            None
        };

        let list = self.read_until_newline();

        // Insert it to the global definitions list and report error for duplicate definition
        if let Some((old_span, ..)) = self.macros.insert(name, (span, args, list)) {
            self.diag.report_error("duplicate macro definitions", span);
            self.diag.report_span(Severity::Remark, "previous declared here", old_span);
        }
    }

    /// Parse a formal argument list of `define
    fn parse_define_args(&mut self) -> Vec<(Spanned<String>, Option<Vec<Token>>)> {
        // Discard the parenthesis
        self.next_raw();
        let mut list = Vec::new();
        loop {
            let arg_name = match self.expect_id() {
                Some(v) => v,
                None => {
                    self.diag.report_error("expected identifier in macro formal argument list", self.peek_raw().unwrap().span);
                    break;
                }
            };

            let has_default = match self.peek_raw() {
                Some(Spanned{value: TokenKind::BinaryOp(BinaryOp::Eq), ..}) => true,
                _ => false,
            };
            if has_default {
                // Discard the eq symbol
                self.next_raw();
                self.diag.report_fatal("default macro argument is not yet supported", self.peek_raw().unwrap().span);
            }

            list.push((arg_name, None));

            // Break out from the loop if not comma
            match self.peek_raw() {
                Some(Spanned{value: TokenKind::Comma, ..}) => (),
                _ => break,
            }
            // Consume the comma
            self.next_raw();
        }
        match self.peek_raw() {
            Some(Spanned{value: TokenKind::CloseDelim(Delim::Paren), ..}) => (),
            _ => {
                self.diag.report_error("Expected closing parenthesis", self.peek_raw().unwrap().span);
                // Error recovery
                return list;
            },
        }
        // Discard the parenthesis
        self.next_raw();
        return list;
    }

    /// Parse an ifdef directive
    fn parse_ifdef(&mut self, span: Span, cond: bool) {
        // If this block is nested within a untaken branch, just skip everything
        if let Some((false, _)) = self.branch_stack.last() {
            self.branch_stack.push((false, false));
            return self.skip_tokens();
        }

        // Read the name of this macro
        let name = match self.expect_id() {
            Some(v) => v.value,
            None => {
                self.diag.report_error("expected identifier name after `ifdef or `ifndef", span);
                // Error recovery: Return a non-existing name, thus treating as untaken
                "".to_owned()
            }
        };

        let taken = self.macros.contains_key(&name) == cond;
        self.branch_stack.push((taken, false));
        if !taken {
            self.skip_tokens();
        }
    }

    /// Parse an elsif directive
    fn parse_elsif(&mut self, span: Span) {
        match self.branch_stack.last() {
            None => {
                // An elsif without corresponding if
                self.diag.report_error("`elsif without matching `ifdef or `ifndef", span);
                return;
            }
            Some((_, true)) => {
                // There is already an else
                self.diag.report_error("`elsif after an `else", span);
                // Error recovery: skip
                self.skip_tokens();
                return;
            }
            Some((true, false)) => {
                // Already taken, skip this branch
                self.skip_tokens();
                return;
            }
            _ => {
                // Remove the previous result
                self.branch_stack.pop();
            }
        }

        // If this block is nested within a untaken branch, just skip everything
        if let Some((false, _)) = self.branch_stack.last() {
            self.branch_stack.push((false, false));
            return self.skip_tokens();
        }

        // Read the name of this macro
        let name = match self.expect_id() {
            Some(v) => v.value,
            None => {
                self.diag.report_error("expected identifier name after `ifdef or `ifndef", span);
                // Error recovery: Return a non-existing name, thus treating as untaken
                "".to_owned()
            }
        };

        let taken = self.macros.contains_key(&name);
        self.branch_stack.push((taken, false));
        if !taken {
            self.skip_tokens();
        }
    }

    /// Parse an else directive
    fn parse_else(&mut self, span: Span) {
        match self.branch_stack.last() {
            None => {
                // An elsif without corresponding if
                self.diag.report_error("`else without matching `ifdef or `ifndef", span);
                return;
            }
            Some((_, true)) => {
                // There is already an else
                self.diag.report_error("`else after an `else", span);
                // Error recovery: skip
                self.skip_tokens();
                return;
            }
            Some((true, false)) => {
                // Already taken, skip this branch
                self.skip_tokens();
                return;
            }
            _ => {
                // Remove the previous result
                self.branch_stack.pop();
            }
        }

        // If this block is nested within a untaken branch, just skip everything
        if let Some((false, _)) = self.branch_stack.last() {
            self.branch_stack.push((false, true));
            return self.skip_tokens();
        }

        self.branch_stack.push((true, true));
    }

    /// Parse an endif directive
    fn parse_endif(&mut self, span: Span) {
        match self.branch_stack.pop() {
            None => {
                // An endif without corresponding if
                self.diag.report_error("`endif without matching `ifdef or `ifndef", span);
            }
            Some(_) => {
                // If previous branch is not taken, then we still need to ignore things after this `endif
                if let Some((false, _)) = self.branch_stack.last() {
                    self.skip_tokens()
                }
            }
        }
    }

    /// Parse an include directive
    fn parse_include(&mut self, span: Span) {
        let (filename, span) = match self.next_raw() {
            // TODO: Also handle <xxx>?
            Some(Spanned{value: TokenKind::StringLiteral(str), span}) => (str, span),
            Some(Spanned{value: TokenKind::BinaryOp(BinaryOp::Lt), ..}) => {
                self.diag.report_error("<filename> style include is not yet supported", span);
                return;
            }
            _ => {
                self.diag.report_error("expecting file name after `include", span);
                return;
            },
        };
        let file = match self.mgr.load_source(&filename) {
            Ok(file) => file,
            Err(err) => {
                self.diag.report_error(format!("failed when loading file {}: {}", filename, err), span);
                return;
            }
        };
        self.stacks.push(Lexer::new(self.mgr, self.diag, &file));
    }

    /// Skip tokens until next branching directive or eof
    fn skip_tokens(&mut self) {
        loop {
            let token = match self.next_raw() {
                // EOF
                None => return,
                Some(v) => v,
            };

            match token.value {
                TokenKind::Directive(ref name) => {
                    match name.as_ref() {
                        // Branching directive
                        "ifdef" |
                        "ifndef" |
                        "else" |
                        "elsif" |
                        "endif" => (),
                        _ => continue,
                    }
                }
                // Other token, skip
                _ => continue,
            }

            // Now push back the token and return
            self.pushback_raw(token);
            return;
        }
    }

    fn all(&mut self, src: &Rc<Source>) -> VecDeque<Token> {
        self.stacks.push(Lexer::new(self.mgr, self.diag, src));
        let mut vec = VecDeque::new();
        loop {
            match self.process() {
                None => break,
                Some(v) => match v.value {
                    TokenKind::NewLine |
                    TokenKind::LineComment => (),
                    _ => vec.push_back(v),
                }
            }
        }
        vec
    }
}
