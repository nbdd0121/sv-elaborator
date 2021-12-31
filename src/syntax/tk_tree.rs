//! After tokenization and preprocessing, we got a stream of tokens. Token stream is good for
//! preprocessing purposes but not ideal for parsing, which may require jumping over delimited
//! pairs. Therefore we add an additional step of organisation them into token trees where all
//! tokens between delimited pairs are grouped together.

use super::ast::*;
use super::tokens::*;

use super::super::source::DiagMgr;
use std::collections::VecDeque;

pub fn tk_tree<'a>(diag: &'a DiagMgr, tokens: VecDeque<Token>) -> VecDeque<Token> {
    TkTree::new(diag, tokens).all()
}

struct TkTree<'a> {
    diag: &'a DiagMgr,
    tokens: VecDeque<Token>,
}

impl<'a> TkTree<'a> {
    fn new(diag: &'a DiagMgr, tokens: VecDeque<Token>) -> TkTree<'a> {
        TkTree { diag, tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens
            .pop_front()
            .unwrap_or_else(|| Spanned::new_unspanned(TokenKind::Eof))
    }

    fn next_tree_recurse(&mut self) -> Token {
        let tok = self.next();
        let delim = match *tok {
            // Continue processing if this is an open delimiter
            TokenKind::OpenDelim(delim) => delim,
            // Otherwise return as-is.
            _ => return tok,
        };
        let exp_close = match delim {
            // "'{" correspond to "}"
            Delim::TickBrace => Delim::Brace,
            _ => delim,
        };
        let mut vec = VecDeque::new();
        // Keep reading token until we see a closing delimiter.
        let (close_tok, close_delim) = loop {
            let nxt = self.next_tree_recurse();
            match *nxt {
                TokenKind::CloseDelim(delim) => break (nxt, Some(delim)),
                TokenKind::Eof => break (nxt, None),
                _ => vec.push_back(nxt),
            }
        };
        match close_delim {
            None => {
                self.diag
                    .report_error("open delimiter that is never closed", tok.span);
            }
            Some(v) if v != exp_close => {
                // If symbol doesn't match, raise an error
                self.diag.report_error(
                    format!("unexpected closing delimiter, expecting {:#?}", exp_close),
                    close_tok.span,
                );
            }
            _ => (),
        }
        let overall_span = tok.span.merge(close_tok.span);
        Spanned::new(
            TokenKind::DelimGroup(
                delim,
                Box::new(DelimGroup {
                    open: tok,
                    close: close_tok,
                    tokens: vec,
                }),
            ),
            overall_span,
        )
    }

    fn next_tree(&mut self) -> Token {
        loop {
            let tok = self.next_tree_recurse();
            match *tok {
                TokenKind::CloseDelim(_) => {
                    self.diag.report_error("extra closing delimiter", tok.span);
                }
                _ => return tok,
            }
        }
    }

    fn all(&mut self) -> VecDeque<Token> {
        let mut vec = VecDeque::new();
        loop {
            let tok = self.next_tree();
            match *tok {
                TokenKind::Eof => return vec,
                _ => vec.push_back(tok),
            }
        }
    }
}
