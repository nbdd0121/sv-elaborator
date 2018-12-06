#![allow(dead_code)]

extern crate num;
extern crate lazycell;
extern crate colored;
#[macro_use]
extern crate lazy_static;

mod util;
mod lexer;
mod parser;
mod source;
mod number;
mod printer;

use std::fs::File;
use std::io::prelude::*;

use source::{SrcMgr, Source};
use printer::PrettyPrint;

// use lexer::TokenKind;
use std::rc::Rc;

fn main() {
    let files_to_test = vec![
        "test.sv",
    ];

    let src_mgr = Rc::new(SrcMgr::new());

    'outer: for filename in files_to_test {
        let mut infile = File::open(filename).unwrap();
        let mut contents = String::new();
        infile.read_to_string(&mut contents).unwrap();

        // contents = format!("({})", contents);
        let src = Rc::new(Source::new(filename.to_owned(), contents));
        src_mgr.add_source(src.clone());

        let mut tok = lexer::Tokenizer::new(src_mgr.clone(), &src);
        let mut psr = parser::Parser::new(src_mgr.clone(), tok);
        let list = psr.parse_source().unwrap();

        for i in list {
            i.print(&mut ::std::io::stdout(), "").unwrap();
        }
    }
}