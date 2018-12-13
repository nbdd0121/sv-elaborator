#![allow(dead_code)]

extern crate num;
extern crate lazycell;
extern crate colored;
#[macro_use]
extern crate lazy_static;

mod util;
mod syntax;
mod source;
mod number;
mod printer;

use std::fs::File;
use std::io::prelude::*;

use source::{SrcMgr, Source, DiagMgr, Severity};
use printer::PrettyPrint;

// use lexer::TokenKind;
use std::rc::Rc;

fn main() {
    let files_to_test = vec![
        "test.sv",
    ];

    let src_mgr = Rc::new(SrcMgr::new());
    let diag_mgr = Rc::new(DiagMgr::new(src_mgr.clone()));

    // Register a new panic handler. If the panic is caused by throwing fatal error, we mute
    // Rust's built-in error message and stack trace.
    {
        let hook = std::panic::take_hook();
        std::panic::set_hook(Box::new(move |info| {
            if info.payload().downcast_ref::<Severity>().is_none() {
                hook(info)
            }
        }));
    }

    'outer: for filename in files_to_test {
        let mut infile = File::open(filename).unwrap();
        let mut contents = String::new();
        infile.read_to_string(&mut contents).unwrap();

        // contents = format!("({})", contents);
        let src = Rc::new(Source::new(filename.to_owned(), contents));
        src_mgr.add_source(src.clone());

        let mut tokens = syntax::lexer::Lexer::new(src_mgr.clone(), diag_mgr.clone(), &src).all();
        let mut psr = syntax::parser::Parser::new(src_mgr.clone(), diag_mgr.clone(), tokens);

        let list = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            psr.parse_source()
        })) {
            Ok(v) => v,
            Err(info) => {
                if info.downcast_ref::<Severity>().is_some() {
                    continue
                } else {
                    return;
                }
            }
        };

        let mut printer = PrettyPrint::new();
        for i in list {
            printer.print_item(&i);
            printer.append("\n");
        }
        println!("{}", printer.take());
    }
}
