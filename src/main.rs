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
mod elaborate;

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
    let diag_mgr = DiagMgr::new(src_mgr.clone());

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

    // Parse all files together
    let mut files = Vec::new();
    'outer: for filename in &files_to_test {
        let mut infile = File::open(filename).unwrap();
        let mut contents = String::new();
        infile.read_to_string(&mut contents).unwrap();

        let src = Rc::new(Source::new((*filename).to_owned(), contents));
        src_mgr.add_source(src.clone());

        let list = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let tokens = syntax::lex(&src_mgr, &diag_mgr, &src);
            let list = syntax::parse(&diag_mgr, tokens);
            list
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

        files.push(list);
    }

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() { return; }

    elaborate::resolve(&diag_mgr, &mut files);

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() { return; }

    if false {
        let mut printer = PrettyPrint::new();
        for list in &files {
            for i in list {
                printer.print_item(&i);
                printer.append("\n");
            }
        }
        println!("{}", printer.take());
    }

    let elaborated = elaborate::elaborate(&diag_mgr, &files);

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() { return; }

    let files = elaborate::reconstruct(&elaborated);

    {
        let list = files.first().unwrap();
        println!("/* packages */");
        let mut printer = PrettyPrint::new();
        for i in list {
            printer.print_item(&i);
            printer.append("\n");
        }
        println!("{}", printer.take());
    }

    for (list, name) in files.iter().skip(1).zip(files_to_test.iter()) {
        println!("/* file: {} */", name);
        if list.is_empty() {

        } else {
            let mut printer = PrettyPrint::new();
            for i in list {
                printer.print_item(&i);
                printer.append("\n");
            }
            println!("{}", printer.take());
        }
    }
}
