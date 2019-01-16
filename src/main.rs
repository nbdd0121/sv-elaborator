#![allow(dead_code)]

extern crate num;
extern crate lazycell;
extern crate colored;
#[macro_use]
extern crate lazy_static;
extern crate getopts;

mod util;
mod syntax;
mod source;
mod number;
mod printer;
mod elaborate;
mod lowering;

use std::fs::File;
use std::io::prelude::*;

use source::{SrcMgr, Source, DiagMgr, Severity};
use printer::PrettyPrint;
use syntax::ast;

// use lexer::TokenKind;
use std::rc::Rc;

fn print_help(opts: &getopts::Options, program: &str) {
    let brief = format!("Usage: {} [options] FILES", program);
    eprint!("{}", opts.usage(&brief));
}

fn main() {
    //
    // Argument parsing
    //
    let args: Vec<String> = std::env::args().collect();

    let mut opts = getopts::Options::new();
    opts.optopt("o", "", "set output file name", "FILE");
    opts.optopt("t", "", "set toplevel module name", "MODULE");
    opts.optmulti("b", "", "set a module to be a black box", "MODULE");
    opts.optflag("", "parse", "parse only, do not elaborate");
    opts.optflag("h", "help", "print this help message");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => {
            eprintln!("{}", f.to_string());
            return print_help(&opts, &args[0])
        }
    };

    if matches.opt_present("h") {
        return print_help(&opts, &args[0])
    }

    // Initailise source manager and diagnostic manager first
    let src_mgr = Rc::new(SrcMgr::new());
    let diag_mgr = DiagMgr::new(src_mgr.clone());

    if matches.free.is_empty() {
        diag_mgr.report_span(Severity::Fatal, "no input files specified", source::Span::none());
        return;
    }

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
    'outer: for filename in &matches.free {
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

    if matches.opt_present("parse") {
        let mut out: Box<dyn Write> = match matches.opt_str("o") {
            None => Box::new(std::io::stdout()),
            Some(v) => Box::new(File::create(v).unwrap()),
        };

        let mut printer = PrettyPrint::new();
        for list in &files {
            for i in list {
                printer.print_item(&i);
                printer.append("\n");
            }
        }
        writeln!(out, "{}", printer.take()).unwrap();
        return;
    }

    let mut elaborated = elaborate::elaborate(&diag_mgr, &files, match matches.opt_str("t") {
        None => "chip_top",
        Some(ref v) => v,
    });

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() { return; }

    lowering::gen_name_assign(&mut elaborated);
    let elaborated = lowering::loop_gen_elim(elaborated);
    let elaborated = lowering::inst_array_elim(elaborated);
    let mut elaborated = lowering::gen_blk_elim(elaborated);
    lowering::type_param_elim(&mut elaborated);

    let files = elaborate::reconstruct(&elaborated);

    let mut out: Box<dyn Write> = match matches.opt_str("o") {
        None => Box::new(std::io::stdout()),
        Some(v) => Box::new(File::create(v).unwrap()),
    };

    {
        let list = files.first().unwrap();
        writeln!(out, "/* packages */").unwrap();
        let mut printer = PrettyPrint::new();
        for i in list {
            printer.print_item(&i);
            printer.append("\n");
        }
        writeln!(out, "{}", printer.take()).unwrap();
    }

    let blackbox = matches.opt_strs("b");
    for (list, name) in files.iter().skip(1).zip(matches.free.iter()) {
        writeln!(out, "/* file: {} */", name).unwrap();
        if list.is_empty() {

        } else {
            let mut printer = PrettyPrint::new();
            for i in list {
                // If it is a design unit specified in blackbox list, do not print it out.
                if let ast::Item::DesignDecl(ref decl) = i {
                    if blackbox.iter().any(|item| &decl.name.value == item) {
                        continue
                    }
                }
                printer.print_item(&i);
                printer.append("\n");
            }
            writeln!(out, "{}", printer.take()).unwrap();
        }
    }
}
