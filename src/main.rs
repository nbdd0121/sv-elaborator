#![allow(dead_code)]
#![warn(rust_2018_idioms)]

mod elaborate;
mod lowering;
mod number;
mod opts;
mod printer;
mod source;
mod syntax;
mod util;

use clap::Parser;
use std::fs::File;
use std::io::prelude::*;
use std::path::PathBuf;

use crate::printer::PrettyPrint;
use crate::source::{DiagMgr, Severity, Source, SrcMgr};
use crate::syntax::ast;

// use lexer::TokenKind;
use std::rc::Rc;

#[derive(Parser)]
struct Options {
    /// Set output file name
    #[clap(short, value_name = "FILE")]
    output: Option<PathBuf>,
    /// Set toplevel module name
    #[clap(short, value_name = "MODULE")]
    toplevel: Option<String>,
    /// Set a module to be a black box
    #[clap(short, value_name = "MODULE")]
    blackbox: Vec<String>,
    /// Add a path to the include search path
    #[clap(short = 'I', value_name = "PATH")]
    include: Vec<PathBuf>,
    /// Parse only, do not elaborate
    #[clap(long = "parse")]
    parse_only: bool,
    /// Give a prefix to all generated modules
    #[clap(short)]
    prefix: Option<String>,
    /// Input files
    #[clap(value_name = "FILES")]
    input: Vec<String>,
}

fn main() {
    let mut opts = Options::parse();
    opts.include.insert(0, PathBuf::new());

    // Initailise source manager and diagnostic manager first
    let src_mgr = Rc::new(SrcMgr::new(opts.include));
    let diag_mgr = DiagMgr::new(src_mgr.clone());

    if opts.input.is_empty() {
        diag_mgr.report_span(
            Severity::Fatal,
            "no input files specified",
            source::Span::none(),
        );
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
    for filename in &opts.input {
        let mut infile = File::open(filename).unwrap();
        let mut contents = String::new();
        infile.read_to_string(&mut contents).unwrap();

        let src = Rc::new(Source::new((*filename).to_owned(), contents));
        src_mgr.add_source(src.clone());

        let list = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let tokens = syntax::pp(&src_mgr, &diag_mgr, &src);
            let tokens = syntax::tk_tree(&diag_mgr, tokens);
            let list = syntax::parse(&diag_mgr, tokens);
            list
        })) {
            Ok(v) => v,
            Err(info) => {
                if info.downcast_ref::<Severity>().is_some() {
                    continue;
                } else {
                    ::std::process::exit(1);
                }
            }
        };

        files.push(list);
    }

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() {
        ::std::process::exit(1);
    }

    elaborate::resolve(&diag_mgr, &mut files);

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() {
        ::std::process::exit(1);
    }

    if opts.parse_only {
        let mut out: Box<dyn Write> = match opts.output {
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

    let elab_opts = opts::Opts {
        blackbox: opts.blackbox,
        prefix: opts.prefix,
        toplevel: opts.toplevel.unwrap_or_else(|| "chip_top".to_owned()),
    };

    let mut elaborated = elaborate::elaborate(&diag_mgr, &files, &elab_opts);

    // Abort elaboration when there are syntax errors.
    if diag_mgr.has_error() {
        ::std::process::exit(1);
    }

    lowering::gen_name_assign(&mut elaborated);
    let elaborated = lowering::loop_gen_elim(elaborated);
    let elaborated = lowering::inst_array_elim(elaborated);
    let mut elaborated = lowering::gen_blk_elim(elaborated);
    lowering::type_param_elim(&mut elaborated);

    // If a prefix is specified from command line, do an additional transformation
    if elab_opts.prefix.is_some() {
        elaborated = lowering::prefix(elaborated, &elab_opts);
    }

    let files = elaborate::reconstruct(&elaborated);

    let mut out: Box<dyn Write> = match opts.output {
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

    for (list, name) in files.iter().skip(1).zip(opts.input) {
        writeln!(out, "/* file: {} */", name).unwrap();
        if list.is_empty() {
        } else {
            let mut printer = PrettyPrint::new();
            for i in list {
                // If it is a design unit specified in blackbox list, do not print it out.
                if let ast::Item::DesignDecl(ref decl) = i {
                    if elab_opts
                        .blackbox
                        .iter()
                        .any(|item| &decl.name.value == item)
                    {
                        continue;
                    }
                }
                printer.print_item(&i);
                printer.append("\n");
            }
            writeln!(out, "{}", printer.take()).unwrap();
        }
    }
}
