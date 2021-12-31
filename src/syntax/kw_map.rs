use super::ast::*;
use super::tokens::{Keyword, TokenKind};

use once_cell::sync::Lazy;
use std::collections::HashMap;

pub static HASHMAP: Lazy<HashMap<&'static str, (TokenKind, u8)>> = Lazy::new(|| {
    let mut m = HashMap::new();

    // Verilog 95
    m.insert("always", (TokenKind::AlwaysKw(AlwaysKw::Always), 1));
    m.insert("and", (TokenKind::Keyword(Keyword::And), 1));
    m.insert("assign", (TokenKind::Keyword(Keyword::Assign), 1));
    m.insert("begin", (TokenKind::Keyword(Keyword::Begin), 1));
    m.insert("buf", (TokenKind::Primitive(Primitive::Buf), 1));
    m.insert("bufif0", (TokenKind::Primitive(Primitive::Bufif0), 1));
    m.insert("bufif1", (TokenKind::Primitive(Primitive::Bufif1), 1));
    m.insert("case", (TokenKind::CaseKw(CaseKw::Case), 1));
    m.insert("casex", (TokenKind::CaseKw(CaseKw::Casex), 1));
    m.insert("casez", (TokenKind::CaseKw(CaseKw::Casez), 1));
    m.insert("cmos", (TokenKind::Primitive(Primitive::Cmos), 1));
    m.insert("deassign", (TokenKind::Keyword(Keyword::Deassign), 1));
    m.insert("default", (TokenKind::Keyword(Keyword::Default), 1));
    m.insert("defparam", (TokenKind::Keyword(Keyword::Defparam), 1));
    m.insert("disable", (TokenKind::Keyword(Keyword::Disable), 1));
    m.insert("edge", (TokenKind::Edge(Edge::Edge), 1));
    m.insert("else", (TokenKind::Keyword(Keyword::Else), 1));
    m.insert("end", (TokenKind::Keyword(Keyword::End), 1));
    m.insert("endcase", (TokenKind::Keyword(Keyword::Endcase), 1));
    m.insert("endmodule", (TokenKind::Keyword(Keyword::Endmodule), 1));
    m.insert("endfunction", (TokenKind::Keyword(Keyword::Endfunction), 1));
    m.insert(
        "endprimitive",
        (TokenKind::Keyword(Keyword::Endprimitive), 1),
    );
    m.insert("endspecify", (TokenKind::Keyword(Keyword::Endspecify), 1));
    m.insert("endtable", (TokenKind::Keyword(Keyword::Endtable), 1));
    m.insert("endtask", (TokenKind::Keyword(Keyword::Endtask), 1));
    m.insert("event", (TokenKind::Keyword(Keyword::Event), 1));
    m.insert("for", (TokenKind::Keyword(Keyword::For), 1));
    m.insert("force", (TokenKind::Keyword(Keyword::Force), 1));
    m.insert("forever", (TokenKind::Keyword(Keyword::Forever), 1));
    m.insert("fork", (TokenKind::Keyword(Keyword::Fork), 1));
    m.insert("function", (TokenKind::Keyword(Keyword::Function), 1));
    m.insert("highz0", (TokenKind::Strength0(DriveStrength::Highz), 1));
    m.insert("highz1", (TokenKind::Strength1(DriveStrength::Highz), 1));
    m.insert("if", (TokenKind::Keyword(Keyword::If), 1));
    m.insert("ifnone", (TokenKind::Keyword(Keyword::Ifnone), 1));
    m.insert("initial", (TokenKind::Keyword(Keyword::Initial), 1));
    m.insert("inout", (TokenKind::PortDir(PortDir::Inout), 1));
    m.insert("input", (TokenKind::PortDir(PortDir::Input), 1));
    m.insert("integer", (TokenKind::IntAtomTy(IntAtomTy::Integer), 1));
    m.insert("join", (TokenKind::Keyword(Keyword::Join), 1));
    m.insert(
        "large",
        (TokenKind::ChargeStrength(ChargeStrength::Large), 1),
    );
    m.insert("macromodule", (TokenKind::Keyword(Keyword::Module), 1));
    m.insert(
        "medium",
        (TokenKind::ChargeStrength(ChargeStrength::Medium), 1),
    );
    m.insert("module", (TokenKind::Keyword(Keyword::Module), 1));
    m.insert("nand", (TokenKind::Primitive(Primitive::Nand), 1));
    m.insert("negedge", (TokenKind::Edge(Edge::Negedge), 1));
    m.insert("nmos", (TokenKind::Primitive(Primitive::Nmos), 1));
    m.insert("nor", (TokenKind::Primitive(Primitive::Nor), 1));
    m.insert("not", (TokenKind::Keyword(Keyword::Not), 1));
    m.insert("notif0", (TokenKind::Primitive(Primitive::Notif0), 1));
    m.insert("notif1", (TokenKind::Primitive(Primitive::Notif1), 1));
    m.insert("or", (TokenKind::Keyword(Keyword::Or), 1));
    m.insert("output", (TokenKind::PortDir(PortDir::Output), 1));
    m.insert("parameter", (TokenKind::Keyword(Keyword::Parameter), 1));
    m.insert("pmos", (TokenKind::Primitive(Primitive::Pmos), 1));
    m.insert("posedge", (TokenKind::Edge(Edge::Posedge), 1));
    m.insert("primitive", (TokenKind::Keyword(Keyword::Primitive), 1));
    m.insert("pull0", (TokenKind::Strength0(DriveStrength::Pull), 1));
    m.insert("pull1", (TokenKind::Strength1(DriveStrength::Pull), 1));
    m.insert("pullup", (TokenKind::Primitive(Primitive::Pullup), 1));
    m.insert("pulldown", (TokenKind::Primitive(Primitive::Pulldown), 1));
    m.insert("rcmos", (TokenKind::Primitive(Primitive::Rcmos), 1));
    m.insert("real", (TokenKind::RealTy(RealTy::Real), 1));
    m.insert("realtime", (TokenKind::RealTy(RealTy::Real), 1));
    m.insert("reg", (TokenKind::Keyword(Keyword::Reg), 1));
    m.insert("release", (TokenKind::Keyword(Keyword::Release), 1));
    m.insert("repeat", (TokenKind::Keyword(Keyword::Repeat), 1));
    m.insert("rnmos", (TokenKind::Primitive(Primitive::Rnmos), 1));
    m.insert("rpmos", (TokenKind::Primitive(Primitive::Rpmos), 1));
    m.insert("rtran", (TokenKind::Primitive(Primitive::Rtran), 1));
    m.insert("rtranif0", (TokenKind::Primitive(Primitive::Rtranif0), 1));
    m.insert("rtranif1", (TokenKind::Primitive(Primitive::Rtranif1), 1));
    m.insert("scalared", (TokenKind::Keyword(Keyword::Scalared), 1));
    m.insert(
        "small",
        (TokenKind::ChargeStrength(ChargeStrength::Small), 1),
    );
    m.insert("specify", (TokenKind::Keyword(Keyword::Specify), 1));
    m.insert("specparam", (TokenKind::Keyword(Keyword::Specparam), 1));
    m.insert("strong0", (TokenKind::Strength0(DriveStrength::Strong), 1));
    m.insert("strong1", (TokenKind::Strength1(DriveStrength::Strong), 1));
    m.insert("supply0", (TokenKind::NetTy(NetTy::Supply0), 1));
    m.insert("supply1", (TokenKind::NetTy(NetTy::Supply1), 1));
    m.insert("table", (TokenKind::Keyword(Keyword::Table), 1));
    m.insert("task", (TokenKind::Keyword(Keyword::Task), 1));
    m.insert("time", (TokenKind::IntAtomTy(IntAtomTy::Time), 1));
    m.insert("tran", (TokenKind::Primitive(Primitive::Tran), 1));
    m.insert("tranif0", (TokenKind::Primitive(Primitive::Tranif0), 1));
    m.insert("tranif1", (TokenKind::Primitive(Primitive::Tranif1), 1));
    m.insert("tri", (TokenKind::NetTy(NetTy::Tri), 1));
    m.insert("tri0", (TokenKind::NetTy(NetTy::Tri0), 1));
    m.insert("tri1", (TokenKind::NetTy(NetTy::Tri1), 1));
    m.insert("triand", (TokenKind::NetTy(NetTy::Triand), 1));
    m.insert("trior", (TokenKind::NetTy(NetTy::Trior), 1));
    m.insert("trireg", (TokenKind::NetTy(NetTy::Trireg), 1));
    m.insert("vectored", (TokenKind::Keyword(Keyword::Vectored), 1));
    m.insert("wait", (TokenKind::Keyword(Keyword::Wait), 1));
    m.insert("wand", (TokenKind::NetTy(NetTy::Wand), 1));
    m.insert("weak0", (TokenKind::Strength0(DriveStrength::Weak), 1));
    m.insert("weak1", (TokenKind::Strength1(DriveStrength::Weak), 1));
    m.insert("while", (TokenKind::Keyword(Keyword::While), 1));
    m.insert("wire", (TokenKind::NetTy(NetTy::Wire), 1));
    m.insert("wor", (TokenKind::NetTy(NetTy::Wor), 1));
    m.insert("xnor", (TokenKind::Primitive(Primitive::Xnor), 1));
    m.insert("xor", (TokenKind::Keyword(Keyword::Xor), 1));

    // Verilog 01-noconfig
    m.insert("automatic", (TokenKind::Keyword(Keyword::Automatic), 2));
    m.insert("endgenerate", (TokenKind::Keyword(Keyword::Endgenerate), 2));
    m.insert("generate", (TokenKind::Keyword(Keyword::Generate), 2));
    m.insert("genvar", (TokenKind::Keyword(Keyword::Genvar), 2));
    m.insert("localparam", (TokenKind::Keyword(Keyword::Localparam), 2));
    m.insert(
        "noshowcancelled",
        (TokenKind::Keyword(Keyword::Noshowcancelled), 2),
    );
    m.insert(
        "pulsestyle_ondetect",
        (TokenKind::Keyword(Keyword::PulsestyleOndetect), 2),
    );
    m.insert(
        "pulsestyle_onevent",
        (TokenKind::Keyword(Keyword::PulsestyleOnevent), 2),
    );
    m.insert(
        "showcancelled",
        (TokenKind::Keyword(Keyword::Showcancelled), 2),
    );
    m.insert("signed", (TokenKind::Signing(Signing::Signed), 2));
    m.insert("unsigned", (TokenKind::Signing(Signing::Unsigned), 2));

    // Verilog 01
    m.insert("cell", (TokenKind::Keyword(Keyword::Cell), 3));
    m.insert("config", (TokenKind::Keyword(Keyword::Config), 3));
    m.insert("design", (TokenKind::Keyword(Keyword::Design), 3));
    m.insert("endconfig", (TokenKind::Keyword(Keyword::Endconfig), 3));
    m.insert("incdir", (TokenKind::Keyword(Keyword::Incdir), 3));
    m.insert("include", (TokenKind::Keyword(Keyword::Include), 3));
    m.insert("instance", (TokenKind::Keyword(Keyword::Instance), 3));
    m.insert("liblist", (TokenKind::Keyword(Keyword::Liblist), 3));
    m.insert("library", (TokenKind::Keyword(Keyword::Library), 3));
    m.insert("use", (TokenKind::Keyword(Keyword::Use), 3));

    // Verilog 05
    m.insert("uwire", (TokenKind::NetTy(NetTy::Uwire), 4));

    // SV 05
    m.insert("alias", (TokenKind::Keyword(Keyword::Alias), 5));
    m.insert(
        "always_comb",
        (TokenKind::AlwaysKw(AlwaysKw::AlwaysComb), 5),
    );
    m.insert("always_ff", (TokenKind::AlwaysKw(AlwaysKw::AlwaysFf), 5));
    m.insert(
        "always_latch",
        (TokenKind::AlwaysKw(AlwaysKw::AlwaysLatch), 5),
    );
    m.insert("assert", (TokenKind::Keyword(Keyword::Assert), 5));
    m.insert("assume", (TokenKind::Keyword(Keyword::Assume), 5));
    m.insert("before", (TokenKind::Keyword(Keyword::Before), 5));
    m.insert("bind", (TokenKind::Keyword(Keyword::Bind), 5));
    m.insert("bins", (TokenKind::Keyword(Keyword::Bins), 5));
    m.insert("binsof", (TokenKind::Keyword(Keyword::Binsof), 5));
    m.insert("bit", (TokenKind::IntVecTy(IntVecTy::Bit), 5));
    m.insert("break", (TokenKind::Keyword(Keyword::Break), 5));
    m.insert("byte", (TokenKind::IntAtomTy(IntAtomTy::Byte), 5));
    m.insert("chandle", (TokenKind::Keyword(Keyword::Chandle), 5));
    m.insert("class", (TokenKind::Keyword(Keyword::Class), 5));
    m.insert("clocking", (TokenKind::Keyword(Keyword::Clocking), 5));
    m.insert("const", (TokenKind::Keyword(Keyword::Const), 5));
    m.insert("constraint", (TokenKind::Keyword(Keyword::Constraint), 5));
    m.insert("context", (TokenKind::Keyword(Keyword::Context), 5));
    m.insert("continue", (TokenKind::Keyword(Keyword::Continue), 5));
    m.insert("cover", (TokenKind::Keyword(Keyword::Cover), 5));
    m.insert("covergroup", (TokenKind::Keyword(Keyword::Covergroup), 5));
    m.insert("coverpoint", (TokenKind::Keyword(Keyword::Coverpoint), 5));
    m.insert("cross", (TokenKind::Keyword(Keyword::Cross), 5));
    m.insert("dist", (TokenKind::Keyword(Keyword::Dist), 5));
    m.insert("do", (TokenKind::Keyword(Keyword::Do), 5));
    m.insert("endclass", (TokenKind::Keyword(Keyword::Endclass), 5));
    m.insert("endclocking", (TokenKind::Keyword(Keyword::Endclocking), 5));
    m.insert("endgroup", (TokenKind::Keyword(Keyword::Endgroup), 5));
    m.insert(
        "endinterface",
        (TokenKind::Keyword(Keyword::Endinterface), 5),
    );
    m.insert("endpackage", (TokenKind::Keyword(Keyword::Endpackage), 5));
    m.insert("endprogram", (TokenKind::Keyword(Keyword::Endprogram), 5));
    m.insert("endproperty", (TokenKind::Keyword(Keyword::Endproperty), 5));
    m.insert("endsequence", (TokenKind::Keyword(Keyword::Endsequence), 5));
    m.insert("enum", (TokenKind::Keyword(Keyword::Enum), 5));
    m.insert("expect", (TokenKind::Keyword(Keyword::Expect), 5));
    m.insert("export", (TokenKind::Keyword(Keyword::Export), 5));
    m.insert("extends", (TokenKind::Keyword(Keyword::Extends), 5));
    m.insert("extern", (TokenKind::Keyword(Keyword::Extern), 5));
    m.insert("final", (TokenKind::Keyword(Keyword::Final), 5));
    m.insert("first_match", (TokenKind::Keyword(Keyword::FirstMatch), 5));
    m.insert("foreach", (TokenKind::Keyword(Keyword::Foreach), 5));
    m.insert("forkjoin", (TokenKind::Keyword(Keyword::Forkjoin), 5));
    m.insert("iff", (TokenKind::Keyword(Keyword::Iff), 5));
    m.insert("ignore_bins", (TokenKind::Keyword(Keyword::IgnoreBins), 5));
    m.insert(
        "illegal_bins",
        (TokenKind::Keyword(Keyword::IllegalBins), 5),
    );
    m.insert("import", (TokenKind::Keyword(Keyword::Import), 5));
    m.insert("inside", (TokenKind::Keyword(Keyword::Inside), 5));
    m.insert("int", (TokenKind::IntAtomTy(IntAtomTy::Int), 5));
    m.insert("interface", (TokenKind::Keyword(Keyword::Interface), 5));
    m.insert("intersect", (TokenKind::Keyword(Keyword::Intersect), 5));
    m.insert("join_any", (TokenKind::Keyword(Keyword::JoinAny), 5));
    m.insert("join_none", (TokenKind::Keyword(Keyword::JoinNone), 5));
    m.insert("local", (TokenKind::Keyword(Keyword::Local), 5));
    m.insert("logic", (TokenKind::IntVecTy(IntVecTy::Logic), 5));
    m.insert("longint", (TokenKind::IntAtomTy(IntAtomTy::Longint), 5));
    m.insert("matches", (TokenKind::Keyword(Keyword::Matches), 5));
    m.insert("modport", (TokenKind::Keyword(Keyword::Modport), 5));
    m.insert("new", (TokenKind::Keyword(Keyword::New), 5));
    m.insert("null", (TokenKind::Keyword(Keyword::Null), 5));
    m.insert("package", (TokenKind::Keyword(Keyword::Package), 5));
    m.insert("packed", (TokenKind::Keyword(Keyword::Packed), 5));
    m.insert("priority", (TokenKind::UniqPrio(UniqPrio::Priority), 5));
    m.insert("program", (TokenKind::Keyword(Keyword::Program), 5));
    m.insert("property", (TokenKind::Keyword(Keyword::Property), 5));
    m.insert("protected", (TokenKind::Keyword(Keyword::Protected), 5));
    m.insert("pure", (TokenKind::Keyword(Keyword::Pure), 5));
    m.insert("rand", (TokenKind::Keyword(Keyword::Rand), 5));
    m.insert("randc", (TokenKind::Keyword(Keyword::Randc), 5));
    m.insert("randcase", (TokenKind::Keyword(Keyword::Randcase), 5));
    m.insert(
        "randsequence",
        (TokenKind::Keyword(Keyword::Randsequence), 5),
    );
    m.insert("ref", (TokenKind::PortDir(PortDir::Ref), 5));
    m.insert("return", (TokenKind::Keyword(Keyword::Return), 5));
    m.insert("sequence", (TokenKind::Keyword(Keyword::Sequence), 5));
    m.insert("shortint", (TokenKind::IntAtomTy(IntAtomTy::Shortint), 5));
    m.insert("shortreal", (TokenKind::RealTy(RealTy::Shortreal), 5));
    m.insert("solve", (TokenKind::Keyword(Keyword::Solve), 5));
    m.insert("static", (TokenKind::Keyword(Keyword::Static), 5));
    m.insert("string", (TokenKind::Keyword(Keyword::String), 5));
    m.insert("struct", (TokenKind::Keyword(Keyword::Struct), 5));
    m.insert("super", (TokenKind::Keyword(Keyword::Super), 5));
    m.insert("tagged", (TokenKind::Keyword(Keyword::Tagged), 5));
    m.insert("this", (TokenKind::Keyword(Keyword::This), 5));
    m.insert("throughout", (TokenKind::Keyword(Keyword::Throughout), 5));
    m.insert(
        "timeprecision",
        (TokenKind::Keyword(Keyword::Timeprecision), 5),
    );
    m.insert("timeunit", (TokenKind::Keyword(Keyword::Timeunit), 5));
    m.insert("type", (TokenKind::Keyword(Keyword::Type), 5));
    m.insert("typedef", (TokenKind::Keyword(Keyword::Typedef), 5));
    m.insert("union", (TokenKind::Keyword(Keyword::Union), 5));
    m.insert("unique", (TokenKind::UniqPrio(UniqPrio::Unique), 5));
    m.insert("var", (TokenKind::Keyword(Keyword::Var), 5));
    m.insert("virtual", (TokenKind::Keyword(Keyword::Virtual), 5));
    m.insert("void", (TokenKind::Keyword(Keyword::Void), 5));
    m.insert("wait_order", (TokenKind::Keyword(Keyword::WaitOrder), 5));
    m.insert("wildcard", (TokenKind::Keyword(Keyword::Wildcard), 5));
    m.insert("with", (TokenKind::Keyword(Keyword::With), 5));
    m.insert("within", (TokenKind::Keyword(Keyword::Within), 5));

    // SV 09
    m.insert("accept_on", (TokenKind::Keyword(Keyword::AcceptOn), 6));
    m.insert("checker", (TokenKind::Keyword(Keyword::Checker), 6));
    m.insert("endchecker", (TokenKind::Keyword(Keyword::Endchecker), 6));
    m.insert("eventually", (TokenKind::Keyword(Keyword::Eventually), 6));
    m.insert("global", (TokenKind::Keyword(Keyword::Global), 6));
    m.insert("implies", (TokenKind::Keyword(Keyword::Implies), 6));
    m.insert("let", (TokenKind::Keyword(Keyword::Let), 6));
    m.insert("nexttime", (TokenKind::Keyword(Keyword::Nexttime), 6));
    m.insert("reject_on", (TokenKind::Keyword(Keyword::RejectOn), 6));
    m.insert("restrict", (TokenKind::Keyword(Keyword::Restrict), 6));
    m.insert("s_always", (TokenKind::Keyword(Keyword::SAlways), 6));
    m.insert(
        "s_eventually",
        (TokenKind::Keyword(Keyword::SEventually), 6),
    );
    m.insert("s_nexttime", (TokenKind::Keyword(Keyword::SNexttime), 6));
    m.insert("s_until", (TokenKind::Keyword(Keyword::SUntil), 6));
    m.insert("s_until_with", (TokenKind::Keyword(Keyword::SUntilWith), 6));
    m.insert("strong", (TokenKind::Keyword(Keyword::Strong), 6));
    m.insert(
        "sync_accept_on",
        (TokenKind::Keyword(Keyword::SyncAcceptOn), 6),
    );
    m.insert(
        "sync_reject_on",
        (TokenKind::Keyword(Keyword::SyncRejectOn), 6),
    );
    m.insert("unique0", (TokenKind::UniqPrio(UniqPrio::Unique0), 6));
    m.insert("until", (TokenKind::Keyword(Keyword::Until), 6));
    m.insert("until_with", (TokenKind::Keyword(Keyword::UntilWith), 6));
    m.insert("untyped", (TokenKind::Keyword(Keyword::Untyped), 6));
    m.insert("weak", (TokenKind::Keyword(Keyword::Weak), 6));

    // SV 12
    m.insert("implements", (TokenKind::Keyword(Keyword::Implements), 7));
    m.insert(
        "interconnect",
        (TokenKind::Keyword(Keyword::Interconnect), 7),
    );
    m.insert("nettype", (TokenKind::Keyword(Keyword::Nettype), 7));
    m.insert("soft", (TokenKind::Keyword(Keyword::Soft), 7));

    // SV 17
    // No new keywords

    m
});
