#![warn(dead_code)]

use super::super::number::{LogicValue, LogicNumber};

use std::fmt;
use std::collections::VecDeque;

use super::ast::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    // Symbol delimiter
    Paren,
    Bracket,
    Brace,
    Attr,
    /// Open-only delimiter "'{". The corresponding close delimiter should be "'}".
    TickBrace,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    // System task identifiers that are treated as keyword
    Unit,
    Root,

    // Verilog 95
    And,
    Assign,
    Begin,
    Deassign,
    Default,
    Defparam,
    Disable,
    Else,
    End,
    Endcase,
    Endmodule,
    Endfunction,
    Endprimitive,
    Endspecify,
    Endtable,
    Endtask,
    Event,
    For,
    Force,
    Forever,
    Fork,
    Function,
    If,
    Ifnone,
    Initial,
    Join,
    Module,
    Not,
    Or,
    Parameter,
    Primitive,
    Reg,
    Release,
    Repeat,
    Scalared,
    Specify,
    Specparam,
    Table,
    Task,
    Vectored,
    Wait,
    While,
    Xor,

    // Verilog 01-noconfig
    Automatic,
    Endgenerate,
    Generate,
    Genvar,
    Localparam,
    Noshowcancelled,
    PulsestyleOndetect,
    PulsestyleOnevent,
    Showcancelled,

    // Verilog 01
    Cell,
    Config,
    Design,
    Endconfig,
    Incdir,
    Include,
    Instance,
    Liblist,
    Library,
    Use,

    // Verilog 05

    // SV 05
    Alias,
    Assert,
    Assume,
    Before,
    Bind,
    Bins,
    Binsof,
    Break,
    Chandle,
    Class,
    Clocking,
    Const,
    Constraint,
    Context,
    Continue,
    Cover,
    Covergroup,
    Coverpoint,
    Cross,
    Dist,
    Do,
    Endclass,
    Endclocking,
    Endgroup,
    Endinterface,
    Endpackage,
    Endprogram,
    Endproperty,
    Endsequence,
    Enum,
    Expect,
    Export,
    Extends,
    Extern,
    Final,
    FirstMatch,
    Foreach,
    Forkjoin,
    Iff,
    IgnoreBins,
    IllegalBins,
    Import,
    Inside,
    Interface,
    Intersect,
    JoinAny,
    JoinNone,
    Local,
    Matches,
    Modport,
    New,
    Null,
    Package,
    Packed,
    Program,
    Property,
    Protected,
    Pure,
    Rand,
    Randc,
    Randcase,
    Randsequence,
    Return,
    Sequence,
    Solve,
    Static,
    String,
    Struct,
    Super,
    Tagged,
    This,
    Throughout,
    Timeprecision,
    Timeunit,
    Type,
    Typedef,
    Union,
    Var,
    Virtual,
    Void,
    WaitOrder,
    Wildcard,
    With,
    Within,

    // SV 09
    AcceptOn,
    Checker,
    Endchecker,
    Eventually,
    Global,
    Implies,
    Let,
    Nexttime,
    RejectOn,
    Restrict,
    SAlways,
    SEventually,
    SNexttime,
    SUntil,
    SUntilWith,
    Strong,
    SyncAcceptOn,
    SyncRejectOn,
    Until,
    UntilWith,
    Untyped,
    Weak,

    // SV 12
    Implements,
    Interconnect,
    Nettype,
    Soft,

    // SV 17
    // No new keywords
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Keyword::Module => "module",
            Keyword::Interface => "interface",
            Keyword::Package => "package",
            _ => {
                return write!(f, "{:?} unimp", self);
            }
        };
        write!(f, "{}", str)
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// This is a keyword
    Keyword(Keyword),
    /// An opening delimiter
    OpenDelim(Delim),
    CloseDelim(Delim),

    //
    // Operator groups
    //
    // Single character operators
    /// "="
    Assign,
    /// "?"
    Question,
    /// "'"
    Tick,
    /// "$"
    Dollar,
    /// "@"
    At,
    /// "#"
    Hash,
    /// "."
    Dot,
    /// ","
    Comma,
    /// ":"
    Colon,
    /// ";"
    Semicolon,

    // Multi-character operator
    /// "@*"
    AtStar,
    /// "@@"
    AtAt,
    /// "##"
    CycleDelay,
    /// "(*)"
    ParenedStar,
    /// "=>"
    ParConnect,
    /// "*>"
    FullConnect,
    /// "&&&"
    TripleAnd,
    /// "+:"
    PlusColon,
    /// "-:"
    MinusColon,
    /// ".*"
    WildPattern,
    /// "->>""
    NonblockTrigger,
    /// "::"
    ScopeSep,
    /// ":="
    DistEq,
    /// ":/"
    DistDiv,
    /// "|->"
    OverlapImply,
    /// "|=>"
    NonOverlapImply,
    /* We parse the following operators and a combinational of operators for
     * simplicity. The standard seems to treat them as a single symbol though,
     * but that design isn't really friendly for non-generated tokenizers.
     * // "[=]"
     * Repeat,
     * // "[="
     * RepeatStart,
     * // "[*]"
     * ConsecutiveRepeat,
     * // "[*"
     * ConsecutiveRepeatStart,
     * // "[+]"
     * ConsecutiveRepeatPlus,
     * // "[->]"
     * GotoRepeat,
     * // "[->"
     * GotoRepeatStart,
     */

    /// Represent an unary operator.
    /// Exclude +, -, &, |, ^, ~^, which will be parsed as BinaryOp
    UnaryOp(UnaryOp),
    /// Represent a binary operator.
    /// Note that +, -, *, /, &, |, ^, ~^, ->, >>, <= will all be tokenized as BinaryOp but
    /// has special meaning elsewhere.
    BinaryOp(BinaryOp),
    /// "<<". Can be used interchangably with <<< except that "<<" can also start a streaming
    /// concatenation.
    LShl,
    /// Represent a binary operator with assignment.
    BinaryOpAssign(BinaryOp),
    /// Represent either ++ or --.
    IncDec(IncDec),

    //
    // Keyword groups
    //
    PortDir(PortDir),
    AlwaysKw(AlwaysKw),
    Signing(Signing),
    IntAtomTy(IntAtomTy),
    IntVecTy(IntVecTy),
    RealTy(RealTy),
    NetTy(NetTy),
    /// Exclude supply0, which will be parsed as NetTy
    Strength0(DriveStrength),
    /// Exclude supply1, which will be parsed as NetTy
    Strength1(DriveStrength),
    ChargeStrength(ChargeStrength),
    Edge(Edge),
    UniqPrio(UniqPrio),
    CaseKw(CaseKw),
    /// Exclude not, and, or, xor, which will be parsed as keyword,
    Primitive(Primitive),

    /// Identifier
    Id(String),
    /// A system-task identifier
    SystemTask(String),

    // Literals
    StringLiteral(String),
    TimeLiteral(f64),
    RealLiteral(f64),
    IntegerLiteral(LogicNumber),
    UnbasedLiteral(LogicValue),

    /// An unknown token. For error recovery purposes
    Unknown,

    /// Represent a delimited group of token
    /// We borrowed Rust libsyntax's token tree concept, but we tweaked it for simplicity
    DelimGroup(Delim, Box<DelimGroup>),

    /// Preprocessing directive. Will not exist after preprocessor
    Directive(String),

    // These tokens are normally ignored, but we keep them here so that the lexer is also useful
    // potentially for code formatting purposes.
    Eof,
    Whitespace,
    NewLine,
    LineComment,
    BlockComment,
}

/// A token with span information
pub type Token = Spanned<TokenKind>;

/// A delimited group of token
#[derive(Debug, Clone)]
pub struct DelimGroup {
    pub open: Token,
    pub close: Token,
    pub tokens: VecDeque<Token>,
}

/// This is just a placeholder to allow partial eq to be used on TokenKind. Should never be
/// executed.
impl PartialEq for DelimGroup {
    fn eq(&self, _other: &Self) -> bool {
        unreachable!()
    }
}
