use super::super::number::{LogicValue, LogicNumber};
use super::kw::Keyword;
use super::super::source::Spanned;

use std::fmt;
use std::collections::VecDeque;

use parser::ast::*;

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
pub enum Operator {
    // Binary Ops
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    LShl,
    LShr,
    AShl,
    AShr,

    // Binary Ops Assign
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    AndEq,
    OrEq,
    XorEq,
    LShlEq,
    LShrEq,
    AShlEq,
    AShrEq,

    // Single character operators
    // "="
    Assign,
    // "!"
    LNot,
    // "~",
    Not,
    // "."
    Dot,
    // "<"
    Lt,
    // ">"
    Gt,
    // "?"
    Question,
    // "'"
    Tick,
    // "$"
    Dollar,

    // Multi-character operator
    // "&&"
    LAnd,
    // "||",
    LOr,
    // "<="
    Leq,
    // ">=",
    Geq,
    // "=="
    Eq,
    // "!="
    Neq,
    // "==="
    CaseEq,
    // "!=="
    CaseNeq,
    // "==?"
    WildEq,
    // "!=?"
    WildNeq,
    // "~&"
    Nand,
    // "~|"
    Nor,
    // "~^" or "^~"
    Xnor,
    // "->"
    Implies,
    // "<->"
    Equiv,
    // "=>"
    ParConnect,
    // "*>"
    FullConnect,
    // "&&&"
    TripleAnd,
    // "**"
    Power,
    // "+:"
    PlusColon,
    // "-:"
    MinusColon,
    // ".*"
    WildPattern,
    // "++"
    Inc,
    // "--"
    Dec,
    // "->>""
    NonblockTrigger,
    // "::"
    ScopeSep,
    // ":="
    DistEq,
    // ":/"
    DistDiv,
    // "|->"
    OverlapImply,
    // "|=>"
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
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Mod => "%",
            Operator::And => "&",
            Operator::Or => "|",
            Operator::Xor => "^",
            Operator::LShl => "<<",
            Operator::LShr => ">>",
            Operator::AShl => "<<<",
            Operator::AShr => ">>>",
            Operator::Assign => "=",
            Operator::Lt => "<",
            Operator::Gt => ">",
            Operator::LAnd => "&&",
            Operator::LOr => "||",
            Operator::Eq => "==",
            Operator::Neq => "!=",
            Operator::Inc => "++",
            Operator::Dec => "--",
            _ => {
                return write!(f, "{:?} unimp", self);
            }
        };
        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    /// This is an expression operator or structure symbols
    Operator(Operator),
    /// This is a keyword
    Keyword(Keyword),
    /// An opening delimiter
    OpenDelim(Delim),
    CloseDelim(Delim),

    //
    // Operator groups
    //
    /// "@"
    At,
    /// "#"
    Hash,
    /// ","
    Comma,
    /// ":"
    Colon,
    /// ";"
    Semicolon,

    /// "@*"
    AtStar,
    /// "@@"
    AtAt,
    /// "##"
    CycleDelay,
    /// "(*)"
    ParenedStar,

    //
    // Keyword groups
    //
    PortDir(PortDir),
    AlwaysKw(AlwaysKw),
    Signing(Signing),
    IntAtomTy(IntAtomTy),
    IntVecTy(IntVecTy),
    NonIntTy(NonIntTy),
    NetTy(NetTy),
    /// Exclude supply0, which will be parsed as NetTy
    Strength0(DriveStrength),
    /// Exclude supply1, which will be parsed as NetTy
    Strength1(DriveStrength),
    ChargeStrength(ChargeStrength),
    Edge(Edge),
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
