use super::super::number::{LogicValue, LogicNumber};
use super::kw::Keyword;
use super::super::source::Spanned;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    // Symbol delimiter
    Paren,
    Bracket,
    Brace,
    Attr,
    /// Open-only delimiter "'{". The corresponding close delimiter should be "'}".
    TickBrace,

    // Keyword delimiters
    Block,
    Module,
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
    // "#"
    Hash,
    // ","
    Comma,
    // "."
    Dot,
    // ":"
    Colon,
    // ";"
    Semicolon,
    // "<"
    Lt,
    // ">"
    Gt,
    // "?"
    Question,
    // "@" 
    At,
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
    // "'{"
    TickBrace,
    // "++"
    Inc,
    // "--"
    Dec,
    // "->>""
    NonblockTrigger,
    // "##"
    CycleDelay,
    // "@@"
    AtAt,
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

#[derive(Debug, Clone)]
pub enum TokenKind {
    /// This is an expression operator or structure symbols
    Operator(Operator),
    /// This is a keyword
    Keyword(Keyword),
    /// An opening delimiter
    OpenDelim(Delim),
    CloseDelim(Delim),

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

lazy_static!{
pub static ref EOF: Token = {
    Spanned::new_unspanned(TokenKind::Eof)
};
}

impl Token {
    pub fn eof() -> Token {
        Spanned::new_unspanned(TokenKind::Eof)
    }

    pub fn eof_ref() -> &'static Token {
        &EOF
    }
}
