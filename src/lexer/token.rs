use super::super::number::{LogicValue, LogicNumber};
use super::kw::Keyword;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    // Delimiters
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    OpenAttr,
    CloseAttr,

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
pub enum Token {
    Eof,
    Unknown,
    Whitespace,
    NewLine,
    LineComment,
    BlockComment,

    Operator(Operator),

    Id(String),
    Keyword(Keyword),
    SystemTask(String),

    StringLiteral(String),
    TimeLiteral(f64),
    RealLiteral(f64),
    IntegerLiteral(LogicNumber),
    UnbasedLiteral(LogicValue)
}
