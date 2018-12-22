use super::super::source::Span;
use super::super::syntax::tokens::{Token, Keyword};
use std::fmt;
use std::ops::{Deref, DerefMut};

//
// General purpose helpers
//

pub trait AstNode where Self: Sized {
    /// An user-friendly name for error message
    fn name() -> &'static str;

    /// Given a span, return an `Self` for error-recovery purpose.
    /// If `None` is returned, a fatal error will be thrown.
    fn recovery(_: Span) -> Option<Self> {
        None
    }
}

/// Represent an object with span information
#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned {
            value: value,
            span: span,
        }
    }

    pub fn new_unspanned(value: T) -> Spanned<T> {
        Spanned {
            value: value,
            span: Span::none(),
        }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T: fmt::Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}


//
// Keyword enums
//

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortDir {
    Input,
    Output,
    Inout,
    Ref,
}

impl fmt::Display for PortDir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PortDir::Input => write!(f, "input"),
            PortDir::Output => write!(f, "output"),
            PortDir::Inout => write!(f, "inout"),
            PortDir::Ref => write!(f, "ref"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AlwaysKw {
    Always,
    AlwaysComb,
    AlwaysLatch,
    AlwaysFf,
}

impl fmt::Display for AlwaysKw {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            AlwaysKw::Always => "always",
            AlwaysKw::AlwaysComb => "always_comb",
            AlwaysKw::AlwaysLatch => "always_latch",
            AlwaysKw::AlwaysFf => "always_ff",
        })
    }
}

/// Represent a built-in atomic integer type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntAtomTy {
    Byte,
    Shortint,
    Int,
    Longint,
    Integer,
    Time,
}

impl fmt::Display for IntAtomTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            IntAtomTy::Byte => "byte",
            IntAtomTy::Shortint => "shortint",
            IntAtomTy::Int => "int",
            IntAtomTy::Longint => "longint",
            IntAtomTy::Integer => "integer",
            IntAtomTy::Time => "time",
        })
    }
}

/// Represent a built-in vectored integer base type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntVecTy {
    Bit,
    Logic,
}

impl fmt::Display for IntVecTy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", if self == &IntVecTy::Bit { "bit" } else { "logic" })
    }
}

/// Represent a built-in non-integer type. The spec says that realtime is an alias to real.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RealTy {
    Real,
    Shortreal,
}

/// Represent a built-in net-type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NetTy {
    Supply0,
    Supply1,
    Tri,
    Triand,
    Trior,
    Trireg,
    Tri0,
    Tri1,
    Uwire,
    Wire,
    Wand,
    Wor,
}

/// Represent a drive strength
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DriveStrength {
    Supply,
    Strong,
    Pull,
    Weak,
    Highz,
}

/// Represent a charge strength
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChargeStrength {
    Small,
    Medium,
    Large,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signing {
    Signed,
    Unsigned,
}

impl fmt::Display for Signing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Signing::Signed => "signed",
            Signing::Unsigned => "unsigned",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    Posedge,
    Negedge,
    Edge,
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Edge::Posedge => "posedge",
            Edge::Negedge => "negedge",
            Edge::Edge => "edge",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UniqPrio {
    Unique,
    Unique0,
    Priority,
}

impl fmt::Display for UniqPrio {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UniqPrio::Unique => "unique",
            UniqPrio::Unique0 => "unique0",
            UniqPrio::Priority => "priority",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseKw {
    Case,
    Casez,
    Casex,
}

impl fmt::Display for CaseKw {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            CaseKw::Case => "case",
            CaseKw::Casez => "casez",
            CaseKw::Casex => "casex",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Primitive {
    Cmos,
    Rcmos,
    Bufif0,
    Bufif1,
    Notif0,
    Notif1,
    Nmos,
    Pmos,
    Rnmos,
    Rpmos,
    And,
    Nand,
    Or,
    Nor,
    Xor,
    Xnor,
    Buf,
    Not,
    Tranif0,
    Tranif1,
    Rtranif1,
    Rtranif0,
    Tran,
    Rtran,
    Pullup,
    Pulldown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncDec {
    Inc,
    Dec,
}

impl fmt::Display for IncDec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            IncDec::Inc => "++",
            IncDec::Dec => "--",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Add,
    Sub,
    LNot,
    Not,
    And,
    Nand,
    Or,
    Nor,
    Xor,
    Xnor,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Add => "+",
            UnaryOp::Sub => "-",
            UnaryOp::LNot => "!",
            UnaryOp::Not => "~",
            UnaryOp::And => "&",
            UnaryOp::Nand => "~&",
            UnaryOp::Or => "|",
            UnaryOp::Nor => "~|",
            UnaryOp::Xor => "^",
            UnaryOp::Xnor => "~^",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    CaseEq,
    CaseNeq,
    WildEq,
    WildNeq,
    LAnd,
    LOr,
    Power,
    Lt,
    Leq,
    Gt,
    Geq,
    And,
    Or,
    Xor,
    Xnor,
    Shl,
    LShr,
    AShr,
    Imply,
    Equiv,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Neq => "!=",
            BinaryOp::CaseEq => "===",
            BinaryOp::CaseNeq => "!==",
            BinaryOp::WildEq => "==?",
            BinaryOp::WildNeq => "!=?",
            BinaryOp::LAnd => "&&",
            BinaryOp::LOr => "||",
            BinaryOp::Power => "**",
            BinaryOp::Lt => "<",
            BinaryOp::Leq => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Geq => ">=",
            BinaryOp::And => "&",
            BinaryOp::Or => "|",
            BinaryOp::Xor => "^",
            BinaryOp::Xnor => "~^",
            BinaryOp::Shl => "<<",
            BinaryOp::LShr => ">>",
            BinaryOp::AShr => ">>>",
            BinaryOp::Imply => "->",
            BinaryOp::Equiv => "<->",
        })
    }
}

//
// A.1.2 SystemVerilog source text
//

#[derive(Debug, Clone)]
pub enum Item {
    DesignDecl(Box<DesignDecl>),
    PkgImport(Vec<PkgImportItem>),
    ParamDecl(Box<ParamDecl>),
    DataDecl(Box<DataDecl>),

    /// Standard typedef
    Typedef(Option<Box<AttrInst>>, Box<DataType>, Box<Ident>, Vec<Dim>),

    /// Typedef that imports data types from interface
    TypedefIntf(Option<Box<AttrInst>>, Box<Expr>, Box<Ident>, Box<Ident>),

    ContinuousAssign(Vec<Expr>),
    Initial(Box<Stmt>),
    Always(AlwaysKw, Box<Stmt>),

    HierInstantiation(Box<HierInstantiation>),

    GenRegion(Vec<Item>),
    LoopGen(Box<LoopGen>),
    IfGen(Box<IfGen>),
    SysTfCall(Box<SysTfCall>),

    ModportDecl(Option<Box<AttrInst>>, Vec<(Ident, Vec<ModportPortDecl>)>),
}

impl AstNode for Item {
    fn name() -> &'static str { "item" }
}

/// Declaration of module, interface, program or package
#[derive(Debug, Clone)]
pub struct DesignDecl {
    pub attr: Option<Box<AttrInst>>,
    pub kw: Keyword,
    pub lifetime: Lifetime,
    pub name: Ident,
    pub pkg_import: Vec<Vec<PkgImportItem>>,
    pub param: Option<Vec<ParamDecl>>,
    pub port: Vec<PortDecl>,
    pub items: Vec<Item>,
}

//
// A.1.3 Module parameters and ports
//

/// AST for parameter_declaration or localparam_declaration
#[derive(Debug, Clone)]
pub struct ParamDecl {
    // Parameter or localparam
    pub kw: Keyword,
    pub ty: Option<Box<DataType>>,
    pub list: Vec<DeclAssign>,
}

/// The type of ANSI port
#[derive(Debug, Clone)]
pub enum PortDecl {
    Data(PortDir, NetPortType, Box<DataType>, Vec<DeclAssign>),
    Interface(Option<Box<Ident>>, Option<Box<Ident>>, Vec<DeclAssign>),
    Explicit(PortDir, Box<Ident>, Box<Expr>),
}

//
// A.2.1.3 Type declarations
//

#[derive(Debug, Clone)]
pub struct PkgImportItem(pub Ident, pub Option<Ident>);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lifetime {
    Static,
    Automatic,
}

//
// A.2.2.1 Net and variable types
//

/// Represent a data_type_or_implicit. We have merged implicit here to simplify code, but if
/// explicit data_type is required a check is needed.
#[derive(Debug, Clone)]
pub enum DataTypeKind {
    /// This isn't really a data type, but it is more convinient to have it here.
    Type,
    Implicit(Signing, Vec<Dim>),
    IntVec(IntVecTy, Signing, Vec<Dim>),
    IntAtom(IntAtomTy, Option<Signing>),
    Real(RealTy),
    Aggr(AggrDecl, Vec<Dim>),
    Enum(EnumDecl, Vec<Dim>),
    String,
    Chandle,
    VirtualInterface, // TODO
    Event,
    /// A hierahical name. Could possibly be typedef'd type, class type or covergroup identifier.
    HierName(Option<Scope>, Ident, Vec<Dim>),
    /// Type reference of form type'(expr_or_data_type)
    TypeRef(Box<Expr>),
    /// Void type
    Void,
}

/// Should be boxed when nested in other AST structure.
pub type DataType = Spanned<DataTypeKind>;

impl AstNode for DataType {
    fn name() -> &'static str {
        "data type"
    }
}

/// Represent a net_port_type (but without data type)
#[derive(Debug, Clone)]
pub enum NetPortType {
    Builtin(NetTy),
    UserDefined(Box<Ident>),
    Interconnect,
    /// This is actually a variable port. We put it here for simplicity, as PortType
    /// is the only time this enum is ever used.
    Variable,
    /// It should have default net type
    Default,
}

#[derive(Debug, Clone)]
pub struct DataDecl {
    pub attr: Option<Box<AttrInst>>,
    pub has_const: bool,
    pub lifetime: Lifetime,
    pub ty: DataType,
    pub list: Vec<DeclAssign>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AggrType {
    Struct,
    Union,
    TaggedUnion,
}

impl fmt::Display for AggrType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            AggrType::Struct => "struct",
            AggrType::Union => "union",
            AggrType::TaggedUnion => "union tagged",
        })
    }
}

/// Represent a struct_union
#[derive(Debug, Clone)]
pub struct AggrDecl {
    pub kind: AggrType,
    pub packed: bool,
    pub sign: Signing,
    pub members: Vec<AggrMember>,
}

/// Represent a struct_union_member
#[derive(Debug, Clone)]
pub struct AggrMember {
    pub attr: Option<Box<AttrInst>>,
    // TODO: Random qualifier
    pub ty: DataType,
    pub list: Vec<DeclAssign>,
}

/// Represent a enum_declaration
#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub ty: Option<Box<DataType>>,
    pub members: Vec<DeclAssign>,
}

//
// A.2.4 Declaration assignments
//

/// Most common declaration assignment
#[derive(Debug, Clone)]
pub struct DeclAssign {
    pub name: Ident,
    pub dim: Vec<Dim>,
    pub init: Option<Box<Expr>>,
}

impl AstNode for DeclAssign {
    fn name() -> &'static str { "declaration assignment" }
}

//
// A.2.5 Declaration ranges
//

/// Possible ways of specifying a variable dimension
#[derive(Debug, Clone)]
pub enum DimKind {
    /// Represent bit-select/dimension of type `[ expression ]`.
    /// It can also represent dimension of type `[ data_type ]`, but the resolution will not occur
    /// during parsing.
    Value(Box<Expr>),
    /// Represent bit-select/dimension of type `[ expression : expression ]`
    /// It can also represent a queue dimension with optional max size, of type
    /// `[ $ [ : constant expression ] ]` but the resolution will not occur during parsing.
    Range(Box<Expr>, Box<Expr>),
    /// Represent bit-select of type `[ expression +: expression ]`
    PlusRange(Box<Expr>, Box<Expr>),
    /// Represent bit-select of type `[ expression -: expression ]`
    MinusRange(Box<Expr>, Box<Expr>),
    /// Represent dimension of type `[]`
    Unsized,
    /// Represent dimension of type `[ * ]`
    AssocWild,
}


/// Should be boxed when nested in other AST structure.
pub type Dim = Spanned<DimKind>;

//
// A.2.9 Interface declarations
//

/// The type of modport port
#[derive(Debug, Clone)]
pub enum ModportPortDecl {
    Simple(Option<Box<AttrInst>>, PortDir, Vec<ModportSimplePort>),
    Clocking(Option<Box<AttrInst>>, Box<Ident>),
    // TODO: modport_tf_port
}

#[derive(Debug, Clone)]
pub enum ModportSimplePort {
    Named(Ident),
    Explicit(Ident, Box<Expr>),
}

//
// A.4.1.1 Module instantiations
//

#[derive(Debug, Clone)]
pub struct HierInst {
    pub name: Ident,
    pub dim: Vec<Dim>,
    pub ports: Args,
}

#[derive(Debug, Clone)]
pub struct HierInstantiation {
    pub attr: Option<Box<AttrInst>>,
    pub name: Ident,
    pub param: Option<Args>,
    pub inst: Vec<HierInst>,
}

#[derive(Debug, Clone)]
pub struct Args {
    pub ordered: Vec<(Option<Box<AttrInst>>, Option<Box<Expr>>)>,
    pub named: Vec<(Option<Box<AttrInst>>, Box<Ident>, Option<Box<Expr>>)>,
    pub has_wildcard: bool,
}

impl AstNode for Args {
    fn name() -> &'static str { "arguments" }
}

//
// A.4.2 Generate instantiations
//

#[derive(Debug, Clone)]
pub struct LoopGen {
    pub attr: Option<Box<AttrInst>>,
    pub genvar: bool,
    pub id: Ident,
    pub init: Expr,
    pub cond: Expr,
    pub update: Expr,
    pub block: GenBlock,
}

#[derive(Debug, Clone)]
pub struct IfGen {
    pub attr: Option<Box<AttrInst>>,
    pub if_block: Vec<(Expr, GenBlock)>,
    pub else_block: Option<Box<GenBlock>>,
}

#[derive(Debug, Clone)]
pub struct GenBlock {
    pub name: Option<Box<Ident>>,
    pub items: Vec<Item>,
}

//
// A.6.4 Statements
//

#[derive(Debug, Clone)]
pub enum StmtKind {
    Empty,
    TimingCtrl(TimingCtrl, Box<Stmt>),
    If(Option<UniqPrio>, Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    SeqBlock(Vec<Stmt>),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub label: Option<Box<Ident>>,
    pub attr: Option<Box<AttrInst>>,
    pub value: StmtKind,
}

impl AstNode for Stmt {
    fn name() -> &'static str { "statement" }
}

//
// A.6.5 Timing control statements
//

#[derive(Debug, Clone)]
pub struct EventExprItem {
    pub edge: Option<Edge>,
    pub expr: Box<Expr>,
    pub iff: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub enum EventExpr {
    Item(Box<EventExprItem>),
    List(Vec<EventExpr>),
    Paren(Box<EventExpr>),
}

#[derive(Debug, Clone)]
pub enum TimingCtrl {
    DelayCtrl(Box<Expr>),
    ExprEventCtrl(Box<EventExpr>),
    NameEventCtrl(Option<Scope>, HierId),
    ImplicitEventCtrl,
    // CycleDelay
}

//
// A.6.7.1 Patterns
//

/// Represent an assignment pattern.
#[derive(Debug, Clone)]
pub enum AssignPattern {
    Simple(Vec<Expr>),
    Keyed(Vec<(Expr, Expr)>),
    Mult(Box<Expr>, Vec<Expr>),
}

//
// A.8.2 Subroutine call
//

#[derive(Debug, Clone)]
pub struct SysTfCall {
    pub task: Spanned<String>,
    pub args: Option<Args>,
}

//
// A.8.3 Expressions
//

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// As in many cases expression and type can occur in a same context, we have
    /// `ExprKind::Type` in the enum to represent the case where we know "this is definitely a
    /// type". In some cases other expression can also be viewed as type, e.g. `id[x]`
    Type(Box<DataType>),
    Literal(Token),
    
    /// A hierachical name
    HierName(Option<Scope>, HierId),

    /// Empty queue initializer ("{}")
    EmptyQueue,

    /// Concatenation
    Concat(Vec<Expr>),

    /// Multiple concatenation
    MultConcat(Box<Expr>, Box<Expr>),

    /// Assignment pattern expression
    AssignPattern(Option<Box<DataType>>, AssignPattern),

    /// Element select
    Select(Box<Expr>, Dim),

    /// Member access
    Member(Box<Expr>, Ident),

    // Subroutine calls
    /// Call to system task
    SysTfCall(Box<SysTfCall>),

    // Casts
    ConstCast(Box<Expr>),
    SignCast(Signing, Box<Expr>),
    TypeCast(Box<Expr>, Box<Expr>),

    Unary(UnaryOp, Option<Box<AttrInst>>, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Option<Box<AttrInst>>, Box<Expr>),
    PrefixIncDec(IncDec, Option<Box<AttrInst>>, Box<Expr>),
    PostfixIncDec(Box<Expr>, Option<Box<AttrInst>>, IncDec),

    /// Assignment
    Assign(Box<Expr>, Box<Expr>),
    BinaryAssign(Box<Expr>, BinaryOp, Box<Expr>),

    /// Parenthesised expression
    Paren(Box<Expr>),

    /// Min-typ-max expression
    MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Conditional expression
    Cond(Box<Expr>, Option<Box<AttrInst>>, Box<Expr>, Box<Expr>),
}

pub type Expr = Spanned<ExprKind>;

impl AstNode for Expr {
    fn name() -> &'static str {
        "expression"
    }
}

//
// A.9.1 Attributes
//

#[derive(Debug, Clone)]
pub struct AttrSpec {
    pub name: Ident,
    pub expr: Option<Box<Expr>>
}

#[derive(Debug, Clone)]
pub struct AttrInstStruct(pub Vec<AttrSpec>);

pub type AttrInst = Spanned<AttrInstStruct>;

//
// A.9.3 Identifiers
//

#[derive(Debug, Clone)]
pub enum Scope {
    /// $unit scope
    Unit,
    /// local scope
    Local,
    /// a named scope, can possibily be nested with in a outer scope
    Name(Option<Box<Scope>>, Box<Ident>),
}

#[derive(Debug, Clone)]
pub enum HierId {
    /// $root
    Root,
    /// this
    This,
    /// super or this.super
    Super,
    /// a named identifier, can possibily has a parent id
    Name(Option<Box<HierId>>, Box<Ident>)
}

impl AstNode for HierId {
    fn name() -> &'static str { "hierachical identifier" }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

impl SymbolId {
    pub const DUMMY: Self = SymbolId(std::u32::MAX);
}

/// Represent an identifier. In additional to value and span as Spanned<T> have, it also contains
/// a SymbolId field which will be filled by resolver.
#[derive(Clone)]
pub struct Ident {
    pub value: String,
    pub span: Span,
    pub symbol: SymbolId,
}

impl Ident {
    pub fn new(value: String, span: Span) -> Self {
        Self {
            value: value,
            span: span,
            symbol: SymbolId::DUMMY,
        }
    }

    pub fn new_unspanned(value: String) -> Self {
        Self {
            value: value,
            span: Span::none(),
            symbol: SymbolId::DUMMY,
        }
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &String {
        &self.value
    }
}

impl DerefMut for Ident {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.value
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.symbol == SymbolId::DUMMY {
            write!(f, "/*unresolved*/{}", self.value)
        } else {
            self.value.fmt(f)
        }
    }
}

impl PartialEq for Ident {
    fn eq(&self, rhs: &Self) -> bool {
        self.value == rhs.value
    }
}
