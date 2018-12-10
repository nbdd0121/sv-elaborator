use super::super::source::{Span, Spanned};
use super::super::lexer::{Token, Keyword, Operator};
use std::fmt;

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
pub enum NonIntTy {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Edge {
    Posedge,
    Negedge,
    Edge,
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

//
// Unknown
//

#[derive(Debug)]
pub enum Item {
    TimeunitDecl,
    DesignDecl(Box<DesignDecl>),
    UdpDecl,
    BindDirective,
    ConfigDecl,
    PkgImport(Vec<PkgImportItem>),
    ParamDecl(Box<ParamDecl>),
    DataDecl(Box<DataDecl>),

    ContinuousAssign(Vec<Expr>),
    Always(AlwaysKw, Box<Stmt>),

    HierInstantiation(Box<HierInstantiation>),

    GenRegion(Vec<Item>),
    LoopGen(Box<LoopGen>),
    IfGen(Box<IfGen>),
    GenBlock(Box<GenBlock>),
    SysTfCall(Box<SysTfCall>),
}

impl AstNode for Item {
    fn name() -> &'static str { "item" }
}

//
// A.1.2 SystemVerilog source text
//

/// Declaration of module, interface, program or package
#[derive(Debug)]
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
#[derive(Debug)]
pub struct ParamDecl {
    // Parameter or localparam
    pub kw: Keyword,
    pub ty: Option<Box<DataType>>,
    pub list: Vec<DeclAssign>,
}

/// The type of ANSI port
#[derive(Debug)]
pub enum PortDecl {
    Data(PortDir, NetPortType, Box<DataType>, Vec<DeclAssign>),
    Interface(Option<Box<Ident>>, Option<Box<Ident>>, Vec<DeclAssign>),
    Explicit(PortDir, Box<Ident>, Box<Expr>),
}

//
// A.2.1.3 Type declarations
//

#[derive(Debug)]
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
#[derive(Debug)]
pub enum DataTypeKind {
    /// This isn't really a data type, but it is more convinient to have it here.
    Type,
    Implicit(Signing, Vec<Dim>),
    IntVec(IntVecTy, Signing, Vec<Dim>),
    IntAtom(IntAtomTy, Signing),
    NonIntType(NonIntTy),
    StructUnion, // TODO
    Enum, // TODO
    String,
    Chandle,
    VirtualInterface, // TODO
    Event,
    /// A hierahical name. Could possibly be typedef'd type, class type or covergroup identifier.
    HierName(Option<Scope>, HierId, Vec<Dim>),
    /// Type reference of form type'(expr_or_data_type)
    TypeRef(Box<Expr>),
}

/// Should be boxed when nested in other AST structure.
pub type DataType = Spanned<DataTypeKind>;

/// Represent a net_port_type (but without data type)
#[derive(Debug)]
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

#[derive(Debug)]
pub struct DataDecl {
    pub attr: Option<Box<AttrInst>>,
    pub has_const: bool,
    pub lifetime: Lifetime,
    pub ty: DataType,
    pub list: Vec<DeclAssign>,
}

//
// A.2.4 Declaration assignments
//

/// Most common declaration assignment
#[derive(Debug)]
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
#[derive(Debug)]
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
// A.4.1.1 Module instantiations
//

#[derive(Debug)]
pub struct HierInst {
    pub name: Ident,
    pub dim: Vec<Dim>,
    pub ports: Vec<Arg>,
}

#[derive(Debug)]
pub struct HierInstantiation {
    pub attr: Option<Box<AttrInst>>,
    pub name: Ident,
    pub param: Option<Vec<Arg>>,
    pub inst: Vec<HierInst>,
}

#[derive(Debug)]
pub enum Arg {
    Ordered(Option<Box<AttrInst>>, Option<Box<Expr>>),
    Named(Option<Box<AttrInst>>, Box<Ident>, Option<Box<Expr>>),
    NamedWildcard(Option<Box<AttrInst>>),
}

impl AstNode for Vec<Arg> {
    fn name() -> &'static str { "arguments" }
}

//
// A.4.2 Generate instantiations
//

#[derive(Debug)]
pub struct LoopGen {
    pub attr: Option<Box<AttrInst>>,
    pub genvar: bool,
    pub id: Ident,
    pub init: Expr,
    pub cond: Expr,
    pub update: Expr,
    pub block: Item,
}

#[derive(Debug)]
pub struct IfGen {
    pub attr: Option<Box<AttrInst>>,
    pub cond: Expr,
    pub true_block: Item,
    pub false_block: Option<Box<Item>>,
}

#[derive(Debug)]
pub struct GenBlock {
    pub name: Option<Box<Ident>>,
    pub items: Vec<Item>,
}

//
// A.6.4 Statements
//

#[derive(Debug)]
pub enum StmtKind {
    Empty,
    TimingCtrl(TimingCtrl, Box<Stmt>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct EventExprItem {
    pub edge: Option<Edge>,
    pub expr: Box<Expr>,
    pub iff: Option<Box<Expr>>,
}

#[derive(Debug)]
pub enum EventExpr {
    List(Vec<EventExprItem>),
    Paren(Box<EventExpr>),
}

#[derive(Debug)]
pub enum TimingCtrl {
    DelayCtrl(Box<Expr>),
    ExprEventCtrl(Box<EventExpr>),
    NameEventCtrl(Option<Scope>, HierId),
    ImplicitEventCtrl,
    // CycleDelay
}

//
// A.8.2 Subroutine call
//

#[derive(Debug)]
pub struct SysTfCall {
    pub task: Spanned<String>,
    pub args: Option<Vec<Arg>>,
}

//
// A.8.3 Expressions
//

#[derive(Debug)]
pub enum ExprKind {
    /// As in many cases expression and type can occur in a same context, we have
    /// `ExprKind::Type` in the enum to represent the case where we know "this is definitely a
    /// type". In some cases other expression can also be viewed as type, e.g. `id[x]`
    Type(Box<DataType>),
    Literal(Token),
    
    /// A hierachical name
    HierName(Option<Scope>, HierId),

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

    Unary(Operator, Option<Box<AttrInst>>, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    PostfixIncDec(Box<Expr>, Operator),

    /// Assignment
    Assign(Box<Expr>, Operator, Box<Expr>),

    /// Parenthesised expression
    Paren(Box<Expr>),

    /// Min-typ-max expression
    MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),

    /// Conditional expression
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
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

#[derive(Debug)]
pub struct AttrSpec {
    pub name: Ident,
    pub expr: Option<Box<Expr>>
}

#[derive(Debug)]
pub struct AttrInstStruct(pub Vec<AttrSpec>);

pub type AttrInst = Spanned<AttrInstStruct>;

//
// A.9.3 Identifiers
//

#[derive(Debug)]
pub enum Scope {
    /// $unit scope
    Unit,
    /// local scope
    Local,
    /// a named scope, can possibily be nested with in a outer scope
    Name(Option<Box<Scope>>, Box<Ident>),
}

#[derive(Debug)]
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

/// Should be boxed when nested in other AST structure. An exception is that if the identifier is
/// a compulsory part for an AST, it does not have to be boxed.
pub type Ident = Spanned<String>;
