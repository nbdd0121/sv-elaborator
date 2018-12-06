use super::super::source::{Span, Spanned};
use super::super::lexer::{Token, Keyword, Operator};

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
// Unknown
//

#[derive(Debug)]
pub enum Item {
    TimeunitDecl,
    ModuleDecl(Box<ModuleDecl>),
    UdpDecl,
    InterfaceDecl,
    ProgramDecl,
    PackageDecl,
    PackageItem, // TODO Expand
    BindDirective,
    ConfigDecl,

    ContinuousAssign(Vec<Expr>),
}

//
// A.1.2 SystemVerilog source text
//

#[derive(Debug)]
pub struct ModuleDecl {
    pub lifetime: Lifetime,
    pub name: Ident,
    pub param: Option<Vec<ParamDecl>>,
    pub port: Vec<PortDecl>,
    pub items: Vec<Item>
}

//
// A.1.3 Module parameters and ports
//

/// Represent either a "type" keyword or a data type
#[derive(Debug)]
pub enum Sort {
    // We use "Kind" to denote type of type
    Kind,
    Type(Box<DataType>),
}

/// AST for parameter_declaration or localparam_declaration
#[derive(Debug)]
pub struct ParamDecl {
    // Parameter or localparam
    pub kw: Keyword,
    pub ty: Option<Sort>,
    pub list: Vec<DeclAssign>,
}

#[derive(Debug, Clone, Copy)]
pub enum PortDir {
    Input,
    Output,
    Inout,
    Ref,
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
    Implicit(Signing, Vec<Dim>),
    IntVec(Keyword, Signing, Vec<Dim>),
    IntAtom(Keyword, Signing),
    NonIntType(Keyword),
    StructUnion, // TODO
    Enum, // TODO
    String,
    Chandle,
    Interface, // TODO
    Event,
    // TODO
}

/// Should be boxed when nested in other AST structure.
pub type DataType = Spanned<DataTypeKind>;

#[derive(Debug)]
pub enum Signing {
    Signed,
    Unsigned,
}

/// Represent a built-in net-type
#[derive(Debug)]
pub enum NetType {
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

/// Represent a net_port_type (but without data type)
#[derive(Debug)]
pub enum NetPortType {
    Builtin(NetType),
    UserDefined(Box<Ident>),
    Interconnect,
    /// This is actually a variable port. We put it here for simplicity, as PortType
    /// is the only time this enum is ever used.
    Variable,
    /// It should have default net type
    Default,
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

//
// A.2.5 Declaration ranges
//

/// Possible ways of specifying a variable dimension
#[derive(Debug)]
pub enum DimKind {
    /// Represent dimension of type `[ constant_expression : constant_expression ]`
    Range(Box<Expr>, Box<Expr>),
    /// Represent dimension of type `[ constant_expression ]`.
    /// It can also represent dimension of type `[ data_type ]`, but the resolution will not occur
    /// during parsing.
    Value(Box<Expr>),
    /// Represent dimension of type `[]`
    Unsized,
    /// Represent dimension of type `[ * ]`
    AssocWild,
    /// Represent a queue dimension with optional max size, of type
    /// `[ $ [ : constant expression ] ]`
    Queue(Option<Box<Expr>>)
}

/// Should be boxed when nested in other AST structure.
pub type Dim = Spanned<DimKind>;

///
/// A.8.3 Expressions
///

#[derive(Debug)]
pub enum Select {
    Range(Box<Expr>, Box<Expr>),
    PlusRange(Box<Expr>, Box<Expr>),
    MinusRange(Box<Expr>, Box<Expr>),
    Value(Box<Expr>)
}

#[derive(Debug)]
pub enum ExprKind {
    Type(Box<DataType>),
    Literal(Token),
    
    /// A hierachical name
    HierName(Option<Scope>, HierId),

    /// Element select
    Select(Box<Expr>, Select),

    /// Member access
    Member(Box<Expr>, Ident),

    // Casts
    ConstCast(Box<Expr>),
    SignCast(Signing, Box<Expr>),
    TypeCast(Box<Expr>, Box<Expr>),

    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),

    /// Assignment
    Assign(Box<Expr>, Operator, Box<Expr>),

    /// Min-typ-max expression
    MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub type Expr = Spanned<ExprKind>;

impl AstNode for Expr {
    fn name() -> &'static str {
        "expression"
    }
}


///
/// A.9.3 Identifiers
///

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

/// Should be boxed when nested in other AST structure. An exception is that if the identifier is
/// a compulsory part for an AST, it does not have to be boxed.
pub type Ident = Spanned<String>;
