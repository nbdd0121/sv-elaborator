use super::super::source::{Span, Spanned};
use super::super::lexer::{Token, Keyword};

pub enum Item {
    TimeunitDecl,
    ModuleDecl,
    UdpDecl,
    InterfaceDecl,
    ProgramDecl,
    PackageDecl,
    PackageItem, // TODO Expand
    BindDirective,
    ConfigDecl,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Token)
}

pub type Expr = Spanned<ExprKind>;

#[derive(Debug)]
pub enum ExprOrType {
    Expr(Box<Expr>),
    Type(Box<DataType>),
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
    pub list: Vec<ParamAssign>,
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

#[derive(Debug)]
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

/// AST for param_assignment
#[derive(Debug)]
pub struct ParamAssign {
    pub name: Ident,
    pub dim: Vec<Dim>,
    pub init: Option<ExprOrType>,
}

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
    /// Represent dimension of type `[ constant_expression ]`
    Value(Box<Expr>),
    /// Represent dimension of type `[]`
    Unsized,
    /// Represent dimension of type `[ data_type ]`
    Assoc(Box<DataType>),
    /// Represent dimension of type `[ * ]`
    AssocWild,
    /// Represent a queue dimension with optional max size, of type
    /// `[ $ [ : constant expression ] ]`
    Queue(Option<Box<Expr>>)
}

/// Should be boxed when nested in other AST structure.
pub type Dim = Spanned<DimKind>;

///
/// A.9.3 Identifiers
///

/// Should be boxed when nested in other AST structure. An exception is that if the identifier is
/// a compulsory part for an AST, it does not have to be boxed.
pub type Ident = Spanned<String>;
