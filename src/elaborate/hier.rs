//! This module represent elaborated hierachical items.

use syntax::ast::{self, Ident};
use syntax::tokens;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
pub use super::ty::{IntTy, Ty, Struct, Enum};
pub use super::expr::Val;

/// Resolved and evaluated parameter declaration
#[derive(Debug, Clone, PartialEq)]
pub struct ParamDecl {
    //// Parameter or localparam
    pub kw: tokens::Keyword,
    pub name: Ident,
    pub ty: Ty,
    pub init: Val,
}

/// Resolved data port declaration
#[derive(Debug, Clone)]
pub struct DataPortDecl {
    pub dir: ast::PortDir,
    pub net: ast::NetPortType,
    pub name: Ident,
    pub ty: Ty,
    pub init: Option<Box<ast::Expr>>,
}

/// Resolved and evaluated typedef
#[derive(Debug, Clone)]
pub struct TypedefDecl {
    pub ty: Ty,
    pub name: Ident,
}

/// Un-instantiated module during resolution and elaboration
pub struct DesignDecl {
    /// The AST of this design declaration
    pub ast: Rc<ast::DesignDecl>,
    /// All instantiated instances. Current organised in a Vec because DesignParam can't be hashed.
    pub instances: RefCell<Vec<(Rc<DesignParam>, Rc<DesignInstantiation>)>>,
}

/// Represent parameterisation and interface parameterisation of a design unit.
/// Due to multiple way of referencing this param this should be used with Rc.
#[derive(PartialEq)]
pub struct DesignParam {
    /// The value of all parameters declared in parameter list. This also include localparams
    /// as they need to be evaluated anyway for dependency reasons and we don't want to evaluate
    /// them multiple times. Rc'ed here just for convience, could be removed.
    pub param: Rc<Vec<ParamDecl>>,
    pub intf: HashMap<String, Rc<DesignInstantiation>>,
}

/// Represent an instantiated design unit.
pub struct DesignInstantiation {
    /// Pointer to the design declaration
    pub ast: Rc<ast::DesignDecl>,
    /// Generate name of this instance,
    pub name: Ident,
    /// The parameters of this instantiation, this include interface ports already.
    pub param: Rc<DesignParam>,
    /// Hierarchical scope
    pub scope: HierScope,
}

impl PartialEq for DesignInstantiation {
    fn eq(&self, rhs: &Self) -> bool {
        // DesignInstantiation is equal only if they're the same instance
        self as *const Self == rhs as *const Self
    }
}

/// Represent a instance.
pub struct InstanceDecl {
    pub inst: Rc<DesignInstantiation>,
    pub name: Ident,
    pub dim: Vec<(i32, i32)>,
    // not evaluated yet.
    pub port: ast::PortConn,
}

/// Represent a modport declaration
pub struct Modport {
    pub name: Ident,
    pub scope: HierScope,
}

pub struct InterfacePortDecl {
    pub inst: Rc<DesignInstantiation>,
    pub modport: Option<Rc<Modport>>,
    pub name: Ident,
    pub dim: Vec<(i32, i32)>,
}

/// Represent a package.
pub struct PkgDecl {
    /// Name of this package.
    pub name: Ident,
    /// Hierarchical scope
    pub scope: HierScope,
}

/// Represent a hierachical scope in the hierachy that can be indexed using dotted names.
/// Can be either root, a design unit instantiation or a generate block.
#[derive(Clone)]
pub struct HierScope {
    /// All items, either named or un-named, under this scope.
    pub items: Vec<HierItem>,
    /// Index into items from their names
    pub names: HashMap<String, HierItem>,
}

impl HierScope {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            names: HashMap::new(),
        }
    }
}

/// Represent a generate block
pub struct GenBlock {
    pub name: Option<Box<Ident>>,
    pub scope: HierScope,
}

pub struct GenVar {
    pub name: Ident,
    pub value: RefCell<i32>,
}

/// Represent a loop-generate block
pub struct LoopGenBlock {
    pub name: Option<Box<Ident>>,
    pub instances: RefCell<Vec<(i32, Rc<GenBlock>)>>,
}

/// Represent a elaborated item. This item should be able to cheaply cloned.
/// The reason we use enum of Rc instead of Rc of enum is that by using the former approach we can
/// also use HierItem to represent "type" of hiearchical reference, e.g. InstancePart.
#[derive(Clone)]
pub enum HierItem {
    /// A parameter
    Param(Rc<ParamDecl>),
    /// A typedef or parameter type
    Type(Rc<TypedefDecl>),
    /// Represent a single port
    DataPort(Rc<DataPortDecl>),
    InterfacePort(Rc<InterfacePortDecl>),
    /// An design declaration that is not instantiated
    Design(Rc<DesignDecl>),
    /// Other items that we don't really care in elaboration, e.g. data declaration
    /// We might need to treat a little bit different to support constant functions though.
    Other(Rc<ast::Item>),
    OtherName,
    /// An instantiated design
    Instance(Rc<InstanceDecl>),
    /// An part-selected instance
    InstancePart {
        /// Pointer to the instantiated design
        inst: Rc<DesignInstantiation>,
        /// Modport reference
        modport: Option<Rc<Modport>>,
        /// Remaining dimensions
        dim: Vec<(i32, i32)>,
    },
    GenBlock(Rc<GenBlock>),
    GenVar(Rc<GenVar>),
    LoopGenBlock(Rc<LoopGenBlock>),
    Modport(Rc<Modport>),
    // /// An array of HierItems. Could be module arrays or loop-generate items.
    // Array(Vec<HierItem>),
    /// A enum name. For actual name and value, use the second index to index into
    /// first's elements field.
    Enum(Rc<Enum>, usize),
}

/// Everything after elaboration
pub struct Source {
    pub units: Vec<HierScope>,

    /// All packages
    pub pkgs: HashMap<String, PkgDecl>,

    /// All elaborated structures.
    pub structs: Vec<Rc<Struct>>,
    pub enums: Vec<Rc<Enum>>,
}
