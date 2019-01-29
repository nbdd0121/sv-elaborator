//! This module represent elaborated hierachical items.

use syntax::ast::{self, Ident, SymbolId};
use syntax::tokens;

use std::rc::{Rc, Weak};
use std::cell::RefCell;
use std::collections::HashMap;
pub use super::ty::{IntTy, Ty, Struct, Enum};
pub use super::expr::{Expr, Stmt, Val};

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

/// Resolved variable declaration
#[derive(Debug)]
pub struct DataDecl {
    pub lifetime: ast::Lifetime,
    pub ty: Ty,
    pub name: Ident,
    pub init: Option<Box<Expr>>,
}

/// Resolved net declaration
#[derive(Debug)]
pub struct NetDecl {
    pub net: ast::NetTy,
    pub ty: Ty,
    pub name: Ident,
    pub init: Option<Box<Expr>>,
}

/// Partially resolved function declaration
#[derive(Debug)]
pub struct FuncDecl {
    pub lifetime: ast::Lifetime,
    pub ty: Ty,
    pub name: Ident,
    pub ports: Vec<ast::PortDecl>,
    pub stmts: Vec<ast::Stmt>,
}

/// Partially resolved function declaration
#[derive(Debug)]
pub struct TaskDecl {
    pub lifetime: ast::Lifetime,
    pub name: Ident,
    pub ports: Vec<ast::PortDecl>,
    pub stmts: Vec<ast::Stmt>,
}

/// Un-instantiated module during resolution and elaboration
#[derive(Clone)]
pub struct DesignDecl {
    /// The AST of this design declaration
    pub ast: Rc<ast::DesignDecl>,
    /// All instantiated instances. Current organised in a Vec because DesignParam can't be hashed.
    pub instances: RefCell<Vec<(Rc<DesignParam>, Rc<DesignInstantiation>)>>,
}

impl PartialEq for DesignDecl {
    fn eq(&self, rhs: &Self) -> bool {
        // DesignDecl is equal only if they're the same instance
        self as *const Self == rhs as *const Self
    }
}

/// Represent parameterisation and interface parameterisation of a design unit.
/// Due to multiple way of referencing this param this should be used with Rc.
#[derive(PartialEq)]
pub struct DesignParam {
    /// The value of all parameters declared in parameter list. This also include localparams
    /// as they need to be evaluated anyway for dependency reasons and we don't want to evaluate
    /// them multiple times. Rc'ed here just for convience, could be removed.
    pub param: Rc<Vec<ParamDecl>>,
    pub intf: HashMap<String, DesignInstHandle>,
}

/// Represent a handle to an design instantiation
#[derive(PartialEq, Clone)]
pub struct DesignInstHandle(pub Rc<DesignDecl>, pub Rc<DesignParam>);

impl DesignInstHandle {
    pub fn get_instance(&self) -> Rc<DesignInstantiation> {
        let vec = self.0.instances.borrow();
        Rc::clone(&vec.iter().find(|(param, _)| param == &self.1).unwrap().1)
    }
}

/// Represent an instantiated design unit.
#[derive(Clone)]
pub struct DesignInstantiation {
    pub decl: Weak<DesignDecl>,
    /// Generate name of this instance,
    pub name: Ident,
    /// The parameters of this instantiation, this include interface ports already.
    pub param: Rc<DesignParam>,
    /// Hierarchical scope
    pub scope: HierScope,
}

/// Represent a instance.
pub struct InstanceDecl {
    pub inst: DesignInstHandle,
    pub name: Ident,
    pub dim: Vec<(i32, i32)>,
    pub port: Vec<Option<Expr>>,
}

/// Represent a modport declaration
pub struct Modport {
    pub name: Ident,
    pub scope: HierScope,
}

pub struct InterfacePortDecl {
    pub inst: DesignInstHandle,
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
    pub names: HashMap<String, usize>,
    /// Lexical map from symbol to item
    pub symbols: HashMap<SymbolId, usize>,
}

impl HierScope {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            names: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    /// Insert an item into the scope.
    pub fn insert(&mut self, ident: Option<Ident>, item: HierItem) {
        let index = self.items.len();
        self.items.push(item);
        if let Some(ident) = ident {
            self.names.insert(ident.value, index);
            self.symbols.insert(ident.symbol, index);
        }
    }

    pub fn find<'a>(&'a self, name: &str) -> Option<&'a HierItem> {
        match self.names.get(name) {
            None => None,
            Some(index) => Some(&self.items[*index])
        }
    }
}

/// Represent a generate block
#[derive(Clone)]
pub struct GenBlock {
    pub name: Option<Ident>,
    pub scope: HierScope,
    pub id: Option<usize>,
}

pub struct GenVar {
    pub name: Ident,
    pub value: RefCell<i32>,
}

/// Represent a loop-generate block
#[derive(Clone)]
pub struct LoopGenBlock {
    pub name: Option<Ident>,
    pub instances: RefCell<Vec<(i32, Rc<GenBlock>)>>,
    pub id: usize,
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
    /// Data declaration
    DataDecl(Rc<DataDecl>),
    NetDecl(Rc<NetDecl>),
    FuncDecl(Rc<FuncDecl>),
    TaskDecl(Rc<TaskDecl>),
    ContinuousAssign(Rc<Expr>),
    Always(ast::AlwaysKw, Rc<Stmt>),
    /// Other items that we don't really care in elaboration
    /// We might need to treat a little bit different to support constant functions though.
    Other(Rc<ast::Item>),
    /// An instantiated design
    Instance(Rc<InstanceDecl>),
    /// An part-selected instance
    InstancePart {
        /// Pointer to the instantiated design
        inst: DesignInstHandle,
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
