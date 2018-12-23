//! This pass will try to resolve identifiers and desugar package imports into fully-qualified
//! references. In this pass only first-level identifiers are resolved. For example, when
//! referencing parameter in an interface, `intf.ty`, only `intf` is resolved but not `ty`. In
//! fact, before we elaborate further and deal with parameterisation and generate blocks we are
//! unable to resolve further.

use syntax::ast;
use syntax::ast::*;
use syntax::tokens::*;
use source::*;

use super::ast_visit::AstVisitor;

use std::rc::Rc;
use std::collections::HashMap;

pub fn resolve(diag: &DiagMgr, units: &mut Vec<Vec<Item>>) {
    let mut resolver = Resolver::new(diag);
    resolver.visit(units);
}

/// Describe what does this symbol mean.
#[derive(Debug, Clone, Copy)]
enum SymbolKind {
    /// All kind of top-level design unit except interface and program
    Design,
    /// An interface type. Treated differently due to interface ports.
    Interface,
    /// A hierachical instance.
    Instance,
    /// Symbol introduced by typedef or type parameter
    Type,
    /// Value paramter
    Param,
    /// This is name of an interface port
    IntfPort,
    /// Net or variable
    Var,
    /// Name of generate block
    GenBlock,
    /// Means that the symbol is conflicted, e.g. import the same name (wildcard) from two
    /// packages.
    Conflict,
    /// Error recovery symbol
    Error,
}

/// What's being declared in the current scope so far
struct Scope {
    /// Either a symbol defined in this scope or a symbol imported wildcard imported and used.
    map: HashMap<String, (SymbolId, SymbolKind)>,
    /// All symbols that are wildcard imported from packages but are not yet used.
    weak: HashMap<String, (SymbolId, SymbolKind)>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            map: HashMap::new(),
            weak: HashMap::new(),
        }
    }

    fn resolve(&mut self, name: &String) -> Option<(SymbolId, SymbolKind)> {
        if let Some(v) = self.map.get(name) {
            return Some(*v)
        }
        if let Some(v) = self.weak.get(name).map(|v| *v) {
            // If a symbol is found in weak hashmap, move it to strong one.
            self.map.insert(name.to_owned(), v);
            self.weak.remove(name);
            return Some(v)
        }
        None
    }
}

struct Resolver<'a> {
    // /// Caches self-determined expression size
    // size: HashMap<NodeId, (bool, usize)>,
    diag: &'a DiagMgr,
    symbol: u32,

    /// Packages. As package name needs to be inserted into both pkg and pkg_ref we avoid
    /// duplication by using reference-counted pointers here.
    pkg: HashMap<Rc<String>, HashMap<String, (SymbolId, SymbolKind)>>,
    /// Reference from SymbolId to corresponding package name.
    pkg_ref: HashMap<SymbolId, Rc<String>>,

    scopes: Vec<Scope>,
}

impl<'a> Resolver<'a> {

    fn new(diag: &'a DiagMgr) -> Resolver<'a> {
        Resolver {
            diag: diag,
            symbol: 0,
            pkg: HashMap::new(),
            pkg_ref: HashMap::new(),
            // Pre-fill scope with a globla scope.
            scopes: vec![Scope::new()],
        }
    }

    fn alloc_id(&mut self) -> SymbolId {
        let ret = self.symbol;
        self.symbol += 1;
        SymbolId(ret)
    }

    fn add_to_scope_noalloc(&mut self, ident: &Ident, kind: SymbolKind) {
        if let Some(_) = self.scopes.last_mut().unwrap().map.insert(ident.value.clone(), (
            ident.symbol,
            kind
        )) {
            self.diag.report_fatal(
                format!("name {} is already used in other definitions", ident.value),
                ident.span
            );
        }
    }

    fn add_to_scope(&mut self, ident: &mut Ident, kind: SymbolKind) {
        ident.symbol = self.alloc_id();
        if let Some(_) = self.scopes.last_mut().unwrap().map.insert(ident.value.clone(), (
            ident.symbol,
            kind
        )) {
            self.diag.report_fatal(
                format!("name {} is already used in other definitions", ident.value),
                ident.span
            );
        }
    }

    fn resolve(&mut self, ident: &mut Ident) -> SymbolKind {
        let ret = 'block: loop {
            for scope in self.scopes.iter_mut().rev() {
                if let Some((id, kind)) = scope.resolve(&ident.value) {
                    break 'block (id, kind)
                }
            }
            self.diag.report_error(
                format!("name {} does not exist in the scope", ident.value),
                ident.span
            );
            break (SymbolId::DUMMY, SymbolKind::Error)
        };
        if let SymbolKind::Conflict = ret.1 {
            self.diag.report_fatal(
                format!("name {} is ambiguious: it exists in two wildcard-imported packages", ident.value),
                ident.span
            );
        }
        ident.symbol = ret.0;
        ret.1
    }

    fn resolve_unit(&mut self, ident: &mut Ident) -> SymbolKind {
        let ret = 'block: loop {
            if let Some((id, kind)) = self.scopes.first_mut().unwrap().resolve(&ident.value) {
                break 'block (id, kind)
            }
            self.diag.report_error(
                format!("name {} does not exist in the scope", ident.value),
                ident.span
            );
            break (SymbolId::DUMMY, SymbolKind::Error)
        };
        if let SymbolKind::Conflict = ret.1 {
            self.diag.report_fatal(
                format!("name {} is ambiguious: it exists in two wildcard-imported packages", ident.value),
                ident.span
            );
        }
        ident.symbol = ret.0;
        ret.1
    }

    fn resolve_pkg(&mut self, pkg: &Ident, ident: &mut Ident) -> SymbolKind {
        let pkg_items = match self.pkg.get(&pkg.value) {
            None => {
                self.diag.report_error("cannot find this package", pkg.span);
                ident.symbol = SymbolId::DUMMY;
                return SymbolKind::Error;
            }
            Some(v) => v,
        };
        // This is an explicit import, retrieve from the pkg_items.
        let symbol = match pkg_items.get(&ident.value) {
            None => {
                self.diag.report_error("cannot find the name in package", ident.span);
                (SymbolId::DUMMY, SymbolKind::Error)
            }
            Some(ret) => *ret,
        };
        ident.symbol = symbol.0;
        symbol.1
    }

    /// Build global symbols. First all all top-level design units into the top-level scope.
    fn build_global(&mut self, items: &mut Vec<Vec<Item>>) {
        for items in items {
            for item in items {
                match item {
                    Item::DesignDecl(decl) => {
                        match decl.kw {
                            Keyword::Module |
                            Keyword::Primitive |
                            Keyword::Program => {
                                self.add_to_scope(&mut decl.name, SymbolKind::Design);
                            }
                            Keyword::Interface => {
                                self.add_to_scope(&mut decl.name, SymbolKind::Interface);
                            }
                            // Package are very special and are not dealt here.
                            Keyword::Package => (),
                            _ => unreachable!(),
                        }
                    }
                    _ => (),
                }
            }
        }
    }

    fn visit_unit(&mut self, items: &mut Vec<Item>) {
        for item in items {
            self.visit_item(item);
        }
    }

    fn visit(&mut self, items: &mut Vec<Vec<Item>>) {
        self.build_global(items);
        for items in items {
            self.scopes.push(Scope::new());
            self.visit_unit(items);
            self.scopes.pop();
        }
    }

    fn visit_scoped_id(&mut self, scope: &mut Option<ast::Scope>, id: &mut Ident) {
        match scope {
            None => {
                self.resolve(id);
                // If this name comes from a package, desugar it.
                if let Some(pkg) = self.pkg_ref.get(&id.symbol) {
                    *scope = Some(ast::Scope::Name(None, Box::new(Ident::new_unspanned(String::clone(pkg)))));
                }
            }
            Some(ast::Scope::Unit) => {
                self.resolve_unit(id);
                // If this name comes from a package, desugar it.
                if let Some(pkg) = self.pkg_ref.get(&id.symbol) {
                    *scope = Some(ast::Scope::Name(None, Box::new(Ident::new_unspanned(String::clone(pkg)))));
                }
            }
            Some(ast::Scope::Name(None, pkg)) => {
                // We did extra checking here by checking if the thing exists in package. This is
                // not actually necessary though.
                self.resolve_pkg(pkg, id);
            }
            _ => unimplemented!(),
        }
    }

    fn visit_hier_name(&mut self, scope: &mut Option<ast::Scope>, id: &mut HierId) {
        // We only resolve the top-most one
        match id {
            HierId::Name(Some(sup), _) => self.visit_hier_name(scope, sup),
            HierId::Name(None, id) => self.visit_scoped_id(scope, id),
            _ => (),
        }
    }

    fn visit_import(&mut self, import: &mut Vec<PkgImportItem>) {
        for import in import {
            if let Some(v) = &mut import.1 {
                // Resolve reference and add to scope with same name.
                let kind = self.resolve_pkg(&import.0, v);
                self.add_to_scope_noalloc(v, kind);
            } else {
                // First get the package from global package list.
                let pkg_items = match self.pkg.get(&import.0.value) {
                    None => {
                        self.diag.report_error("cannot find this package", import.0.span);
                        continue;
                    }
                    Some(v) => v,
                };

                // This is a wildcard import. Add them to weak list.
                let scope = self.scopes.last_mut().unwrap();
                for (name, (id, kind)) in pkg_items {
                    if let Some(_) = scope.weak.insert(name.to_owned(), (*id, *kind)) {
                        scope.weak.insert(name.to_owned(), (SymbolId::DUMMY, SymbolKind::Conflict));
                    }
                }
            }
        }
    }
}

impl<'a> AstVisitor for Resolver<'a> {
    fn visit_param_decl(&mut self, decl: &mut ParamDecl) {
        if let Some(ty) = &mut decl.ty {
            self.visit_ty(ty);
        }
        for assign in &mut decl.list {
            for dim in &mut assign.dim { self.visit_dim(dim); }
            if let Some(v) = &mut assign.init { self.visit_expr(v); }
            self.add_to_scope(&mut assign.name, SymbolKind::Param);
        }
    }

    fn visit_item(&mut self, item: &mut Item) {
        match item {
            Item::DesignDecl(decl) => {
                // If name is not yet resolved (i.e. not top-level), add it to the scope.
                if let Keyword::Package = decl.kw {
                    if self.scopes.len() != 2 {
                        // TODO: This should actually be checked in parser.
                        self.diag.report_fatal("package can only appear in compilation-unit level", decl.name.span);
                    }
                } else {
                    if decl.name.symbol == SymbolId::DUMMY {
                        self.add_to_scope(
                            &mut decl.name,
                            if let Keyword::Interface = decl.kw { SymbolKind::Interface } else { SymbolKind::Design }
                        );
                    }
                }

                // Introduce new scope
                self.scopes.push(Scope::new());

                for import in &mut decl.pkg_import {
                    self.visit_import(import);
                }

                if let Some(param) = &mut decl.param {
                    for decl in param {
                        self.visit_param_decl(decl)
                    }
                }

                for port in &mut decl.port {
                    match port {
                        PortDecl::Data(_, _, ty, list) => {
                            self.visit_ty(ty);
                            for assign in list {
                                for dim in &mut assign.dim { self.visit_dim(dim); }
                                if let Some(v) = &mut assign.init { self.visit_expr(v); }
                                self.add_to_scope(&mut assign.name, SymbolKind::Var);
                            }
                        }
                        PortDecl::Interface(intf, modport, list) => {
                            if let Some(v) = intf {
                                match (modport.is_some(), self.resolve(v)) {
                                    (false, SymbolKind::Type) => panic!("aww!! this should be a data port instead!"),
                                    (_, SymbolKind::Interface) => (),
                                    _ => {
                                        self.diag.report_fatal(format!("name {} is not an interface", v.value), v.span);
                                    }
                                }
                            }
                            for assign in list {
                                for dim in &mut assign.dim { self.visit_dim(dim); }
                                if let Some(v) = &mut assign.init { self.visit_expr(v); }
                                self.add_to_scope(&mut assign.name, SymbolKind::IntfPort);
                            }
                        }
                        PortDecl::Explicit(_, _name, expr) => {
                            // The name here is an external name but not an internal one.
                            self.visit_expr(expr);
                        }
                    }
                    self.visit_port_decl(port)
                }

                for item in &mut decl.items {
                    self.visit_item(item)
                }

                // Leave the namespace
                let scope = self.scopes.pop().unwrap();
                if let Keyword::Package = decl.kw {
                    let name = Rc::new(decl.name.value.to_owned());
                    for (_, (id, _)) in &scope.map {
                        self.pkg_ref.insert(*id, name.clone());
                    }
                    self.pkg.insert(name, scope.map);
                }
                return;
            }
            Item::PkgImport(import) => {
                self.visit_import(import);
                return;
            }
            Item::ParamDecl(_) => (),
            Item::DataDecl(decl) => {
                self.visit_ty(&mut decl.ty);
                for assign in &mut decl.list {
                    for dim in &mut assign.dim {
                        self.visit_dim(dim);
                    }
                    if let Some(v) = &mut assign.init {
                        self.visit_expr(v);
                    }
                    self.add_to_scope(&mut assign.name, SymbolKind::Var);
                }
                return;
            }
            Item::Typedef(_, ty, name, dim) => {
                self.visit_ty(ty);
                for dim in dim {
                    self.visit_dim(dim);
                }
                self.add_to_scope(name, SymbolKind::Type);
                return;
            }
            Item::TypedefIntf(_, expr, _, target) => {
                self.visit_expr(expr);
                self.add_to_scope(target, SymbolKind::Type);
                return;
            }
            Item::ContinuousAssign(_) |
            Item::Initial(_) |
            Item::Always(..) => (),
            Item::HierInstantiation(inst) => {
                let kind = self.resolve(&mut inst.name);
                match kind {
                    SymbolKind::Design |
                    SymbolKind::Interface |
                    SymbolKind::Error => (),
                    _ => {
                        self.diag.report_fatal("only design units can appear in hierachical instantiation", inst.name.span);
                    }
                }
                if let Some(param) = &mut inst.param {
                    self.visit_args(param);
                }
                for single_inst in &mut inst.inst {
                    self.add_to_scope(&mut single_inst.name, SymbolKind::Instance);
                    for dim in &mut single_inst.dim {
                        self.visit_dim(dim);
                    }
                    self.visit_args(&mut single_inst.ports);
                }
                return;
            }
            Item::GenRegion(_) => (),
            Item::LoopGen(gen) => {
                if let Some(name) = &mut gen.block.name {
                    self.add_to_scope(name, SymbolKind::GenBlock);
                }
                self.scopes.push(Scope::new());
                if gen.genvar {
                    self.add_to_scope(&mut gen.id, SymbolKind::Var);
                }
                self.visit_expr(&mut gen.init);
                self.visit_expr(&mut gen.cond);
                self.visit_expr(&mut gen.update);
                for item in &mut gen.block.items {
                    self.visit_item(item);
                }
                self.scopes.pop();
                return;
            }
            Item::IfGen(gen) => {
                // For IfGen construct, it is permissible to have else blocks to share name
                // with if block.
                let mut names_added: HashMap<String, SymbolId> = HashMap::new();

                for (cond, block) in &mut gen.if_block {
                    self.visit_expr(cond);
                    if let Some(name) = &mut block.name {
                        // Use symbol in names_added if there is, otherwise insert.
                        match names_added.get(&name.value).map(|v| *v) {
                            None => {
                                self.add_to_scope(name, SymbolKind::GenBlock);
                                names_added.insert(name.value.clone(), name.symbol);
                            }
                            Some(v) => name.symbol = v,
                        }
                    }
                    self.scopes.push(Scope::new());
                    for item in &mut block.items {
                        self.visit_item(item);
                    }
                    self.scopes.pop();
                }

                if let Some(false_item) = &mut gen.else_block {
                    if let Some(name) = &mut false_item.name {
                        // Use symbol in names_added if there is, otherwise insert.
                        match names_added.get(&name.value).map(|v| *v) {
                            None => {
                                self.add_to_scope(name, SymbolKind::GenBlock);
                                names_added.insert(name.value.clone(), name.symbol);
                            }
                            Some(v) => name.symbol = v,
                        }
                    }
                    self.scopes.push(Scope::new());
                    for item in &mut false_item.items {
                        self.visit_item(item);
                    }
                    self.scopes.pop();
                }
                return;
            }
            Item::SysTfCall(_) => (),
            // We don't really need to resolve modport yet, so just make its kind "Var" for the moment.
            Item::ModportDecl(_, decl) => {
                for decl in decl {
                    for item in &mut decl.1 {
                        match item {
                            ModportPortDecl::Simple(_, _, list) => {
                                for item in list {
                                    match item {
                                        ModportSimplePort::Named(name) => {
                                            self.resolve(name);
                                        }
                                        ModportSimplePort::Explicit(_name, expr) => {
                                            self.visit_expr(expr);
                                        }
                                    }
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    self.add_to_scope(&mut decl.0, SymbolKind::Var);
                }
                return;
            }
        }
        // If code reaches here it means this is not a definition. So follow normal visiting procedure.
        self.do_visit_item(item);
    }

    fn visit_enum(&mut self, en: &mut EnumDecl) {
        if let Some(v) = &mut en.ty {
            self.visit_ty(v);
        }
        for assign in &mut en.members {
            self.add_to_scope(&mut assign.name, SymbolKind::Param);
            if let Some(v) = &mut assign.init {
                self.visit_expr(v);
            }
        }
    }

    fn visit_ty(&mut self, ty: &mut DataType) {
        if let DataTypeKind::HierName(scope, id, dim) = &mut ty.value {
            self.visit_scoped_id(scope, id);
            for dim in dim {
                self.visit_dim(dim);
            }
            return;
        }
        self.do_visit_ty(ty);
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        if let ExprKind::HierName(scope, id) = &mut expr.value {
            self.visit_hier_name(scope, id);
            return;
        }
        self.do_visit_expr(expr);
    }
}
