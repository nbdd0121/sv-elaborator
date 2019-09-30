//! This pass will try to resolve identifiers and desugar package imports into fully-qualified
//! references. In this pass only first-level identifiers are resolved. For example, when
//! referencing parameter in an interface, `intf.ty`, only `intf` is resolved but not `ty`. In
//! fact, before we elaborate further and deal with parameterisation and generate blocks we are
//! unable to resolve further.

use syntax::ast;
use syntax::ast::*;
use syntax::tokens::*;
use source::*;
use syntax::ast_visit::AstVisitor;

use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use num::ToPrimitive;

pub fn resolve(diag: &DiagMgr, units: &mut Vec<Vec<Item>>) {
    let mut resolver = Resolver::new(diag);
    resolver.visit(units);
}

/// Describe what does this symbol mean.
#[derive(Debug, Clone)]
enum SymbolKind {
    /// All kind of top-level design unit
    Design {
        interface: bool,
        ports: Rc<Vec<String>>
    },
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
            return Some(v.clone())
        }
        if let Some(v) = self.weak.remove(name) {
            // If a symbol is found in weak hashmap, move it to strong one.
            self.map.insert(name.to_owned(), v.clone());
            return Some(v)
        }
        None
    }
}

struct Resolver<'a> {
    // /// Caches self-determined expression size
    // size: HashMap<NodeId, (bool, usize)>,
    diag: &'a DiagMgr,

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
            pkg: HashMap::new(),
            pkg_ref: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    fn add_to_scope_noalloc(&mut self, ident: &Ident, kind: SymbolKind) {
        match self.scopes.last_mut().unwrap().map.entry(ident.value.clone()) {
            Entry::Occupied(_) => {
                self.diag.report_error(
                    format!("name {} is already used in other definitions", ident.value),
                    ident.span
                );
            }
            Entry::Vacant(ent) => {
                ent.insert((ident.symbol, kind));
            }
        }
    }

    fn add_to_scope(&mut self, ident: &mut Ident, kind: SymbolKind) {
        ident.symbol = SymbolId::allocate();
        self.add_to_scope_noalloc(ident, kind);
    }

    fn resolve(&mut self, ident: &mut Ident) -> SymbolKind {
        let mut ret = 'block: loop {
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
            self.diag.report_error(
                format!("name {} is ambiguious: it exists in two wildcard-imported packages", ident.value),
                ident.span
            );
            ret = (SymbolId::DUMMY, SymbolKind::Error)
        }
        ident.symbol = ret.0;
        ret.1
    }

    fn resolve_unit(&mut self, ident: &mut Ident) -> SymbolKind {
        let mut ret = 'block: loop {
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
            self.diag.report_error(
                format!("name {} is ambiguious: it exists in two wildcard-imported packages", ident.value),
                ident.span
            );
            ret = (SymbolId::DUMMY, SymbolKind::Error)
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
            Some(ret) => ret.clone(),
        };
        ident.symbol = symbol.0;
        symbol.1
    }

    /// Resolve a symbol without possibly introducing a wildcard-imported symbol.
    fn resolve_strong(&mut self, ident: &mut Ident) -> Option<SymbolKind> {
        let ret = 'block: loop {
            for scope in self.scopes.iter_mut().rev() {
                if let Some(v) = scope.map.get(&ident.value) {
                    break 'block v.clone()
                }
            }
            return None
        };
        ident.symbol = ret.0;
        Some(ret.1)
    }

    /// Read a list of PortDecl and generate a list of ports
    fn port_list(ports: &Vec<PortDecl>) -> Vec<String> {
        let mut port_list = Vec::new();
        for port in ports {
            match port {
                PortDecl::Data(.., list) |
                PortDecl::Interface(.., list) => {
                    for assign in list {
                        port_list.push(assign.name.value.clone());
                    }
                }
                PortDecl::Explicit(_, name, _) => {
                    port_list.push(name.value.clone());
                }
            }
        }
        port_list
    }

    /// Build global symbols.
    /// * First we handle all packages. We do this first as packages cannot refer to other
    ///   identifiers in the compilation unit scope, so they are effectively isolated. By process
    //    them first we also get rid of file dependency problems.
    /// * Then we add all all top-level design units into the top-level scope.
    fn build_global(&mut self, items: &mut Vec<Vec<Item>>) {
        for items in items.iter_mut() {
            for item in items {
                match item {
                    Item::PkgDecl(decl) => {
                        // Introduce new scope
                        self.scopes.push(Scope::new());
                        for item in &mut decl.items {
                            self.visit_item(item)
                        }
                        // Leave the namespace
                        let scope = self.scopes.pop().unwrap();
                        let name = Rc::new(decl.name.value.to_owned());
                        for (_, (id, _)) in &scope.map {
                            self.pkg_ref.insert(*id, name.clone());
                        }
                        self.pkg.insert(name, scope.map);
                    }
                    _ => (),
                }
            }
        }

        // Introduce the global scope.
        self.scopes.push(Scope::new());
        for items in items {
            for item in items {
                match item {
                    Item::DesignDecl(decl) => {
                        let symbol = SymbolKind::Design {
                            interface: decl.kw == Keyword::Interface,
                            ports: Rc::new(Self::port_list(&decl.port)),
                        };
                        self.add_to_scope(&mut decl.name, symbol);
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

    fn desugar_pkg(&self, scope: &mut Option<ast::Scope>, id: &Ident) {
        // If this name comes from a package, desugar it.
        if let Some(pkg) = self.pkg_ref.get(&id.symbol) {
            *scope = Some(ast::Scope::Name(None, Box::new(Ident::new_unspanned(String::clone(pkg)))));
        }
    }

    fn visit_scoped_id(&mut self, scope: &mut Option<ast::Scope>, id: &mut Ident) {
        match scope {
            None => {
                self.resolve(id);
                self.desugar_pkg(scope, id);
            }
            Some(ast::Scope::Unit) => {
                self.resolve_unit(id);
                self.desugar_pkg(scope, id);
            }
            Some(ast::Scope::Name(None, pkg)) => {
                // We did extra checking here by checking if the thing exists in package. This is
                // not actually necessary though.
                self.resolve_pkg(pkg, id);
            }
            _ => unimplemented!(),
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
                    if let Some(_) = scope.weak.insert(name.to_owned(), (*id, kind.clone())) {
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
                if decl.name.symbol == SymbolId::DUMMY {
                    let symbol = SymbolKind::Design {
                        interface: decl.kw == Keyword::Interface,
                        ports: Rc::new(Self::port_list(&decl.port)),
                    };
                    self.add_to_scope(
                        &mut decl.name,
                        symbol
                    );
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
                                    (_, SymbolKind::Design {
                                        interface: true,
                                        ..
                                    }) => (),
                                    (_, SymbolKind::Error) => (),
                                    _ => {
                                        self.diag.report_error(format!("name {} is not an interface", v.value), v.span);
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
                self.scopes.pop();
                return;
            }
            Item::PkgDecl(decl) => {
                if self.scopes.len() != 2 {
                    // TODO: This should actually be checked in parser.
                    self.diag.report_error("package can only appear in compilation-unit level", decl.name.span);
                }
                return;
            }
            Item::FuncDecl(decl) => {
                self.visit_ty(&mut decl.ty);
                self.add_to_scope(&mut decl.name, SymbolKind::Var);
                self.scopes.push(Scope::new());
                for port in &mut decl.ports {
                    match port {
                        PortDecl::Data(_, _, ty, list) => {
                            self.visit_ty(ty);
                            for assign in list {
                                for dim in &mut assign.dim { self.visit_dim(dim); }
                                if let Some(v) = &mut assign.init { self.visit_expr(v); }
                                self.add_to_scope(&mut assign.name, SymbolKind::Var);
                            }
                        }
                        _ => unreachable!(),
                    }
                    self.visit_port_decl(port)
                }
                for stmt in &mut decl.stmts {
                    self.visit_stmt(stmt);
                }
                self.scopes.pop();
                return;
            }
            Item::TaskDecl(decl) => {
                self.add_to_scope(&mut decl.name, SymbolKind::Var);
                self.scopes.push(Scope::new());
                for port in &mut decl.ports {
                    match port {
                        PortDecl::Data(_, _, ty, list) => {
                            self.visit_ty(ty);
                            for assign in list {
                                for dim in &mut assign.dim { self.visit_dim(dim); }
                                if let Some(v) = &mut assign.init { self.visit_expr(v); }
                                self.add_to_scope(&mut assign.name, SymbolKind::Var);
                            }
                        }
                        _ => unreachable!(),
                    }
                    self.visit_port_decl(port)
                }
                for stmt in &mut decl.stmts {
                    self.visit_stmt(stmt);
                }
                self.scopes.pop();
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
            Item::NetDecl(decl) => {
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
            Item::TypedefIntf(_, intf, _, target) => {
                self.visit_hier_name(intf);
                self.add_to_scope(target, SymbolKind::Type);
                return;
            }
            Item::ContinuousAssign(_) |
            Item::Initial(_) |
            Item::Always(..) => (),
            Item::HierInstantiation(inst) => {
                let ports = match self.resolve(&mut inst.name) {
                    SymbolKind::Design { ports, ..} => ports,
                    SymbolKind::Error => return,
                    _ => {
                        self.diag.report_error(
                            "only design units can appear in hierachical instantiation",
                            inst.name.span
                        );
                        return;
                    }
                };
                if let Some(param) = &mut inst.param {
                    self.visit_args(param);
                }
                for single_inst in &mut inst.inst {
                    self.add_to_scope(&mut single_inst.name, SymbolKind::Instance);
                    for dim in &mut single_inst.dim {
                        self.visit_dim(dim);
                    }
                    let span = single_inst.name.span;
                    ::util::replace_with(&mut single_inst.ports, |port_conn| {
                        // As resolver, we need to de-sugar implicit and wildcard port
                        // connections. To make later code easier, we will reorder these ports
                        // so that they correspond to the order of port declaration. Note that
                        // for unconnected ports (and omitted ports in ordered variant), we still
                        // need to omit them because having explicit empty connections will
                        // suppress default connections if any.
                        let mut new_list = vec![(None, None); ports.len()];
                        match port_conn {
                            PortConn::Ordered(list) => {
                                if list.len() > ports.len() {
                                    self.diag.report_error(
                                        "instantiation contains more ports connections than declared",
                                        span
                                    );
                                }
                                for (dest, (attr, expr)) in new_list.iter_mut().zip(list) {
                                    // A omitted port in ordered list means omitted (instead of
                                    // forcing unconnection in ordered list).
                                    if let Some(mut v) = expr {
                                        self.visit_expr(&mut v);
                                        *dest = (attr, Some(Some(v)));
                                    }
                                }
                            }
                            PortConn::Named(list) => {
                                let mut has_wildcard = false;
                                for (attr, conn) in list {
                                    // Find the corresponding index in the port list.
                                    let id = match conn {
                                        NamedPortConn::Explicit(ref name, _) |
                                        NamedPortConn::Implicit(ref name) => {
                                            let id = match ports.iter().position(|port_name| port_name == &name.value) {
                                                None => {
                                                    self.diag.report_error(
                                                        format!("no port declaration named {} is declared", name),
                                                        name.span
                                                    );
                                                    continue
                                                }
                                                Some(id) => id,
                                            };
                                            if new_list[id].1.is_some() {
                                                self.diag.report_error(
                                                    "duplicate port connections",
                                                    name.span
                                                )
                                            }
                                            id
                                        }
                                        NamedPortConn::Wildcard => {
                                            has_wildcard = true;
                                            continue;
                                        }
                                    };
                                    match conn {
                                        NamedPortConn::Explicit(_, mut expr) => {
                                            // Explicit port - just visit the expression and return as is.
                                            if let Some(v) = &mut expr { self.visit_expr(v) };
                                            new_list[id] = (attr, Some(expr));
                                        }
                                        NamedPortConn::Implicit(mut name) => {
                                            // Implicit port - desugar to explicit port
                                            let mut scope = None;
                                            self.visit_scoped_id(&mut scope, &mut name);
                                            let span = name.span;
                                            new_list[id] = (attr, Some(Some(Box::new(Spanned::new(
                                                ExprKind::HierName(HierId::Name(scope, Box::new(name))), span
                                            )))));
                                        }
                                        _ => unreachable!(),
                                    }
                                }
                                if has_wildcard {
                                    new_list.iter_mut().zip(ports.iter()).for_each(|(v, name)| {
                                        if v.1.is_some() { return }
                                        let mut name = Box::new(Ident::new_unspanned(name.clone()));
                                        // Failure to resolve isn't an error for wildcard port
                                        // connection - it shall fall back to default value.
                                        match self.resolve_strong(&mut name) {
                                            None => return,
                                            _ => (),
                                        }
                                        let mut scope = None;
                                        self.desugar_pkg(&mut scope, &name);
                                        *v = (None, Some(Some(Box::new(Spanned::new_unspanned(
                                            ExprKind::HierName(HierId::Name(scope, name))
                                        )))));
                                    });
                                }
                            }
                            PortConn::Resolved(_) => unreachable!(),
                        }
                        PortConn::Resolved(new_list)
                    });
                }
                return;
            }
            Item::GenRegion(_) => (),
            Item::LoopGen(gen) => {
                if let Some(name) = &mut gen.block.name {
                    self.add_to_scope(name, SymbolKind::GenBlock);
                }
                self.visit_expr(&mut gen.init);
                self.scopes.push(Scope::new());
                if gen.genvar {
                    self.add_to_scope(&mut gen.id, SymbolKind::Var);
                }
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
            Item::Comment(_) => (),
        }
        // If code reaches here it means this is not a definition. So follow normal visiting procedure.
        self.do_visit_item(item);
    }

    fn visit_enum(&mut self, en: &mut EnumDecl) {
        if let Some(v) = &mut en.ty {
            self.visit_ty(v);
        }
        ::util::replace_with(&mut en.members, |members| {
            members.into_iter().flat_map(|mut assign| {
                if assign.dim.is_empty() {
                    // A standard enum definition
                    vec![assign]
                } else {
                    // De-sugar generated name constants
                    let (start, end) = match assign.dim.pop().unwrap().value {
                        DimKind::Value(end) => (0, {
                            let vec = if let ExprKind::Literal(Spanned {
                                value: TokenKind::IntegerLiteral(val), ..
                            }) = end.value { val.value } else { unreachable!(); };
                            vec.get_two_state().unwrap().to_usize().unwrap() - 1
                        }),
                        DimKind::Range(start, end) => ({
                            let vec = if let ExprKind::Literal(Spanned {
                                value: TokenKind::IntegerLiteral(val), ..
                            }) = start.value { val.value } else { unreachable!(); };
                            vec.get_two_state().unwrap().to_usize().unwrap()
                        }, {
                            let vec = if let ExprKind::Literal(Spanned {
                                value: TokenKind::IntegerLiteral(val), ..
                            }) = end.value { val.value } else { unreachable!(); };
                            vec.get_two_state().unwrap().to_usize().unwrap()
                        }),
                        _ => unreachable!(),
                    };
                    let ident = assign.name;
                    let mut list = Vec::new();
                    // Handle first element (we need to use the init)
                    assign.name = Ident::new(format!("{}{}", ident.value, start), ident.span);
                    list.push(assign);
                    for val in (start + 1) ..= end {
                        list.push(DeclAssign {
                            name: Ident::new(format!("{}{}", ident.value, val), ident.span),
                            dim: Vec::new(),
                            init: None,
                        });
                    }
                    list
                }
            }).map(|mut assign| {
                self.add_to_scope(&mut assign.name, SymbolKind::Param);
                if let Some(v) = &mut assign.init {
                    self.visit_expr(v);
                }
                assign
            }).collect()
        });
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

    fn visit_hier_name(&mut self, id: &mut HierId) {
        // We only resolve the top-most one
        match id {
            HierId::Name(scope, id) => self.visit_scoped_id(scope, id),
            HierId::Member(sup, _) => self.visit_hier_name(sup),
            HierId::Select(sup, dim) => {
                self.visit_hier_name(sup);
                self.visit_dim(dim);
            },
            _ => (),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt.value {
            StmtKind::For {
                // We only care for loops with variable declaration. If ty is None, then all
                // loop variables are pre-declared and thus we don't care.
                ty: Some(ref mut ty),
                ref mut init, ref mut cond, ref mut update, ref mut body
            } => {
                self.visit_ty(ty);
                self.scopes.push(Scope::new());
                for expr in init {
                    if let ExprKind::Assign(lhs, rhs) = &mut expr.value {
                        if let ExprKind::HierName(HierId::Name(None, name)) = &mut lhs.value {
                            self.add_to_scope(name, SymbolKind::Var);
                        } else { unreachable!() }
                        self.visit_expr(rhs);
                    } else { unreachable!(); }
                }
                if let Some(expr) = cond { self.visit_expr(expr); }
                for expr in update { self.visit_expr(expr); }
                self.visit_stmt(body);
                self.scopes.pop();
            }
            StmtKind::SeqBlock(ref mut items) => {
                self.scopes.push(Scope::new());
                for item in items { self.visit_stmt(item); }
                self.scopes.pop();
            }
            StmtKind::DataDecl(ref mut decl) => {
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
            }
            _ => self.do_visit_stmt(stmt),
        }
    }
}
