//! Convert instance arrays and interface array ports into multiple non-array ones.

use std::rc::Rc;
use std::cmp;
use std::collections::HashMap;
use num::ToPrimitive;

use syntax::tokens;
use syntax::ast::{self, Ident};
use elaborate::ty;
use elaborate::expr;
use elaborate::hier::{self, HierScope, HierItem, InstanceDecl, InterfacePortDecl};

pub fn inst_array_elim(source: hier::Source) -> hier::Source {
    let mut elim = InstArrayEliminator {
        scopes: Vec::new(),
        units: Vec::new(),
        pkgs: source.pkgs,
        structs: source.structs,
        enums: source.enums,
        map: HashMap::new(),
    };
    elim.visit(source.units);
    hier::Source {
        units: elim.units,
        pkgs: elim.pkgs,
        structs: elim.structs,
        enums: elim.enums,
    }
}

struct InstArrayEliminator {
    scopes: Vec<HierScope>,
    units: Vec<HierScope>,
    pkgs: HashMap<String, hier::PkgDecl>,
    structs: Vec<Rc<ty::Struct>>,
    enums: Vec<Rc<ty::Enum>>,

    /// Mapping from the instance arrays to individual unrolled instances
    map: HashMap<usize, (i32, i32, HashMap<i32, HierItem>)>,
}

fn dim_to_i32(dim: &ast::Dim) -> i32 {
    if let ast::DimKind::Value(ref val) = dim.value {
        if let ast::ExprKind::Literal(ref tok) = val.value {
            if let tokens::TokenKind::IntegerLiteral(ref lit) = tok.value {
                return lit.value.get_two_state().unwrap().to_i32().unwrap();
            }
        }
    }
    unreachable!();
}

impl InstArrayEliminator {
    pub fn visit(&mut self, mut units: Vec<HierScope>) {
        // We need to transform in multiple steps, because when we transform expressions we rely
        // on both the old declaration information and the new declaration information.
        // We therefore perform this operation in two steps:
        // * First, we gather all instance arrays and create HierItem of each individual instances
        //   for them.
        // * In second step, we visit all expressions and replace HierItem::Select with reference
        //   to new HierItem
        // * In the third step, we eliminate instance arrays and add the newly created HierItem
        //   into the scope.
        for unit in &mut units {
            self.gather_array(unit);
        }
        let units: Vec<_> = units.into_iter().map(|scope| self.xfrm_scope(scope)).collect();
        self.units = units.into_iter().map(|scope| self.expand_array(scope)).collect();
    }

    /// First stage of instance array elimination: gather arrays.
    pub fn gather_array(&mut self, scope: &mut HierScope) {
        for item in &mut scope.items {
            match item {
                HierItem::Instance(decl) => {
                    // This is not an array
                    if decl.dim.is_empty() {
                        continue;
                    }
                    // TODO: Deal with more dimensions
                    assert!(decl.dim.len() == 1);
                    let (lb, ub) = decl.dim[0];
                    let (lb, ub) = (cmp::min(lb, ub), cmp::max(lb, ub));
                    let mut map = HashMap::new();
                    for i in lb..=ub {
                        let name = format!("{}_{}", decl.name.value, i);
                        // TODO: Consider allocate a SymbolId
                        let ident = Ident::new(name, decl.name.span);
                        let item = HierItem::Instance(Rc::new(InstanceDecl {
                            inst: decl.inst.clone(),
                            name: ident,
                            dim: Vec::new(),
                            port: decl.port.clone(),
                        }));
                        map.insert(i, item);
                    }
                    // Use the address of declaration as index key
                    let ptr = &**decl as *const _ as usize;
                    self.map.insert(ptr, (lb, ub, map));
                    continue;
                }
                HierItem::InterfacePort(decl) => {
                    // This is not an array, return as is.
                    if decl.dim.is_empty() {
                        continue;
                    }
                    // TODO: Deal with more dimensions
                    assert!(decl.dim.len() == 1);
                    let (lb, ub) = decl.dim[0];
                    let (lb, ub) = (cmp::min(lb, ub), cmp::max(lb, ub));
                    let mut map = HashMap::new();
                    for i in lb..=ub {
                        let name = format!("{}_{}", decl.name.value, i);
                        let ident = Ident::new(name, decl.name.span);
                        let item = HierItem::InterfacePort(Rc::new(InterfacePortDecl {
                            inst: decl.inst.clone(),
                            modport: decl.modport.clone(),
                            name: ident,
                            dim: Vec::new(),
                        }));
                        map.insert(i, item);
                    }
                    let ptr = &**decl as *const _ as usize;
                    self.map.insert(ptr, (lb, ub, map));
                    continue;
                }
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        self.gather_array(&mut Rc::get_mut(inst).unwrap().scope);
                    }
                }
                HierItem::GenBlock(genblk) => {
                    self.gather_array(&mut Rc::get_mut(genblk).unwrap().scope);
                }
                _ => (),
            }
        }
    }

    /// Transform an hierachical identifier and return its resolved result.
    /// Currently if resolution fails, HierItem::OtherName is returned.
    pub fn xfrm_hier_id(&mut self, id: &mut ast::HierId) -> HierItem {
        match id {
            ast::HierId::Name(scope, name) => {
                match scope {
                    Some(ast::Scope::Name(None, pkg)) => {
                        // Packaged name, retrieve from package
                        self.pkgs[&pkg.value].scope.find(&name).unwrap().clone()
                    }
                    None => {
                        'resolve_loop: loop {
                            for scope in self.scopes.iter().rev() {
                                if let Some(v) = scope.find(&name) {
                                    break 'resolve_loop v.clone()
                                }
                            }
                            unreachable!()
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ast::HierId::Member(parent, name) => {
                let parent_hier = self.xfrm_hier_id(parent);
                match parent_hier {
                    HierItem::InterfacePort(decl) => {
                        decl.inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::Instance(decl) => {
                        decl.inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::InstancePart { inst, .. } => {
                        inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::GenBlock(decl) => {
                        decl.scope.find(&name).unwrap().clone()
                    }
                    _ => unreachable!()
                }
            }
            ast::HierId::Select(..) => {
                let mut hier = None;
                ::util::replace_with(id, |id| {
                    // Move out parent and the bit-select out from original ast::HierId
                    let (mut parent, parent_hier, sel) = if let ast::HierId::Select(mut parent, sel) = id {
                        let hier = self.xfrm_hier_id(&mut parent);
                        let sel = dim_to_i32(&sel);
                        (*parent, hier, sel)
                    } else { unreachable!() };
                    hier = Some(match parent_hier {
                        HierItem::Instance(ref inst) => {
                            HierItem::InstancePart {
                                inst: inst.inst.clone(),
                                modport: None,
                                dim: inst.dim.iter().skip(1).map(Clone::clone).collect(),
                            }
                        }
                        HierItem::InterfacePort(ref decl) => {
                            HierItem::InstancePart {
                                inst: decl.inst.clone(),
                                modport: decl.modport.clone(),
                                dim: decl.dim.iter().skip(1).map(Clone::clone).collect(),
                            }
                        }
                        _ => unreachable!(),
                    });
                    // Mutate the parent's name to include the dimension
                    match parent.value {
                        ast::HierId::Name(_, ref mut name) |
                        ast::HierId::Member(_, ref mut name) => {
                            name.value = format!("{}_{}", name.value, sel);
                        }
                        _ => unreachable!(),
                    };
                    // Replace the HierId::Select with the mutated parent
                    parent.value
                });
                hier.unwrap()
            }
            _ => unimplemented!(),
        }
    }

    /// Transform an expression. This visits all subexpressions.
    pub fn xfrm_expr(&mut self, expr: &mut expr::Expr) {
        match &mut expr.value {
            expr::ExprKind::Const(..) => (),
            expr::ExprKind::HierName(id) => {
                self.xfrm_hier_id(id);
            }
            expr::ExprKind::EmptyQueue => (),
            expr::ExprKind::Concat(list) => for item in list { self.xfrm_expr(item) },
            expr::ExprKind::MultConcat(_, subexpr) => self.xfrm_expr(subexpr),
            expr::ExprKind::AssignPattern(_, pattern) => {
                match pattern {
                    expr::AssignPattern::Simple(list) => {
                        for item in list {
                            self.xfrm_expr(item);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            expr::ExprKind::Select(parent, dim) => {
                self.xfrm_expr(parent);
                match &mut dim.value {
                    expr::DimKind::Value(expr) => self.xfrm_expr(expr),
                    expr::DimKind::Range(..) => (),
                    expr::DimKind::PlusRange(expr, _) => self.xfrm_expr(expr),
                    expr::DimKind::MinusRange(expr, _) => self.xfrm_expr(expr),
                }
            }
            expr::ExprKind::Member(parent, _) => self.xfrm_expr(parent),
            expr::ExprKind::SysTfCall(..) => unimplemented!(),
            expr::ExprKind::FuncCall { .. } => (),
            expr::ExprKind::ConstCast(..) => unimplemented!(),
            expr::ExprKind::TypeCast(_, rhs) => self.xfrm_expr(rhs),
            expr::ExprKind::SignCast(..) => unimplemented!(),
            expr::ExprKind::WidthCast(_, rhs) => self.xfrm_expr(rhs),
            expr::ExprKind::Unary(_, expr) => self.xfrm_expr(expr),
            expr::ExprKind::Binary(lhs, _, rhs) => {
                self.xfrm_expr(lhs);
                self.xfrm_expr(rhs);
            }
            expr::ExprKind::PrefixIncDec(..) => unimplemented!(),
            expr::ExprKind::PostfixIncDec(..) => unimplemented!(),
            expr::ExprKind::Assign(lhs, rhs) |
            expr::ExprKind::NonblockAssign(lhs, rhs) => {
                self.xfrm_expr(lhs);
                self.xfrm_expr(rhs);
            }
            expr::ExprKind::BinaryAssign(..) => unimplemented!(),
            expr::ExprKind::Paren(expr) => self.xfrm_expr(expr),
            expr::ExprKind::MinTypMax(..) => unimplemented!(),
            expr::ExprKind::Cond(cond, t, f) => {
                self.xfrm_expr(cond);
                self.xfrm_expr(t);
                self.xfrm_expr(f);
            }
        }
    }

    // Part of second stage. Transform ports list, expand instance arrays into list of individual
    // instances.
    fn xfrm_ports(&mut self, ports: Vec<Option<expr::Expr>>) -> Vec<Option<expr::Expr>> {
        let mut new_list = Vec::new();
        for mut port in ports {
            if let Some(mut port) = port {
                if let expr::Expr{value: expr::ExprKind::HierName(ref mut id), span, .. } = port {
                    let hier = self.xfrm_hier_id(id);
                    match hier {
                        HierItem::Instance(ref decl) => {
                            // Only transform arrays
                            if !decl.dim.is_empty() {
                                // TODO: Deal with more dimensions
                                assert!(decl.dim.len() == 1);
                                let (lb, ub) = decl.dim[0];
                                let (lb, ub) = (cmp::min(lb, ub), cmp::max(lb, ub));
                                for i in lb..=ub {
                                    let mut id_clone = id.clone();
                                    match id_clone {
                                        ast::HierId::Name(_, ref mut name) |
                                        ast::HierId::Member(_, ref mut name) => {
                                            name.value = format!("{}_{}", name.value, i);
                                        }
                                        _ => unreachable!(),
                                    };
                                    new_list.push(Some(expr::Expr {
                                        value: expr::ExprKind::HierName(id_clone),
                                        ty: ty::Ty::Void,
                                        span,
                                    }))
                                }
                                continue;
                            }
                        }
                        _ => (),
                    }
                } else {
                    self.xfrm_expr(&mut port);
                }
                new_list.push(Some(port));
            } else {
                new_list.push(None);
            }
        }
        new_list
    }

    /// Second stage of instance array elimination: transform expressions.
    /// This function requires scope to be taken away and then placed back because we need to build
    /// up self.scopes to be able to resolve expressions.
    pub fn xfrm_scope(&mut self, scope: HierScope) -> HierScope {
        self.scopes.push(HierScope {
            items: Vec::new(),
            names: scope.names,
            symbols: scope.symbols,
        });
        for mut item in scope.items {
            match &mut item {
                HierItem::Instance(decl) => {
                    if decl.dim.is_empty() {
                        // If this is not an array, visit its own ports
                        ::util::replace_with(&mut Rc::get_mut(decl).unwrap().port, |ports| self.xfrm_ports(ports));
                    } else {
                        // Otherwise we visit ports of its clones instead
                        let ptr = &**decl as *const _ as usize;
                        let (lb, ub, mut map) = self.map.remove(&ptr).unwrap();
                        for (_, inst) in &mut map {
                            if let HierItem::Instance(decl) = inst {
                                ::util::replace_with(&mut Rc::get_mut(decl).unwrap().port, |ports| self.xfrm_ports(ports));
                            } else { unreachable!() }
                        }
                        self.map.insert(ptr, (lb, ub, map));
                    }
                }
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        ::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| self.xfrm_scope(scope));
                    }
                }
                HierItem::ContinuousAssign(expr) => {
                    self.xfrm_expr(Rc::get_mut(expr).unwrap());
                }
                HierItem::GenBlock(genblk) => {
                    ::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| self.xfrm_scope(scope));
                }
                // TODO: There're more cases where we need to visit expressions
                _ => (),
            }
            self.scopes.last_mut().unwrap().items.push(item);
        }
        self.scopes.pop().unwrap()
    }

    /// Third stage of instance array elimination: expand arrays.
    pub fn expand_array(&mut self, scope: HierScope) -> HierScope {
        self.scopes.push(HierScope::new());
        for mut item in scope.items {
            let ident = match item {
                HierItem::Instance(decl) => {
                    // This is not an array, return as is.
                    if decl.dim.is_empty() {
                        let ident = decl.name.clone();
                        self.scopes.last_mut().unwrap().insert(Some(ident), HierItem::Instance(decl));
                        continue;
                    }
                    let ptr = &*decl as *const _ as usize;
                    let (lb, ub, mut map) = self.map.remove(&ptr).unwrap();
                    for i in lb..=ub {
                        let inst = map.remove(&i).unwrap();
                        if let HierItem::Instance(decl) = inst {
                            let ident = decl.name.clone();
                            self.scopes.last_mut().unwrap().insert(Some(ident), HierItem::Instance(decl));
                        } else { unreachable!() }
                    }
                    continue;
                }
                HierItem::InterfacePort(decl) => {
                    // This is not an array, return as is.
                    if decl.dim.is_empty() {
                        let ident = decl.name.clone();
                        self.scopes.last_mut().unwrap().insert(Some(ident), HierItem::InterfacePort(decl));
                        continue;
                    }
                    let ptr = &*decl as *const _ as usize;
                    let (lb, ub, mut map) = self.map.remove(&ptr).unwrap();
                    for i in lb..=ub {
                        let inst = map.remove(&i).unwrap();
                        if let HierItem::InterfacePort(decl) = inst {
                            let ident = decl.name.clone();
                            self.scopes.last_mut().unwrap().insert(Some(ident), HierItem::InterfacePort(decl));
                        } else { unreachable!() }
                    }
                    continue;
                }
                HierItem::Design(ref mut decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        ::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| self.expand_array(scope));
                    }
                    None
                }
                HierItem::GenBlock(ref mut genblk) => {
                    ::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| self.expand_array(scope));
                    genblk.name.as_ref().map(|name| Ident::clone(name))
                }
                ref item => super::common::name_of(item).map(Ident::clone),
            };
            self.scopes.last_mut().unwrap().insert(ident, item);
        }
        self.scopes.pop().unwrap()
    }
}
