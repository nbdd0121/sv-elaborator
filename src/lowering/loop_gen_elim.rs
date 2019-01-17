//! After elaboration we have each iteration of loop generate construct evaluated already.
//! This construct, however, cannot be translated back to source code directly. This pass will
//! lower loop generate constructs into generate blocks, assigning new names to each iterations.
//!
//! This pass must be run after gen_name_assign.

use std::rc::Rc;
use std::collections::HashMap;
use num::ToPrimitive;

use syntax::tokens;
use syntax::ast::{self, Ident};
use elaborate::ty;
use elaborate::expr;
use elaborate::hier::{self, HierScope, HierItem};
use elaborate::eht_visit::EhtVisitor;

pub fn loop_gen_elim(source: hier::Source) -> hier::Source {
    let mut elim = LoopGenEliminator {
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

struct LoopGenEliminator {
    scopes: Vec<HierScope>,
    units: Vec<HierScope>,
    pkgs: HashMap<String, hier::PkgDecl>,
    structs: Vec<Rc<ty::Struct>>,
    enums: Vec<Rc<ty::Enum>>,
    
    /// Mapping from the loop gens to individual unrolled instances
    map: HashMap<usize, HashMap<i32, HierItem>>,
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

impl LoopGenEliminator {
    pub fn visit(&mut self, mut units: Vec<HierScope>) {
        // We perform this operation in three steps:
        // * First, assign new names to each generate block
        // * In second step, we visit all expressions and replace HierItem::Select with reference
        //   to new generate block
        // * In the third step, we eliminate loop generate constructs and expand them.
        for unit in &mut units {
            self.prepare_genblks(unit);
        }
        let units: Vec<_> = units.into_iter().map(|scope| self.xfrm_scope(scope)).collect();
        self.units = units.into_iter().map(|scope| self.expand_loopgen(scope)).collect();
    }

    /// First stage of loop gen elimination: assign names to each generate block
    pub fn prepare_genblks(&mut self, scope: &mut HierScope) {
        for item in &mut scope.items {
            match item {
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        self.prepare_genblks(&mut Rc::get_mut(inst).unwrap().scope);
                    }
                }
                HierItem::GenBlock(genblk) => {
                    self.prepare_genblks(&mut Rc::get_mut(genblk).unwrap().scope);
                }
                HierItem::LoopGenBlock(loopgenblk) => {
                    for (val, genblk) in loopgenblk.instances.borrow_mut().iter_mut() {
                        let genblk_mut = Rc::get_mut(genblk).unwrap();
                        // Prepare nested scopes
                        self.prepare_genblks(&mut genblk_mut.scope);
                        // Then assign names
                        let mut new_name = Ident::new_unspanned(
                            format!("{}_{}", loopgenblk.name.as_ref().unwrap(), val)
                        );
                        new_name.symbol = ast::SymbolId::allocate();
                        genblk_mut.name = Some(new_name);
                    }
                }
                _ => (),
            }
        }
    }

    /// Transform an hierachical identifier and return its resolved result.
    /// Currently if resolution fails, HierItem::OtherName is returned.
    pub fn xfrm_hier_id(&mut self, id: &mut ast::HierId) -> HierItem {
        let (repl, hier) = match id {
            ast::HierId::Name(scope, name) => {
                return match scope {
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
                return match parent_hier {
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
            ast::HierId::Select(parent, sel) => {
                let parent_hier = self.xfrm_hier_id(parent);
                let sel = dim_to_i32(&sel);
                let hier = match parent_hier {
                    HierItem::Instance(ref inst) => {
                        return HierItem::InstancePart {
                            inst: inst.inst.clone(),
                            modport: None,
                            dim: inst.dim.iter().skip(1).map(Clone::clone).collect(),
                        }
                    }
                    HierItem::InterfacePort(ref decl) => {
                        return HierItem::InstancePart {
                            inst: decl.inst.clone(),
                            modport: decl.modport.clone(),
                            dim: decl.dim.iter().skip(1).map(Clone::clone).collect(),
                        }
                    }
                    HierItem::LoopGenBlock(ref decl) => {
                        let genblk = decl.instances.borrow().iter().find(|(num, _)| num == &sel).unwrap().1.clone();
                        // Mutate the parent's name to include the dimension
                        match parent.value {
                            ast::HierId::Name(_, ref mut name) |
                            ast::HierId::Member(_, ref mut name) => {
                                *name = Box::new(genblk.name.as_ref().unwrap().clone());
                            }
                            _ => unreachable!(),
                        }
                        HierItem::GenBlock(genblk)
                    }
                    _ => unreachable!(),
                };
                // Replace parent
                (std::mem::replace(&mut **parent, ast::Spanned::new_unspanned(ast::HierId::Root)), hier)
            }
            _ => unimplemented!(),
        };
        *id = repl.value;
        hier
    }

    /// Second stage of loop gen elimination: transform expressions.
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
                    for port in &mut Rc::get_mut(decl).unwrap().port {
                        if let Some(port) = port { self.visit_expr(port) }
                    }
                }
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        ::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| self.xfrm_scope(scope));
                    }
                }
                HierItem::ContinuousAssign(expr) => {
                    self.visit_expr(Rc::get_mut(expr).unwrap());
                }
                HierItem::GenBlock(genblk) => {
                    ::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| self.xfrm_scope(scope));
                }
                HierItem::LoopGenBlock(ref loopgenblk) => {
                    for (_, genblk) in loopgenblk.instances.borrow_mut().iter_mut() {
                        ::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| self.xfrm_scope(scope));
                    }
                }
                // TODO: There're more cases where we need to visit expressions
                _ => (),
            }
            self.scopes.last_mut().unwrap().items.push(item);
        }
        self.scopes.pop().unwrap()
    }

    /// Third stage of loop gen elimination: expand loop generate constructs.
    pub fn expand_loopgen(&mut self, scope: HierScope) -> HierScope {
        self.scopes.push(HierScope::new());
        for mut item in scope.items {
            let ident = match item {
                HierItem::Design(ref mut decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        ::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| self.expand_loopgen(scope));
                    }
                    None
                }
                HierItem::GenBlock(ref mut genblk) => {
                    ::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| self.expand_loopgen(scope));
                    Some(genblk.name.as_ref().unwrap().clone())
                }
                HierItem::LoopGenBlock(loopgenblk) => {
                    let loopgenblk = Rc::try_unwrap(loopgenblk).unwrap_or_else(|_| unreachable!());
                    for (_, mut genblk) in loopgenblk.instances.into_inner() {
                        ::util::replace_with(&mut Rc::get_mut(&mut genblk).unwrap().scope, |scope| self.expand_loopgen(scope));
                        let name = genblk.name.clone();
                        self.scopes.last_mut().unwrap().insert(name, HierItem::GenBlock(genblk));
                    }
                    continue;
                }
                ref item => super::common::name_of(item).map(Ident::clone),
            };
            self.scopes.last_mut().unwrap().insert(ident, item);
        }
        self.scopes.pop().unwrap()
    }
}

impl EhtVisitor for LoopGenEliminator {
    fn visit_expr(&mut self, expr: &mut expr::Expr) {
        match expr.value {
            expr::ExprKind::HierName(ref mut id) => {
                self.xfrm_hier_id(id);
            }
            _ => self.do_visit_expr(expr),
        }
    }
}
