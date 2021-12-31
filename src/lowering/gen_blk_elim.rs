//! After elaboration we have each iteration of gen blkerate construct evaluated already.
//! This construct, however, cannot be translated back to source code directly. This pass will
//! lower gen blkerate constructs into generate blocks, assigning new names to each iterations.
//!
//! This pass must be run after gen_name_assign.

use num::ToPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

use crate::elaborate::eht_visit::EhtVisitor;
use crate::elaborate::expr;
use crate::elaborate::hier::{self, HierItem, HierScope};
use crate::elaborate::ty;
use crate::syntax::ast::{self, Ident};
use crate::syntax::tokens;

pub fn gen_blk_elim(source: hier::Source) -> hier::Source {
    let mut elim = GenBlkEliminator {
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

struct GenBlkEliminator {
    scopes: Vec<HierScope>,
    units: Vec<HierScope>,
    pkgs: HashMap<String, hier::PkgDecl>,
    structs: Vec<Rc<ty::Struct>>,
    enums: Vec<Rc<ty::Enum>>,

    /// Mapping from the gen blks to individual unrolled instances
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

impl GenBlkEliminator {
    pub fn visit(&mut self, mut units: Vec<HierScope>) {
        // We perform this operation in three steps:
        // * First, assign new names to each item nested directly within the generate block
        // * In second step, we visit all expressions and replace names
        // * In the third step, we eliminate generate blocks and expand them.
        for unit in &mut units {
            self.prepare_genblks(unit);
        }
        let units: Vec<_> = units
            .into_iter()
            .map(|scope| self.xfrm_scope(scope))
            .collect();
        self.units = units
            .into_iter()
            .map(|scope| self.expand_genblk(scope))
            .collect();
    }

    /// First stage of gen blk elimination: assign names to nested items
    pub fn prepare_genblks(&mut self, scope: &mut HierScope) {
        for item in &mut scope.items {
            match item {
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        self.prepare_genblks(&mut Rc::get_mut(inst).unwrap().scope);
                    }
                }
                HierItem::GenBlock(genblk) => {
                    // We will need to assign names to each nested items first before going
                    // further down. This is because nested genblks need to propagate down with
                    // its new name.
                    let genblk = Rc::get_mut(genblk).unwrap();
                    let blk_name = genblk.name.as_ref().unwrap();
                    for item in &mut genblk.scope.items {
                        // Get the name of the item
                        if let Some(ident) = super::common::name_of_mut(item) {
                            // Prefix the ident with genblk's name
                            ident.value = format!("{}_{}", blk_name, ident);
                        }
                    }

                    // Propagate down the scope
                    self.prepare_genblks(&mut genblk.scope);
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
                        let item = 'resolve_loop: loop {
                            for scope in self.scopes.iter().rev() {
                                if let Some(v) = scope.find(&name) {
                                    break 'resolve_loop v.clone();
                                }
                            }
                            unreachable!("cannot find name {}", name)
                        };
                        // Replace with actual name
                        **name = super::common::name_of(&item).unwrap().clone();
                        item
                    }
                    _ => unimplemented!(),
                };
            }
            ast::HierId::Member(parent, name) => {
                let parent_hier = self.xfrm_hier_id(parent);
                match parent_hier {
                    HierItem::InterfacePort(decl) => {
                        return decl.inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::Instance(decl) => {
                        return decl.inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::InstancePart { inst, .. } => {
                        return inst.get_instance().scope.find(&name).unwrap().clone()
                    }
                    HierItem::GenBlock(decl) => {
                        let item = decl.scope.find(&name).unwrap().clone();
                        let new_name = super::common::name_of(&item).unwrap().clone();
                        // Remove prefixing parent and replace with actual name
                        match &mut parent.value {
                            ast::HierId::Name(None, name) | ast::HierId::Member(_, name) => {
                                **name = new_name
                            }
                            _ => unimplemented!(),
                        }
                        let repl = std::mem::replace(&mut parent.value, ast::HierId::Root);
                        (repl, item)
                    }
                    _ => unreachable!(),
                }
            }
            ast::HierId::Select(parent, _) => {
                let parent_hier = self.xfrm_hier_id(parent);
                return match parent_hier {
                    HierItem::Instance(ref inst) => HierItem::InstancePart {
                        inst: inst.inst.clone(),
                        modport: None,
                        dim: inst.dim.iter().skip(1).map(Clone::clone).collect(),
                    },
                    HierItem::InterfacePort(ref decl) => HierItem::InstancePart {
                        inst: decl.inst.clone(),
                        modport: decl.modport.clone(),
                        dim: decl.dim.iter().skip(1).map(Clone::clone).collect(),
                    },
                    _ => unreachable!(),
                };
            }
            _ => unimplemented!(),
        };
        *id = repl;
        hier
    }

    /// Second stage of gen blk elimination: transform expressions.
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
                        if let hier::PortConn::Expr(port) = port {
                            self.visit_expr(port)
                        }
                    }
                }
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        crate::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| {
                            self.xfrm_scope(scope)
                        });
                    }
                }
                HierItem::ContinuousAssign(expr) => {
                    self.visit_expr(Rc::get_mut(expr).unwrap());
                }
                HierItem::Always(_, stmt) => {
                    self.visit_stmt(Rc::get_mut(stmt).unwrap());
                }
                HierItem::GenBlock(genblk) => {
                    crate::util::replace_with(&mut Rc::get_mut(genblk).unwrap().scope, |scope| {
                        self.xfrm_scope(scope)
                    });
                }
                // TODO: There're more cases where we need to visit expressions
                _ => (),
            }
            self.scopes.last_mut().unwrap().items.push(item);
        }
        self.scopes.pop().unwrap()
    }

    /// Third stage of gen blk elimination: expand gen blkerate constructs.
    pub fn expand_genblk(&mut self, scope: HierScope) -> HierScope {
        self.scopes.push(HierScope::new());
        for item in scope.items {
            self.expand_item(item);
        }
        self.scopes.pop().unwrap()
    }

    pub fn expand_item(&mut self, mut item: HierItem) {
        let ident = match item {
            HierItem::Design(ref mut decl) => {
                for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                    crate::util::replace_with(&mut Rc::get_mut(inst).unwrap().scope, |scope| {
                        self.expand_genblk(scope)
                    });
                }
                None
            }
            HierItem::GenBlock(ref mut genblk) => {
                let items =
                    std::mem::replace(&mut Rc::get_mut(genblk).unwrap().scope.items, Vec::new());
                for item in items {
                    self.expand_item(item);
                }
                return;
            }
            ref item => super::common::name_of(item).map(Ident::clone),
        };
        self.scopes.last_mut().unwrap().insert(ident, item);
    }
}

impl EhtVisitor for GenBlkEliminator {
    fn visit_expr(&mut self, expr: &mut expr::Expr) {
        match expr.value {
            expr::ExprKind::HierName(ref mut id) => {
                self.xfrm_hier_id(id);
            }
            _ => self.do_visit_expr(expr),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut expr::Stmt) {
        match &mut stmt.value {
            expr::StmtKind::For {
                ty: Some(ty),
                init,
                cond,
                update,
                body,
            } => {
                // The scope for initialisers
                self.scopes.push(HierScope::new());
                for expr in init.iter_mut() {
                    if let expr::ExprKind::Assign(lhs, _) = &expr.value {
                        if let expr::ExprKind::HierName(ast::HierId::Name(None, name)) = &lhs.value
                        {
                            self.scopes.last_mut().unwrap().insert(
                                Some(Ident::clone(name)),
                                HierItem::DataDecl(Rc::new(hier::DataDecl {
                                    lifetime: ast::Lifetime::Automatic,
                                    ty: ty::Ty::clone(ty),
                                    name: Ident::clone(name),
                                    // We will process initialisers later, set it to none here.
                                    init: None,
                                })),
                            );
                        } else {
                            unreachable!()
                        }
                    } else {
                        unreachable!();
                    }
                }
                for expr in init {
                    self.visit_expr(expr);
                }
                if let Some(expr) = cond {
                    self.visit_expr(expr);
                }
                for expr in update {
                    self.visit_expr(expr);
                }
                self.visit_stmt(body);
                self.scopes.pop();
                return;
            }
            expr::StmtKind::SeqBlock(list) => {
                self.scopes.push(HierScope::new());
                list.iter_mut().for_each(|stmt| self.visit_stmt(stmt));
                self.scopes.pop();
                return;
            }
            expr::StmtKind::DataDecl(decl) => {
                {
                    let decl = Rc::get_mut(decl).unwrap();
                    if let Some(expr) = &mut decl.init {
                        self.visit_expr(expr);
                    }
                }
                self.scopes.last_mut().unwrap().insert(
                    Some(Ident::clone(&decl.name)),
                    HierItem::DataDecl(Rc::clone(decl)),
                );
                return;
            }
            _ => (),
        }
        self.do_visit_stmt(stmt);
    }
}
