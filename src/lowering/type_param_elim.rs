//! Convert type parameters into local typedefs. This will break HierScope's `names` and `symbols`
//! indices.

use std::rc::Rc;

use crate::elaborate::expr::Val;
use crate::elaborate::hier::{self, HierItem, TypedefDecl};
use crate::elaborate::ty::Ty;

pub fn type_param_elim(source: &mut hier::Source) {
    let mut elim = TypeParamEliminator {};
    elim.visit(source)
}

struct TypeParamEliminator {}

impl TypeParamEliminator {
    pub fn visit_item(&mut self, item: &mut HierItem) {
        match item {
            HierItem::Design(decl) => {
                for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                    self.visit_instantiation(Rc::get_mut(inst).unwrap());
                }
            }
            HierItem::GenBlock(genblk) => {
                for item in &mut Rc::get_mut(genblk).unwrap().scope.items {
                    self.visit_item(item);
                }
            }
            HierItem::LoopGenBlock(loopgenblk) => {
                for (_, genblk) in loopgenblk.instances.borrow_mut().iter_mut() {
                    for item in &mut Rc::get_mut(genblk).unwrap().scope.items {
                        self.visit_item(item);
                    }
                }
            }
            _ => (),
        }
    }

    pub fn visit_instantiation(&mut self, inst: &mut hier::DesignInstantiation) {
        crate::util::replace_with(&mut inst.scope.items, |items| {
            let mut ports = Vec::new();
            let mut others = Vec::new();

            for item in items {
                match item {
                    HierItem::Param(decl) => {
                        if let Ty::Type = decl.ty {
                            others.push(HierItem::Type(Rc::new(TypedefDecl {
                                ty: if let Val::Type(ref ty) = decl.init {
                                    ty.clone()
                                } else {
                                    unreachable!()
                                },
                                name: decl.name.clone(),
                            })))
                        } else {
                            others.push(HierItem::Param(decl))
                        }
                    }
                    HierItem::DataPort(decl) => ports.push(HierItem::DataPort(decl)),
                    HierItem::InterfacePort(decl) => ports.push(HierItem::InterfacePort(decl)),
                    mut item => {
                        self.visit_item(&mut item);
                        others.push(item)
                    }
                }
            }

            ports.extend(others);
            ports
        })
    }

    pub fn visit(&mut self, source: &mut hier::Source) {
        for unit in &mut source.units {
            for item in &mut unit.items {
                self.visit_item(item);
            }
        }
    }
}
