//! Assign names to each generate block. When elaborating each generate block already have an id
//! assigned. In case that the generate block has no externally accessible names it will be named
//! according to the id. This pass will facilitate generate block elimination.

use std::rc::Rc;

use syntax::ast::Ident;
use elaborate::hier::{self, HierScope, HierItem};

pub fn gen_name_assign(source: &mut hier::Source) {
    for unit in &mut source.units {
        assign_name(unit);
    }
}

fn assign_name(scope: &mut HierScope) {
    for item in &mut scope.items {
        match item {
            HierItem::Design(decl) => {
                for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                    assign_name(&mut Rc::get_mut(inst).unwrap().scope);
                }
            }
            HierItem::GenBlock(genblk) => {
                let genblk = Rc::get_mut(genblk).unwrap();
                if genblk.name.is_none() {
                    let mut number_part = format!("{}", genblk.id.unwrap());
                    let mut full_name = format!("genblk{}", number_part);
                    while scope.names.contains_key(&full_name) {
                        number_part = format!("0{}", number_part);
                        full_name = format!("genblk{}", number_part);
                    }
                    genblk.name = Some(Ident::new_unspanned(full_name));
                }
                assign_name(&mut genblk.scope);
            }
            HierItem::LoopGenBlock(loopgenblk) => {
                let loopgenblk = Rc::get_mut(loopgenblk).unwrap();
                if loopgenblk.name.is_none() {
                    let mut number_part = format!("{}", loopgenblk.id);
                    let mut full_name = format!("genblk{}", number_part);
                    while scope.names.contains_key(&full_name) {
                        number_part = format!("0{}", number_part);
                        full_name = format!("genblk{}", number_part);
                    }
                    loopgenblk.name = Some(Ident::new_unspanned(full_name));
                }
                for (_, genblk) in loopgenblk.instances.borrow_mut().iter_mut() {
                    let genblk_mut = Rc::get_mut(genblk).unwrap();
                    assign_name(&mut genblk_mut.scope);
                }
            }
            _ => (),
        }
    }
}
