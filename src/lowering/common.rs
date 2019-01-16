use std::rc::Rc;

use syntax::ast::Ident;
use elaborate::hier::HierItem;

pub fn name_of_mut(item: &mut HierItem) -> Option<&mut Ident> {
    match item {
        HierItem::Instance(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::InterfacePort(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::Param(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::Type(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::DataPort(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::Design(_) => unimplemented!(),
        HierItem::DataDecl(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::FuncDecl(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::ContinuousAssign(..) => None,
        HierItem::Other(..) => None,
        HierItem::InstancePart { .. } => unreachable!(),
        HierItem::GenBlock(genblk) => Rc::get_mut(genblk).unwrap().name.as_mut(),
        HierItem::GenVar(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::LoopGenBlock(_) => unimplemented!(),
        HierItem::Modport(decl) => Some(&mut Rc::get_mut(decl).unwrap().name),
        HierItem::Enum(..) => {
            // TODO: Ignore for now
            None
        }
    }
}

pub fn name_of(item: &HierItem) -> Option<&Ident> {
    match item {
        HierItem::Instance(decl) => Some(&decl.name),
        HierItem::InterfacePort(decl) => Some(&decl.name),
        HierItem::Param(decl) => Some(&decl.name),
        HierItem::Type(decl) => Some(&decl.name),
        HierItem::DataPort(decl) => Some(&decl.name),
        HierItem::Design(_) => unimplemented!(),
        HierItem::DataDecl(decl) => Some(&decl.name),
        HierItem::FuncDecl(decl) => Some(&decl.name),
        HierItem::ContinuousAssign(..) => None,
        HierItem::Other(..) => None,
        HierItem::InstancePart { .. } => unreachable!(),
        HierItem::GenBlock(genblk) => genblk.name.as_ref(),
        HierItem::GenVar(decl) => Some(&decl.name),
        HierItem::LoopGenBlock(_) => unimplemented!(),
        HierItem::Modport(decl) => Some(&decl.name),
        HierItem::Enum(..) => {
            // TODO: Ignore for now
            None
        }
    }
}
