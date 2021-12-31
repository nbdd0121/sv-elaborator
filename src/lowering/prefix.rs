//! Prefix all compilation-unit level design units with a given prefix, except blackboxes and
//! toplevels.

use crate::elaborate::hier::{self, HierItem};
use crate::opts::Opts;
use std::rc::Rc;

pub fn prefix(mut source: hier::Source, opts: &Opts) -> hier::Source {
    for unit in &mut source.units {
        for item in &mut unit.items {
            match item {
                HierItem::Design(decl) => {
                    for (_, inst) in decl.instances.borrow_mut().iter_mut() {
                        let inst = Rc::get_mut(inst).unwrap();
                        // If a prefix is specified, we also give all modules instantiated in such way a prefix.
                        // Blackbox and toplevel modules are unprefixed.
                        let is_blackbox = opts.blackbox.iter().any(|item| &inst.name.value == item);
                        let is_toplevel = opts.toplevel == inst.name.value;
                        if !is_blackbox && !is_toplevel {
                            inst.name.value =
                                format!("{}{}", opts.prefix.as_ref().unwrap(), inst.name.value);
                        }
                    }
                }
                _ => (),
            }
        }
    }
    source
}
