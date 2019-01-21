//! Elaborator will essentially instantiate all parameterised modules and modules with interface
//! ports, evaluate all constant expressions to an actual constant representation, expand all
//! typedefs to concrete types, and give all user-defined structs/enums a global accessible names
//! (which will enable expansion of type parameters). After elaboration, all user-defined structs
//! or enums will be presented in the top-level and are type-defed directly. There shall be no
//! typedefs elsewhere in the code.
//!
//! In most languages we first perform symbol resolutions, type check & annotations etc, but
//! SystemVerilog is ill-designed therefore we face many challenges:
//! * due to the existence of interfaces and generate constructs. To support interface typedef we
//!   will be unable to do type checks until later.
//! * Parameters can be untyped and they will be dynamically assigned with a type according to
//!   their parameter.
//! * Expression sizes can be self-determined or context-determined. If their sizes are context-
//!   determined, their size depend on the largest operand in the entire enclosing expression.
//! * The spec is really vague and messy.

use syntax::ast::{self, *};
use syntax::tokens::*;
use source::*;
use number::{LogicValue, LogicVec, Int};

use num::{BigUint, Zero, ToPrimitive, FromPrimitive};
use std::cmp;
use std::collections::HashMap;

use std::rc::Rc;
use std::cell::RefCell;

use super::ty::{IntTy, Struct};
use super::hier::{self, Ty, Val, HierItem, HierScope};
use super::ty;
use super::expr;

pub fn elaborate(diag: &DiagMgr, items: &Vec<Vec<Item>>, toplevel: &str) -> hier::Source {
    let mut elaborator = Elaborator::new(diag, toplevel);
    elaborator.elaborate(items);
    hier::Source {
        units: elaborator.units,
        pkgs: elaborator.pkgs,
        structs: elaborator.structs,
        enums: elaborator.enums,
    }
}

struct Elaborator<'a> {
    diag: &'a DiagMgr,
    toplevel_name: &'a str,

    scopes: Vec<HierScope>,
    genblk: usize,

    units: Vec<HierScope>,

    /// All packages
    pkgs: HashMap<String, hier::PkgDecl>,

    /// All elaborated structures.
    structs: Vec<Rc<Struct>>,
    enums: Vec<Rc<ty::Enum>>,
}

impl<'a> Elaborator<'a> {

    pub fn new(diag: &'a DiagMgr, toplevel: &'a str) -> Elaborator<'a> {
        Elaborator {
            diag: diag,
            toplevel_name: toplevel,

            scopes: Vec::new(),
            genblk: 0,

            units: Vec::new(),
            pkgs: HashMap::new(),
            structs: Vec::new(),
            enums: Vec::new(),
        }
    }

    fn resolve_opt(&self, name: &Ident) -> Option<HierItem> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.symbols.get(&name.symbol) {
                return Some(scope.items[*v].clone())
            }
        }
        None
    }

    fn resolve(&self, name: &Ident) -> HierItem {
        match self.resolve_opt(name) {
            None => self.diag.report_fatal(format!("cannot find identifier {} in current scope", name), name.span),
            Some(v) => v,
        }
    }

    /// Insert the identifier into scope.
    pub fn add_to_scope(&mut self, ident: &Ident, item: HierItem) {
        self.scopes.last_mut().unwrap().insert(Some(ident.clone()), item);
    }

    pub fn add_item(&mut self, item: HierItem) {
        // Add to current item list
        self.scopes.last_mut().unwrap().insert(None, item);
    }

    /// This will instantiate a parameterised module.
    pub fn instantiate_design(
        &mut self, decl: &Rc<hier::DesignDecl>, param: Rc<hier::DesignParam>
    ) {
        // Create new hiearchy scope.
        let genblk_saved = self.genblk;
        self.genblk = 0;
        self.scopes.push(HierScope::new());

        // Add instantiated parameters to the scope.
        for decl in param.param.iter() {
            let name = decl.name.clone();
            let declitem = HierItem::Param(Rc::new(hier::ParamDecl::clone(decl)));
            self.add_to_scope(&name, declitem);
        }

        // Resolve ports
        for port in &decl.ast.port {
            match port {
                PortDecl::Data(dir, net, ty, list) => {
                    let ty = self.eval_ty(ty);
                    for assign in list {
                        assert!(assign.dim.len() == 0);
                        // Default net type to wire. Technically we don't need to do this but some
                        // SystemVerilog implementation incorrectly assumes the net port type to
                        // be var when a data type is present.
                        let netty = if let NetPortType::Default = net {
                            NetPortType::Builtin(NetTy::Wire)
                        } else {
                            net.clone()
                        };
                        let declitem = HierItem::DataPort(Rc::new(hier::DataPortDecl {
                            dir: *dir,
                            net: netty,
                            ty: ty.clone(),
                            name: assign.name.clone(),
                            init: assign.init.clone(),
                        }));
                        self.add_to_scope(&assign.name, declitem);
                    }
                }
                PortDecl::Interface(_intf, modport, list) => {
                    for assign in list {
                        let instance = param.intf[&assign.name.value].clone();
                        let modport = match modport {
                            None => None,
                            Some(modport) => {
                                match instance.get_instance().scope.find(&modport) {
                                    Some(HierItem::Modport(modport)) => Some(modport.clone()),
                                    _ => {
                                        self.diag.report_fatal(
                                            format!("cannot find modport {}", modport.value),
                                            modport.span
                                        );
                                    }
                                }
                            }
                        };
                        let dim = self.eval_const_unpacked_dim(&assign.dim);
                        let declitem = HierItem::InterfacePort(Rc::new(hier::InterfacePortDecl {
                            inst: instance,
                            modport: modport,
                            name: assign.name.clone(),
                            dim: dim,
                        }));
                        self.add_to_scope(&assign.name, declitem);
                    }
                }
                _ => unimplemented!(),
            }
        }

        for item in &decl.ast.items {
            self.elaborate_item(item);
        }

        let scope = self.scopes.pop().unwrap();
        self.genblk = genblk_saved;
        let mut inst_list = decl.instances.borrow_mut();

        // Give this module a name. We use the original name for the first instantiation, and
        // give it a numeric suffix for any extra one.
        let name = if inst_list.is_empty() {
            decl.ast.name.value.clone()
        } else {
            format!("{}_{}", decl.ast.name.value, inst_list.len())
        };

        let inst = Rc::new(hier::DesignInstantiation {
            decl: Rc::downgrade(decl),
            name: Ident::new(name, decl.ast.name.span),
            param: Rc::clone(&param),
            scope: scope,
        });
        inst_list.push((Rc::clone(&param), inst));
    }

    pub fn elaborate_instantiation(&mut self, inst: &HierInstantiation) {
        let item = match self.resolve(&inst.name) {
            HierItem::Design(item) => item,
            _ => unreachable!(),
        };

        // First we are going to evaluate parameters.
        // Introduce a temporary scope for dependent parameters.
        let genblk_saved = self.genblk;
        self.genblk = 0;
        self.scopes.push(HierScope::new());

        if let Some(param) = &item.ast.param {
            // First build list of parameter names
            let param_names: Vec<_> = param
                .iter()
                // Only count parameters and ignore localparam
                .filter(|v| v.kw == Keyword::Parameter)
                .flat_map(|v| v.list.iter().map(|v| v.name.value.clone()))
                .collect();

            // Next rearrange parameters into positional ones
            let args = match inst.param.as_ref() {
                None => vec![None; param_names.len()],
                Some(args) => {
                    if args.named.is_empty() {
                        if args.ordered.len() > param_names.len() {
                            self.diag.report_error(
                                "instantiation contains more parameters than declared",
                                inst.name.span
                            );
                        }
                        let mut args: Vec<_> = args.ordered.iter().map(Option::as_ref).collect();
                        args.resize(param_names.len(), None);
                        args
                    } else {
                        let mut new_list = vec![None; param_names.len()];
                        for (name, expr) in &args.named {
                            let id = match param_names.iter().position(|port_name| port_name == &name.value) {
                                None => {
                                    self.diag.report_error(
                                        format!("no parameter named {} is declared", name),
                                        name.span
                                    );
                                    continue
                                }
                                Some(id) => id,
                            };
                            if !new_list[id].is_none() {
                                self.diag.report_error(
                                    "duplicate parameter overriders",
                                    name.span
                                )
                            }
                            new_list[id] = Some(expr.as_ref());
                        }
                        new_list
                            .into_iter()
                            .map(|v| v.unwrap_or(None))
                            .collect()
                    }
                }
            };

            let mut index = 0;
            for param in param {
                // Evaluate the type of this parameter
                let ty = param.ty.as_ref().map(|ty| self.eval_ty(ty));
                for assign in &param.list {
                    // For parameter, need to check if we have overriders.
                    let expr = if param.kw == Keyword::Parameter {
                        let expr = args[index];
                        index += 1;
                        expr
                    } else { None };
                    // If there's no override use default initialiser instead.
                    let expr = expr.or(assign.init.as_ref());
                    // It's an error if there're still no initialiser
                    let expr = if let Some(v) = expr { v } else {
                        self.diag.report_error(format!("parameter {} has no default assignment and is not overridden", assign.name), inst.name.span);
                        return;
                    };

                    // Evaluate the expression. If a ty is specified, then the expression should
                    // be evaluated in assignment-like context.
                    let (ty, val) = match ty {
                        None => self.eval_expr(expr),
                        Some(ref ty) => (ty.clone(), self.eval_expr_assign(expr, ty).1),
                    };

                    // Add it to a temporary scope.
                    let declitem = HierItem::Param(Rc::new(hier::ParamDecl {
                        kw: param.kw,
                        name: assign.name.clone(),
                        ty,
                        init: val,
                    }));
                    self.add_to_scope(&assign.name, declitem);
                }
            }
        }

        // Now tear down the temporary scope and use its content to build a parameter map.
        // This avoids having to clone the values.
        let items = self.scopes.pop().unwrap().items;
        self.genblk = genblk_saved;
        let map = Rc::new(items.into_iter().map(|x| {
            if let HierItem::Param(v) = x {
                Rc::try_unwrap(v).unwrap_or_else(|_| unreachable!())
            } else { unreachable!() }
        }).collect());

        'next_instance: for inst in &inst.inst {
            // Retrieve the list of port connections.
            // resolver has already converted all named port connections to position for us.
            let port_list = match &inst.ports {
                PortConn::Ordered(list) => list,
                _ => unreachable!(),
            };

            // Iterate through interface ports
            let mut index = 0;
            let mut intf_list = HashMap::new();
            for port in &item.ast.port {
                match port {
                    PortDecl::Data(.., list) => index += list.len(),
                    PortDecl::Interface(intf, _, list) => {
                        index += 1;
                        for assign in list {
                            if let Some(ref expr) = assign.init {
                                self.diag.report_error("interface port initializer isn't yet supported", expr.span);
                                continue 'next_instance;
                            }
                            // Make sure the interface port is connected
                            let conn = match &port_list[index - 1] {
                                (_, Some(v)) => v,
                                _ => {
                                    self.diag.report_error(
                                        format!("interface port {} must be connected", assign.name),
                                        inst.name.span
                                    );
                                    continue 'next_instance;
                                }
                            };
                            let name = match &conn.value {
                                ExprKind::HierName(name) => name,
                                _ => {
                                    self.diag.report_error("expected interface instance", conn.span);
                                    continue 'next_instance;
                                }
                            };
                            let hier = match self.type_check_hier_id(name, conn.span).0 {
                                Some(hier) => hier,
                                None => {
                                    self.diag.report_error("expected interface instance", conn.span);
                                    continue 'next_instance;
                                }
                            };
                            // Get the instance
                            let decl = match hier {
                                HierItem::Instance(decl) => decl.inst.clone(),
                                HierItem::InterfacePort(decl) => decl.inst.clone(),
                                HierItem::InstancePart { inst , .. } => inst,
                                _ => {
                                    self.diag.report_error("expected interface instance", conn.span);
                                    continue 'next_instance;
                                }
                            };
                            // Check that it is actually interface
                            if decl.0.ast.kw != Keyword::Interface {
                                self.diag.report_error("expected interface instance", conn.span);
                                continue 'next_instance;
                            }
                            // If the interface port declaration is not using "interface id",
                            // check if the interface actually matches.
                            if let Some(name) = intf {
                                if name.symbol != decl.0.ast.name.symbol {
                                    self.diag.report_error(
                                        format!("expected interface {}, found interface {}", name, decl.0.ast.name),
                                        conn.span
                                    );
                                    continue 'next_instance;
                                }
                            }
                            intf_list.insert(assign.name.value.clone(), decl);
                        }
                    }
                    PortDecl::Explicit(..) => index += 1,
                }
            }

            let map = hier::DesignParam {
                param: Rc::clone(&map),
                intf: intf_list,
            };

            // Search for existing instances.
            let design_inst = hier::DesignInstHandle(Rc::clone(&item), 'outer2: loop {
                for (inst_map, _) in item.instances.borrow().iter() {
                    if &**inst_map == &map {
                        break 'outer2 Rc::clone(inst_map);
                    }
                }
                let param = Rc::new(map);
                self.instantiate_design(&item, Rc::clone(&param));
                break param;
            });

            let port_connections = port_list.iter().map(|(_, port)| {
                // TODO: Should also get types of the ports out
                port.as_ref().map(|port| self.type_check_assign_todo(port))
            }).collect();

            let dim = self.eval_const_unpacked_dim(&inst.dim);
            let declitem = HierItem::Instance(Rc::new(hier::InstanceDecl {
                name: inst.name.clone(),
                inst: design_inst,
                dim,
                port: port_connections,
            }));
            self.add_to_scope(&inst.name, declitem);
        }
    }

    fn elaborate_toplevel(&mut self, module: Rc<hier::DesignDecl>) {
        // First we are going to evaluate parameters.
        // Introduce a temporary scope for dependent parameters.
        let genblk_saved = self.genblk;
        self.genblk = 0;
        self.scopes.push(HierScope::new());

        if let Some(param) = &module.ast.param {
            for param in param {
                // Evaluate the type of this parameter
                let ty = param.ty.as_ref().map(|ty| self.eval_ty(ty));
                for assign in &param.list {
                    // It's an error if there's no initialiser
                    let expr = if let Some(v) = assign.init.as_ref() { v } else {
                        self.diag.report_error(
                            "parameter of top-level module has no default assignment",
                            assign.name.span
                        );
                        return;
                    };

                    // Evaluate the expression
                    let (ty, val) = match ty {
                        None => self.eval_expr(expr),
                        Some(ref ty) => (ty.clone(), self.eval_expr_assign(expr, ty).1),
                    };

                    // Add it to a temporary scope.
                    let declitem = HierItem::Param(Rc::new(hier::ParamDecl {
                        kw: param.kw,
                        name: assign.name.clone(),
                        ty,
                        init: val,
                    }));
                    self.add_to_scope(&assign.name, declitem);
                }
            }
        }

        // Now tear down the temporary scope and use its content to build a parameter map.
        // This avoids having to clone the values.
        let items = self.scopes.pop().unwrap().items;
        self.genblk = genblk_saved;
        let map = Rc::new(items.into_iter().map(|x| {
            if let HierItem::Param(v) = x {
                Rc::try_unwrap(v).unwrap_or_else(|_| unreachable!())
            } else { unreachable!() }
        }).collect());

        // Check that there are no interface ports
        for port in &module.ast.port {
            match port {
                PortDecl::Interface(.., list) => {
                    for assign in list {
                        self.diag.report_fatal("top-level module cannot have interface ports", assign.name.span);
                    }
                }
                _ => (),
            }
        }

        let map = hier::DesignParam {
            param: Rc::clone(&map),
            intf: HashMap::new(),
        };

        // Search for existing instances.
        'outer2: loop {
            for (inst_map, _) in module.instances.borrow().iter() {
                if &**inst_map == &map {
                    break 'outer2;
                }
            }
            self.instantiate_design(&module, Rc::new(map));
            break;
        };
    }

    pub fn elaborate_item(&mut self, item: &Item) {
        match item {
            Item::DesignDecl(decl) => {
                let ident = decl.name.clone();
                let r_decl = hier::DesignDecl {
                    // Need to clone AST here.
                    ast: Rc::new(DesignDecl::clone(decl)),
                    instances: RefCell::new(Vec::new()),
                };
                let item = HierItem::Design(Rc::new(r_decl));
                self.add_to_scope(&ident, item);
            }
            Item::PkgDecl(decl) => {
                // Create new hiearchy scope.
                let genblk_saved = self.genblk;
                self.genblk = 0;
                self.scopes.push(HierScope::new());
                for item in &decl.items {
                    self.elaborate_item(item);
                }
                let scope = self.scopes.pop().unwrap();
                self.genblk = genblk_saved;
                let decl = hier::PkgDecl {
                    name: decl.name.clone(),
                    scope: scope,
                };
                self.pkgs.insert(decl.name.value.clone(), decl);
            }
            Item::FuncDecl(decl) => {
                let ty = self.eval_ty(&decl.ty);
                self.add_to_scope(&decl.name, HierItem::FuncDecl(Rc::new(hier::FuncDecl {
                    lifetime: decl.lifetime,
                    ty: ty,
                    name: decl.name.clone(),
                    ports: decl.ports.clone(),
                    stmts: decl.stmts.clone(),
                })));
            }
            // Package import are already resolved by resolver - discard it.
            Item::PkgImport(_) => (),
            Item::ParamDecl(decl) => {
                let kw = decl.kw;
                let ty = decl.ty.as_ref().map(|ty| self.eval_ty(ty));
                for item in &decl.list {
                    if let Some(v) = &item.init {
                        let (ty, val) = match ty {
                            None => self.eval_expr(v),
                            Some(ref ty) => (ty.clone(), self.eval_expr_assign(v, ty).1),
                        };
                        let declitem = HierItem::Param(Rc::new(hier::ParamDecl {
                            kw,
                            name: item.name.clone(),
                            ty,
                            init: val,
                        }));
                        self.add_to_scope(&item.name, declitem);
                    }
                }
            }
            Item::DataDecl(decl) => {
                if decl.has_const {
                    self.diag.report_error("const data declaration isn't yet supported", decl.ty.span);
                }
                let ty = self.eval_ty(&decl.ty);
                for item in &decl.list {
                    let mut var_ty = ty.clone();
                    for dim in item.dim.iter().rev() {
                        match &dim.value {
                            DimKind::Range(a, b) => {
                                let ub = self.eval_expr_i32(a);
                                let lb = self.eval_expr_i32(b);
                                var_ty = Ty::Array(Box::new(var_ty), ub, lb);
                            }
                            DimKind::Value(a) => {
                                let mut size = self.eval_expr_usize_positive(a) as i32;
                                var_ty = Ty::Array(Box::new(var_ty), 0, size - 1)
                            }
                            _ => unimplemented!(),
                        }
                    }
                    let init = item.init.as_ref().map(|expr| Box::new(self.type_check_assign(expr, &ty)));
                    let decl = Rc::new(hier::DataDecl {
                        lifetime: decl.lifetime,
                        ty: var_ty,
                        name: item.name.clone(),
                        init,
                    });
                    self.add_to_scope(&item.name, HierItem::DataDecl(decl));
                }
            }
            Item::Typedef(_, ty, name, dim) => {
                let ty = self.eval_ty(ty);
                let declitem = HierItem::Type(Rc::new(hier::TypedefDecl {
                    ty,
                    name: Ident::clone(&name),
                }));
                if dim.len() != 0 {
                    unimplemented!();
                }
                self.add_to_scope(&name, declitem);
            }
            Item::TypedefIntf(_, intf, ty, name) => {
                // First evaluate intf to get an hierachical item
                let item = match self.type_check_hier_id(&intf.value, intf.span).0 {
                    None => {
                        self.diag.report_fatal("this must be an interface port name", intf.span);
                    },
                    Some(item) => item,
                };
                // Check if this is an interface port
                let inst = match item {
                    HierItem::InterfacePort(ref decl) => &decl.inst,
                    _ => {
                        self.diag.report_fatal("this must be an interface port name", intf.span);
                    }
                };
                // Find ty inside the interface
                let inst_inst = inst.get_instance();
                let item = match inst_inst.scope.find(&ty) {
                    None => {
                        self.diag.report_fatal("cannot find this in interface port", name.span);
                    }
                    Some(v) => v,
                };
                // Check if the item found is a typedef.
                let ty = match item {
                    HierItem::Type(ref decl) => {
                        decl.ty.clone()
                    }
                    _ => {
                        self.diag.report_fatal("this is not a typedef in interface port", name.span);
                    }
                };
                // Convert this into a conventional typedef.
                let declitem = HierItem::Type(Rc::new(hier::TypedefDecl {
                    ty,
                    name: Ident::clone(&name),
                }));
                self.add_to_scope(&name, declitem);
            }
            // These are not handled specially
            Item::ContinuousAssign(list) => {
                for assign in list {
                    // TODO: Should get the type out
                    let assign = self.type_check_assign_todo(assign);
                    self.add_item(HierItem::ContinuousAssign(Rc::new(assign)));
                }
            }
            v @ Item::Initial(..) => {
                // Need to clone AST here.
                self.add_item(HierItem::Other(Rc::new(Item::clone(v))));
            }
            Item::Always(kw, stmt) => {
                let stmt = self.elaborate_stmt(stmt);
                self.add_item(HierItem::Always(*kw, Rc::new(stmt)));
            }
            Item::HierInstantiation(inst) => {
                self.elaborate_instantiation(inst);
            }
            // GenRegion(Vec<Item>),
            Item::LoopGen(gen) => {
                // Each generate construct will be assigned an id for external names.
                self.genblk += 1;
                let declitem = Rc::new(hier::LoopGenBlock {
                    name: gen.block.name.as_ref().map(|name| Ident::clone(name)),
                    instances: RefCell::new(Vec::new()),
                    id: self.genblk,
                });
                if let Some(name) = &gen.block.name {
                    self.add_to_scope(name, HierItem::LoopGenBlock(Rc::clone(&declitem)));
                } else {
                    self.add_item(HierItem::LoopGenBlock(Rc::clone(&declitem)));
                }

                // This scope is only for genvar
                self.scopes.push(HierScope::new());
                let genvar = Rc::new(hier::GenVar {
                    name: gen.id.clone(),
                    value: RefCell::new(0),
                });
                self.add_to_scope(&gen.id, HierItem::GenVar(Rc::clone(&genvar)));

                // Evaluate the initial value for the genvar
                let init = self.eval_expr_i32(&gen.init);
                if let HierItem::GenVar(genvar) = self.resolve(&gen.id) {
                    *genvar.value.borrow_mut() = init;
                } else {
                    unreachable!()
                }

                loop {
                    // Evaluate the condition
                    let (_, result) = self.eval_expr(&gen.cond);
                    let boolean = match result {
                        Val::Int(vec) => {
                            match vec.get_two_state() {
                                None => unimplemented!(),
                                Some(v) => !v.is_zero(),
                            }
                        }
                        Val::FixStr(str) => !str.is_empty(),
                        _ => unimplemented!(),
                    };

                    // Exit the loop if boolean value does not match
                    if !boolean {
                        break;
                    }

                    // Retrieve the current value of genvar
                    let val = if let HierItem::GenVar(genvar) = self.resolve(&gen.id) {
                        *genvar.value.borrow()
                    } else {
                        unreachable!()
                    };

                    // Elaborate items within the gen block
                    let genblk_saved = self.genblk;
                    self.genblk = 0;
                    self.scopes.push(HierScope::new());

                    // The genvar is automatically converted to a localparam within the block
                    let genvar_item = HierItem::Param(Rc::new(hier::ParamDecl {
                        kw: Keyword::Localparam,
                        name: gen.id.clone(),
                        ty: Ty::Int(IntTy::SimpleVec(32, false, true)),
                        init: Val::Int(LogicVec::from_integer(val)),
                    }));
                    self.add_to_scope(&gen.id, genvar_item);

                    for item in &gen.block.items {
                        self.elaborate_item(item);
                    }

                    let scope = self.scopes.pop().unwrap();
                    self.genblk = genblk_saved;
                    let genblk = Rc::new(hier::GenBlock {
                        name: None,
                        scope,
                        id: None,
                    });
                    declitem.instances.borrow_mut().push((val, genblk));

                    // Execute the update expression
                    self.eval_expr(&gen.update);
                }
                self.scopes.pop();
            }
            Item::IfGen(ifgen) => {
                self.genblk += 1;

                // First figure out which block to instantiate
                let block = 'if_outer: loop {
                    for (cond, block) in &ifgen.if_block {
                        let (_, result) = self.eval_expr(cond);
                        let boolean = match result {
                            Val::Int(vec) => {
                                match vec.get_two_state() {
                                    None => unimplemented!(),
                                    Some(v) => !v.is_zero(),
                                }
                            }
                            Val::FixStr(str) => !str.is_empty(),
                            _ => unimplemented!(),
                        };
                        if boolean {
                            break 'if_outer Some(block);
                        }
                    }

                    if let Some(block) = &ifgen.else_block {
                        break Some(block);
                    }

                    break 'if_outer None;
                };

                if let Some(block) = block {
                    let genblk_saved = self.genblk;
                    self.genblk = 0;
                    self.scopes.push(HierScope::new());
                    for item in &block.items {
                        self.elaborate_item(item);
                    }
                    let scope = self.scopes.pop().unwrap();
                    self.genblk = genblk_saved;
                    let decl = HierItem::GenBlock(Rc::new(hier::GenBlock {
                        name: block.name.as_ref().map(|name| Ident::clone(name)),
                        scope,
                        id: Some(genblk_saved),
                    }));
                    if let Some(v) = &block.name {
                        self.add_to_scope(v, decl);
                    } else {
                        self.add_item(decl);
                    }
                }
            }
            // GenBlock(Box<GenBlock>),
            Item::SysTfCall(call) => {
                // Evaluate all arguments
                let mut arguments = match &call.args {
                    None => Vec::new(),
                    Some(list) => list.ordered.iter().map(|expr| self.eval_expr(expr.as_ref().unwrap())).collect(),
                };
                let severity = match call.task.as_str() {
                    "$fatal" => {
                        // For fatal task, we don't care about finish number in elaboration.
                        if !arguments.is_empty() { arguments.remove(0); }
                        Severity::Error
                    }
                    "$error" => Severity::Error,
                    "$warning" => Severity::Warning,
                    "$info" => Severity::Info,
                    _ => unreachable!(),
                };
                // TODO: Arguments of display
                let formatted = if let Some((_, Val::FixStr(str))) = arguments.get(0) { str } else { "" };
                self.diag.report_span(severity, formatted, call.task.span);
            }
            Item::ModportDecl(_, list) => {
                for (name, decl) in list {
                    self.scopes.push(HierScope::new());
                    for port in decl {
                        match port {
                            ModportPortDecl::Simple(_, dir, list) => {
                                for v in list {
                                    match v {
                                        ModportSimplePort::Named(name) => {
                                            let item = Rc::new(hier::DataPortDecl {
                                                dir: *dir,
                                                net: NetPortType::Variable,
                                                name: name.clone(),
                                                ty: Ty::Void,
                                                init: None,
                                            });
                                            self.add_to_scope(name, HierItem::DataPort(item));
                                        }
                                        ModportSimplePort::Explicit(..) => unimplemented!(),
                                    }
                                }
                            }
                            ModportPortDecl::Clocking(..) => unimplemented!(),
                        }
                    }
                    let scope = self.scopes.pop().unwrap();
                    let modport = Rc::new(hier::Modport {
                        name: name.clone(),
                        scope: scope,
                    });
                    self.add_to_scope(name, HierItem::Modport(modport));
                }
            }
            v => {
                eprintln!("{:?}", v);
                unimplemented!();
            }
        }
    }

    pub fn elaborate(&mut self, items: &Vec<Vec<Item>>) {
        self.scopes.push(HierScope::new());

        // Walk through for first time to add designs into global symbol list
        for items in items {
            for item in items {
                if let Item::DesignDecl(_) = item {
                    self.elaborate_item(item);
                }
            }
        }

        let mut modules_list = self.scopes[0].items.clone();
        modules_list.reverse();

        for items in items {
            self.scopes.push(HierScope::new());
            for item in items {
                if let Item::DesignDecl(_) = item {
                    // This is processed already in the first iteration, so do not elaborate them
                    // again. But do add them to compilation-unit local item list.
                    self.scopes.last_mut().unwrap().items.push(modules_list.pop().unwrap());
                } else {
                    self.elaborate_item(&item);
                }
            }
            self.units.push(self.scopes.pop().unwrap());
        }

        // Find the top-level module
        let toplevel = match self.scopes[0].find(self.toplevel_name) {
            Some(HierItem::Design(item)) => item.clone(),
            _ => {
                self.diag.report_error(
                    format!("cannot find toplevel module {}", self.toplevel_name),
                    Span::none()
                );
                return;
            }
        };
        self.elaborate_toplevel(toplevel);
    }

    fn elaborate_stmt(&mut self, stmt: &ast::Stmt) -> expr::Stmt {
        let kind = match &stmt.value {
            ast::StmtKind::Empty => expr::StmtKind::Empty,
            ast::StmtKind::TimingCtrl(ctrl, stmt) => expr::StmtKind::TimingCtrl(
                ctrl.clone(),
                Box::new(self.elaborate_stmt(stmt))
            ),
            ast::StmtKind::If(uniq, cond, t, f) => {
                let cond = self.type_check_bool(cond);
                let t = self.elaborate_stmt(t);
                let f = f.as_ref().map(|f| self.elaborate_stmt(f));
                expr::StmtKind::If {
                    uniq: *uniq,
                    cond: Box::new(cond),
                    success: Box::new(t),
                    failure: f.map(Box::new),
                }
            },
            ast::StmtKind::Case { uniq, kw, expr, items } => {
                let expr = self.type_check(expr);
                let items = items.iter().map(|(conds, stmt)| {
                    let conds = conds.iter().map(|cond| self.type_check(cond)).collect();
                    let stmt = self.elaborate_stmt(stmt);
                    (conds, stmt)
                }).collect();
                expr::StmtKind::Case {
                    uniq: *uniq,
                    kw: *kw,
                    expr: Box::new(expr),
                    items,
                }
            },
            ast::StmtKind::For { ty, init, cond, update, body } => {
                let ty = ty.as_ref().map(|ty| self.eval_ty(ty));
                if let Some(ref ty) = ty {
                    // The scope for initialisers
                    self.scopes.push(HierScope::new());
                    for expr in init {
                        if let ExprKind::Assign(lhs, _) = &expr.value {
                            if let ExprKind::HierName(HierId::Name(None, name)) = &lhs.value {
                                self.add_to_scope(name, HierItem::DataDecl(Rc::new(hier::DataDecl {
                                    lifetime: ast::Lifetime::Automatic,
                                    ty: ty.clone(),
                                    name: Ident::clone(name),
                                    // We will process initialisers later, set it to none here.
                                    init: None,
                                })));
                            } else { unreachable!() }
                        } else { unreachable!(); }
                    }
                }
                // It's not assignment-like context here as each init is a whole assignment expression.
                // Its subexpression will be treated like assignment-like context.
                let init = init.iter().map(|expr| self.type_check(expr)).collect();
                let cond = cond.as_ref().map(|expr| Box::new(self.type_check_bool(expr)));
                let update = update.iter().map(|expr| self.type_check(expr)).collect();
                let body = Box::new(self.elaborate_stmt(body));
                if ty.is_some() {
                    self.scopes.pop();
                }
                expr::StmtKind::For {
                    ty: ty.map(Box::new),
                    init, cond, update, body
                }
            },
            ast::StmtKind::Assert { kind, expr, success, failure } => {
                let expr = Box::new(self.type_check_bool(expr));
                let success = success.as_ref().map(|stmt| Box::new(self.elaborate_stmt(stmt)));
                let failure = failure.as_ref().map(|stmt| Box::new(self.elaborate_stmt(stmt)));
                expr::StmtKind::Assert {
                    kind: *kind,
                    expr,
                    success,
                    failure,
                }
            },
            ast::StmtKind::SeqBlock(list) => {
                self.scopes.push(HierScope::new());
                let list = list.iter().map(|stmt| self.elaborate_stmt(stmt)).collect();
                self.scopes.pop();
                expr::StmtKind::SeqBlock(list)
            }
            ast::StmtKind::Expr(expr) => {
                let expr = self.type_check(expr);
                expr::StmtKind::Expr(Box::new(expr))
            }
            ast::StmtKind::DataDecl(_decl) => {
                unimplemented!()
            }
        };
        expr::Stmt {
            label: stmt.label.clone(),
            value: kind,
        }
    }

    //
    // The following section handles data type folding
    //

    fn eval_packed_dim(&mut self, mut ty: IntTy, dim: &Vec<Dim>) -> IntTy {
        for dim in dim.iter().rev() {
            match &dim.value {
                DimKind::Range(a, b) => {
                    let ub = self.eval_expr_i32(a);
                    let lb = self.eval_expr_i32(b);
                    ty = ty.vec(ub, lb);
                }
                _ => self.diag.report_fatal("unexpected dimension format", dim.span),
            }
        }
        ty
    }

    pub fn eval_ty(&mut self, ty: &DataType) -> Ty {
        match &ty.value {
            DataTypeKind::Type => Ty::Type,
            DataTypeKind::Implicit(signing, dim) => {
                Ty::Int(self.eval_packed_dim(
                    IntTy::Logic(false, signing == &Signing::Signed),
                    dim
                ))
            }
            DataTypeKind::IntVec(vecty, signing, dim) => {
                Ty::Int(self.eval_packed_dim(
                    IntTy::Logic(vecty == &IntVecTy::Bit, signing == &Signing::Signed),
                    dim
                ))
            }
            DataTypeKind::IntAtom(ty, explicit_sign) => {
                let (width, two_state, mut signed) = match ty {
                    IntAtomTy::Shortint => (16, true, true),
                    IntAtomTy::Int => (32, true, true),
                    IntAtomTy::Longint => (64, true, true),
                    IntAtomTy::Byte => (8, true, true),
                    IntAtomTy::Integer => (32, false, true),
                    IntAtomTy::Time => (64, false, false),
                };
                if let Some(v) = explicit_sign {
                    signed = v == &Signing::Signed;
                }
                Ty::Int(IntTy::SimpleVec(width, two_state, signed))
            }
            DataTypeKind::Real(subty) => Ty::Real(*subty),
            DataTypeKind::Aggr(aggr, dim) => {
                if aggr.kind != AggrType::Struct || !aggr.packed {
                    self.diag.report_fatal(
                        "currently only supported aggregate is packed structure",
                        ty.span
                    );
                }
                let mut list = Vec::new();
                for member in &aggr.members {
                    let ty = if let Ty::Int(ty) = self.eval_ty(&member.ty) {
                        ty
                    } else {
                        self.diag.report_error(
                            "only integral types can be used in packed structure",
                            member.ty.span
                        );
                        // Error recovery: ignore this field
                        continue;
                    };
                    let ty_ty = Ty::Int(ty.clone());
                    for assign in &member.list {
                        let init = assign.init.as_ref().map(|expr| Box::new({
                            let (_, val) = self.eval_expr_assign(expr, &ty_ty);
                            match val {
                                Val::Int(val) => val,
                                _ => unreachable!(),
                            }
                        }));
                        list.push((ty.clone(), assign.name.clone(), init));
                    }
                }

                let struc = Rc::new(Struct::new(aggr.sign == Signing::Signed, list));
                self.structs.push(Rc::clone(&struc));
                Ty::Int(self.eval_packed_dim(
                    IntTy::Struct(struc),
                    dim
                ))
            }
            DataTypeKind::Enum(decl, dim) => {
                // Process base type. Default to int.
                let base = decl.ty.as_ref().and_then(|v| {
                    match self.eval_ty(v) {
                        Ty::Int(ty) => Some(ty),
                        _ => {
                            self.diag.report_error(
                                "the base type of enumeration must be integral type",
                                v.span
                            );
                            None
                        }
                    }
                }).unwrap_or_else(|| IntTy::SimpleVec(32, true, true));
                let base_ty = Ty::Int(base.clone());

                // Set next_value to all one so that the next generated element will be assigned with 0.
                let mut next_value = LogicVec::from_int(base.sign(), Int::all_one(base.width()));

                let enu = Rc::new(ty::Enum {
                    base: base.clone(),
                    elements: RefCell::new(Vec::new()),
                });
                self.enums.push(Rc::clone(&enu));

                // Process all members
                for assign in &decl.members {
                    if let Some(init) = &assign.init {
                        next_value = match self.eval_expr_assign(init, &base_ty).1 {
                            Val::Int(val) => val,
                            _ => unreachable!(),
                        };
                    } else {
                        next_value += 1;
                        if !next_value.is_two_state() {
                            self.diag.report_error(
                                "cannot assign value automatically because previous value is 'x. Consider give it an explicit initializer",
                                assign.name.span
                            );
                            // Error recovery - make the vector all zero.
                            next_value = LogicVec::fill(
                                next_value.width(),
                                next_value.signed,
                                LogicValue::Zero
                            );
                        }
                    };
                    let mut elements = enu.elements.borrow_mut();
                    self.add_to_scope(&assign.name, HierItem::Enum(Rc::clone(&enu), elements.len()));
                    elements.push((assign.name.clone(), next_value.clone()));
                }

                Ty::Int(self.eval_packed_dim(IntTy::Enum(enu), dim))
            }
            DataTypeKind::String => Ty::String,
            DataTypeKind::Chandle => Ty::Chandle,
            DataTypeKind::VirtualInterface => unimplemented!(), // TODO
            DataTypeKind::Event => Ty::Event,
            DataTypeKind::HierName(scope, name, dim) => {
                let item = match scope {
                    Some(Scope::Name(None, pkg)) => {
                        // Packaged name, retrieve from package
                        self.pkgs[&pkg.value].scope.find(&name).unwrap().clone()
                    }
                    None => {
                        // Lexical name
                        self.resolve(name)
                    }
                    _ => {
                        self.diag.report_fatal(
                            "specified scope isn't yet supported",
                            ty.span
                        );
                    }
                };

                let ty = match item {
                    HierItem::Param(ref decl) if Ty::Type == decl.ty => {
                        if let Val::Type(ty) = &decl.init { ty.clone() } else { unreachable!() }
                    }
                    HierItem::Type(ref decl) => decl.ty.clone(),
                    _ => {
                        self.diag.report_fatal(format!("{} is not a type", name), name.span)
                    }
                };

                if let Ty::Int(intty) = ty {
                    // Packed dimension
                    Ty::Int(self.eval_packed_dim(intty, dim))
                } else {
                    assert!(dim.len() == 0);
                    ty
                }
            }
            DataTypeKind::TypeRef(expr) => {
                // First infer the type of this expression (not necessary constant expression,
                // but should already have type resolved).
                let conv = self.type_check(&expr);
                match conv.ty {
                    // If it is a type, then we will evaluate it and use the type.
                    Ty::Type => {
                        if let Val::Type(ty) = self.eval_checked_expr(&conv) {
                            ty
                        } else {
                            unreachable!();
                        }
                    }
                    // Otherwise use its type
                    ty => ty,
                }
            }
            DataTypeKind::Void => Ty::Void,
        }
    }

    //
    // The following section handles expression type check, evaulation and folding
    //

    pub fn type_of_hier(hier: &HierItem) -> Ty {
        match hier {
            HierItem::Param(decl) => decl.ty.clone(),
            HierItem::Type(_) => Ty::Type,
            HierItem::DataPort(decl) => decl.ty.clone(),
            HierItem::DataDecl(decl) => decl.ty.clone(),
            HierItem::FuncDecl(_) => unimplemented!(),
            HierItem::InterfacePort(_) => Ty::Void, // Not typable
            HierItem::Design(_) => unimplemented!(), // Not typable
            HierItem::ContinuousAssign(_) => unreachable!(),
            HierItem::Always(..) => unreachable!(),
            HierItem::Other(_) => unreachable!(),
            HierItem::Instance(_) => Ty::Void, // Not typable
            HierItem::InstancePart{ .. } => Ty::Void, // Not typable
            HierItem::GenBlock(_) => Ty::Void, // Not typable
            HierItem::GenVar(_) => Ty::Int(IntTy::SimpleVec(32, false, true)),
            HierItem::LoopGenBlock(_) => Ty::Void, // Not typable
            HierItem::Modport(_) => Ty::Void, // Not typable
            HierItem::Enum(enu, _) => Ty::Int(IntTy::Enum(Rc::clone(enu))),
        }
    }

    /// Try to parse the name as an hierachical item. If it happens to also contain member/index
    /// access, parse it as expression instead.
    pub fn type_check_hier_id(
        &mut self, name: &HierId, span: Span
    ) -> (Option<HierItem>, expr::Expr) {
        match name {
            HierId::Name(scope, name) => {
                // Simple identifier name. Try to lookup it up.
                let hier = match scope {
                    Some(Scope::Name(None, pkg)) => {
                        // Packaged name, retrieve from package
                        self.pkgs[&pkg.value].scope.find(&name).unwrap().clone()
                    }
                    None => {
                        // Lexical name
                        self.resolve(name)
                    }
                    _ => {
                        self.diag.report_fatal(
                            "specified scope isn't yet supported",
                            span
                        );
                    }
                };
                let expr = expr::Expr {
                    value: expr::ExprKind::HierName(HierId::Name(scope.clone(), name.clone())),
                    ty: Self::type_of_hier(&hier),
                    span,
                };
                (Some(hier), expr)
            }
            HierId::Member(parent, name) => self.type_check_member(parent, name, span),
            HierId::Select(parent, dim) => self.type_check_select(parent, dim, span),
            v => unimplemented!("{:?}", v),
        }
    }

    /// Type-check a hierachical select or member select expression
    pub fn type_check_member(
        &mut self, parent: &Spanned<HierId>, name: &Ident, span: Span
    ) -> (Option<HierItem>, expr::Expr) {
        let (parent_hier, parent_expr) = self.type_check_hier_id(&parent.value, parent.span);
        // In `type_of_hier` we set type of non-expression to void. If the type is not
        // void, then this is an index expression.
        if let Ty::Void = parent_expr.ty {
            let item = match parent_hier {
                // This is an expression that has void type
                None => {
                    self.diag.report_fatal("cannot index into void expression", parent_expr.span)
                },
                Some(item) => item,
            };
            let hier = match item {
                HierItem::InterfacePort(decl) => {
                    if !decl.dim.is_empty() {
                        self.diag.report_fatal(
                            "this is an interface port array, not an interface",
                            parent.span
                        )
                    }
                    let item = match decl.inst.get_instance().scope.find(&name) {
                        None => self.diag.report_fatal(
                            format!("cannot find {} in interface", name),
                            name.span
                        ),
                        Some(v) => v.clone(),
                    };
                    item
                }
                HierItem::Instance(decl) => {
                    if !decl.dim.is_empty() {
                        self.diag.report_fatal(
                            "this is an interface port array, not an interface",
                            parent.span
                        )
                    }
                    let item = match decl.inst.get_instance().scope.find(&name) {
                        None => self.diag.report_fatal(
                            format!("cannot find {} in interface", name),
                            name.span
                        ),
                        Some(v) => v.clone(),
                    };
                    item
                }
                HierItem::InstancePart { inst, dim, ..} => {
                    if !dim.is_empty() {
                        self.diag.report_fatal(
                            "this is an instance array, not an instance",
                            parent.span
                        )
                    }
                    let item = match inst.get_instance().scope.find(&name) {
                        None => self.diag.report_fatal(
                            format!("cannot find {} in interface", name),
                            name.span
                        ),
                        Some(v) => v.clone(),
                    };
                    item
                }
                HierItem::GenBlock(decl) => {
                    let item = match decl.scope.find(&name) {
                        None => self.diag.report_fatal(
                            format!("cannot find {} in generate block", name),
                            name.span
                        ),
                        Some(v) => v.clone(),
                    };
                    item
                }
                _ => unimplemented!("{:?} {:?}", parent, name),
            };
            let expr = if let expr::ExprKind::HierName(id) = parent_expr.value {
                expr::Expr {
                    value: expr::ExprKind::HierName(
                        HierId::Member(Box::new(Spanned::new(id, parent_expr.span)), Box::new(name.clone()))
                    ),
                    ty: Self::type_of_hier(&hier),
                    span,
                }
            } else { unreachable!() };
            return (Some(hier), expr)
        }

        let myty = match parent_expr.ty {
            Ty::Int(IntTy::Struct(ref struc)) => 'struct_loop: loop {
                for (ty, member_name, _) in &struc.members {
                    if name == member_name {
                        break 'struct_loop Ty::Int(ty.clone());
                    }
                }
                self.diag.report_fatal(format!("there are no members named {}", name), span);
            }
            _ => {
                eprintln!("{:?}", parent_expr.ty);
                self.diag.report_fatal("unexpected member select", span);
            }
        };
        (None, expr::Expr {
            value: expr::ExprKind::Member(Box::new(parent_expr), name.clone()),
            ty: myty,
            span,
        })
    }

    /// Type-check a hierachical select or bit/part-select expression.
    pub fn type_check_select(
        &mut self, parent: &Spanned<HierId>, dim: &Dim, span: Span
    ) -> (Option<HierItem>, expr::Expr) {
        let (parent_hier, parent_expr) = self.type_check_hier_id(&parent.value, parent.span);
        // In `type_of_hier` we set type of non-expression to void. If the type is not
        // void, then this is an index expression.
        if let Ty::Void = parent_expr.ty {
            let item = match parent_hier {
                // This is an expression that has void type
                None => {
                    self.diag.report_fatal("cannot index into void expression", parent_expr.span)
                },
                Some(item) => item,
            };
            // Only constant bit select is valid for instance array
            let value = match dim.value {
                DimKind::Value(ref value) => value,
                _ => {
                    self.diag.report_fatal(
                        "only constant bit select is valid in this context",
                        dim.span
                    );
                }
            };
            let value = self.eval_expr_i32(value);
            let hier = match item {
                HierItem::Instance(ref inst) => {
                    match inst.dim.first() {
                        None => {
                            self.diag.report_error("this is not an instance array", parent.span);
                            // Error-recovery: return current instance
                            item.clone()
                        }
                        Some(range) => {
                            if value < cmp::min(range.0, range.1) || value > cmp::max(range.0, range.1) {
                                self.diag.report_fatal(
                                    "constant bit select outside range",
                                    dim.span
                                );
                            }
                            HierItem::InstancePart {
                                inst: inst.inst.clone(),
                                modport: None,
                                dim: inst.dim.iter().skip(1).map(Clone::clone).collect(),
                            }
                        }
                    }
                }
                HierItem::InterfacePort(ref decl) => {
                    match decl.dim.first() {
                        None => {
                            self.diag.report_error("this is not an interface port array", parent.span);
                            // Error-recovery: return current instance
                            item.clone()
                        }
                        Some(range) => {
                            if value < cmp::min(range.0, range.1) || value > cmp::max(range.0, range.1) {
                                self.diag.report_fatal(
                                    "constant bit select outside range",
                                    dim.span
                                );
                            }
                            HierItem::InstancePart {
                                inst: decl.inst.clone(),
                                modport: decl.modport.clone(),
                                dim: decl.dim.iter().skip(1).map(Clone::clone).collect(),
                            }
                        }
                    }
                }
                HierItem::LoopGenBlock(ref decl) => {
                    let genblk = match decl.instances.borrow().iter().find(|(num, _)| num == &value) {
                        None => {
                            self.diag.report_fatal(
                                "constant bit select outside range",
                                dim.span
                            );
                        },
                        Some((_, genblk)) => Rc::clone(genblk),
                    };
                    HierItem::GenBlock(genblk)
                }
                ref v => unimplemented!("{:?}", std::mem::discriminant(v)),
            };
            let expr = if let expr::ExprKind::HierName(id) = parent_expr.value {
                expr::Expr {
                    value: expr::ExprKind::HierName(
                        HierId::Select(Box::new(Spanned::new(id, parent_expr.span)), Box::new(Spanned::new(DimKind::Value(Box::new({
                            let mut expr = super::reconstruct::reconstruct_i32(value);
                            expr.span = dim.span;
                            expr
                        })), dim.span)))
                    ),
                    ty: Self::type_of_hier(&hier),
                    span,
                }
            } else { unreachable!() };
            return (Some(hier), expr)
        };

        // The canonical element type of this int type.
        let canonical_element_type = match parent_expr.ty {
            Ty::Int(ref intty) => match intty {
                // For array type, 
                IntTy::Array(ty, ..) => Ty::Int(IntTy::clone(ty)),
                _ => Ty::Int(IntTy::Logic(intty.two_state(), false)),
            }
            Ty::Array(ref ty, ..) => Ty::clone(ty),
            _ => unimplemented!(),
        };
        let (dim, len) = match dim.value {
            DimKind::Value(ref value) => {
                let expr = self.type_check_int(value);
                let dim = Spanned::new(expr::DimKind::Value(Box::new(expr)), dim.span);
                return (None, expr::Expr {
                    value: expr::ExprKind::Select(Box::new(parent_expr), dim),
                    ty: canonical_element_type,
                    span,
                })
            }
            DimKind::Range(ref ub, ref lb) => {
                let ub = self.eval_expr_i32(ub);
                let lb = self.eval_expr_i32(lb);
                let size = (cmp::max(ub, lb) - cmp::min(ub, lb)) as usize + 1;
                (Spanned::new(expr::DimKind::Range(ub, lb), dim.span), size)
            }
            DimKind::PlusRange(ref value, ref width) => {
                let expr = self.type_check_int(value);
                let w = self.eval_expr_usize_positive(width);
                (Spanned::new(expr::DimKind::PlusRange(Box::new(expr), w), dim.span), w)
            }
            DimKind::MinusRange(ref value, ref width) => {
                let expr = self.type_check_int(value);
                let w = self.eval_expr_usize_positive(width);
                (Spanned::new(expr::DimKind::MinusRange(Box::new(expr), w), dim.span), w)
            }
            _ => {
                self.diag.report_fatal(
                    "unimplemented dimension kind",
                    dim.span
                );
            }
        };
        // For part-select canonical element type must be IntTy
        let canonical_element_type = match canonical_element_type {
            Ty::Int(intty) => intty,
            _ => unimplemented!(),
        };
        (None, expr::Expr {
            value: expr::ExprKind::Select(Box::new(parent_expr), dim),
            ty: Ty::Int(canonical_element_type.vec(len as i32 - 1, 0)),
            span,
        })
    }

    fn type_check_assign_pattern(&mut self, _ty: &Ty, pattern: &ast::AssignPattern) -> expr::AssignPattern {
        let pattern = match pattern {
            ast::AssignPattern::Simple(list) => {
                // TODO: They should be considered as assignment-like instead
                expr::AssignPattern::Simple(
                    list.iter().map(|item| self.type_check_assign_todo(item)).collect()
                )
            }
            _ => unimplemented!(),
        };
        // TODO: Also evaluate within assignment pattern
        pattern
    }

    /// Perform self-determined type checks and convert expression into an post-elaboration
    /// expression.
    pub fn self_type_check(&mut self, expr: &Expr) -> expr::Expr {
        match expr.value {
            ExprKind::Type(ref ty) => {
                let ty = self.eval_ty(ty);
                expr::Expr {
                    value: expr::ExprKind::Const(Val::Type(ty)),
                    span: expr.span,
                    ty: Ty::Type,
                }
            }
            ExprKind::Literal(ref token) => {
                match token.value {
                    TokenKind::StringLiteral(ref str) => expr::Expr {
                        value: expr::ExprKind::Const(Val::FixStr(str.clone())),
                        span: expr.span,
                        ty: Ty::FixStr(str.len())
                    },
                    TokenKind::TimeLiteral(_) => unimplemented!(),
                    TokenKind::RealLiteral(val) => expr::Expr {
                        value: expr::ExprKind::Const(Val::Real(val)),
                        span: expr.span,
                        ty: Ty::Real(RealTy::Real)
                    },
                    TokenKind::IntegerLiteral(ref num) => expr::Expr {
                        value: expr::ExprKind::Const(Val::Int(num.value.clone())),
                        span: expr.span,
                        ty: Ty::Int(IntTy::SimpleVec(
                            if num.sized { num.value.width() } else { 32 },
                            false,
                            num.value.signed()
                        )),
                    },
                    TokenKind::UnbasedLiteral(val) => {
                        let mut num: LogicVec = val.into();
                        // Note that in this case type is unsigned while value is signed.
                        // We delibrately make this the case so that we can apply sign extension
                        // in propagate_size. This inconsistency should be fixed after sign
                        // extensions.
                        num.signed = true;
                        expr::Expr {
                            value: expr::ExprKind::Const(Val::Int(num)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            ExprKind::HierName(ref name) => {
                let (hier, expr) = self.type_check_hier_id(name, expr.span);
                match hier {
                    Some(hier) => {
                        // Fold them into constant right away
                        let value = match hier {
                            HierItem::Param(ref decl) => expr::ExprKind::Const(decl.init.clone()),
                            HierItem::Type(ref decl) => expr::ExprKind::Const(Val::Type(decl.ty.clone())),
                            HierItem::Enum(ref enu, index) =>
                                expr::ExprKind::Const(Val::Int(enu.elements.borrow()[index].1.clone())),
                            _ => expr.value,
                        };
                        expr::Expr {
                            value,
                            ty: expr.ty,
                            span: expr.span,
                        }
                    }
                    None => expr,
                }
            }
            // HierName(Option<Box<Scope>>, HierId)
            // EmptyQueue,
            ExprKind::Concat(ref subexpr, ref select) => {
                // Type check each subexpressions. They all need to be integral
                let subexpr: Vec<_> = subexpr
                    .iter()
                    .map(|expr| self.type_check_int(expr))
                    .collect();
                // Compute the overall width and two_state-ness.
                let (width, two_state) = subexpr
                    .iter()
                    .fold((0, true), |(width, two_state), expr| {
                        if let Ty::Int(ref val) = expr.ty {
                            (width + val.width(), two_state & val.two_state())
                        } else { unreachable!() }
                    });
                assert!(select.is_none()); // TODO
                expr::Expr {
                    value: expr::ExprKind::Concat(subexpr),
                    span: expr.span,
                    ty: Ty::Int(IntTy::SimpleVec(width, two_state, false))
                }
            }
            ExprKind::MultConcat(ref mul, ref subexpr, ref select) => {
                // Multiplier must be an integer
                // TODO: Not necessary positive if within concat, can be zero
                let mul_val = self.eval_expr_usize_positive(mul);
                let subexpr = self.type_check_int(subexpr);
                let ty = if let Ty::Int(ref val) = subexpr.ty {
                    IntTy::SimpleVec(val.width() * mul_val, val.two_state(), false)
                } else { unreachable!() };
                assert!(select.is_none()); // TODO
                expr::Expr {
                    value: expr::ExprKind::MultConcat(mul_val, Box::new(subexpr)),
                    span: expr.span,
                    ty: Ty::Int(ty),
                }
            }
            ExprKind::AssignPattern(None, _) => {
                // this cannot appear in self-determined context. It must be within an assignment
                // context.
                self.diag.report_fatal(
                    "untyped assignment pattern can only appear in assignment-like context",
                    expr.span
                );
            },
            ExprKind::AssignPattern(Some(ref ty), ref pattern) => {
                let ty = self.eval_ty(ty);
                let pattern = self.type_check_assign_pattern(&ty, pattern);
                expr::Expr {
                    value: expr::ExprKind::AssignPattern(Box::new(ty.clone()), pattern),
                    span: expr.span,
                    ty: ty,
                }
            }
            ExprKind::SysTfCall(_) => {
                // Work around borrow checker
                let call = if let ExprKind::SysTfCall(call) = &expr.value {call} else { unreachable!() };
                // Currently no system task calls should have named arguments
                if let Some(args) = &call.args {
                    if !args.named.is_empty() {
                        self.diag.report_error("system task calls should not contain named arguments", call.task.span);
                    }
                }
                match call.task.as_str() {
                    "$clog2" => {
                        let args = if let Some(args) = &call.args {
                            args.ordered.iter().map(|v| v.as_ref().map(|v| self.type_check(v))).collect()
                        } else {
                            unimplemented!()
                        };
                        expr::Expr {
                            value: expr::ExprKind::SysTfCall(Box::new(call.task.clone()), args),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(32, false, true)),
                        }
                    }
                    "$bits" => {
                        let arg_checked = if let Some(args) = &call.args {
                            args.ordered.len() == 1
                        } else {
                            false
                        };
                        if !arg_checked {
                            self.diag.report_fatal("$bits must have exactly 1 arguments", call.task.span);
                        }
                        let arg = call.args.as_ref().unwrap().ordered[0].as_ref().unwrap();
                        let conv = self.type_check(arg);
                        let ty = match conv.ty {
                            // If it is a type, then we will evaluate it and use the type.
                            Ty::Type => {
                                if let Val::Type(ty) = self.eval_checked_expr(&conv) {
                                    ty
                                } else {
                                    unreachable!();
                                }
                            }
                            // Otherwise use its type
                            ty => ty,
                        };
                        let sz = match ty {
                            Ty::Int(ty) => ty.width() as i32,
                            _ => unimplemented!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Const(Val::Int(LogicVec::from(32, true, sz.into()))),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(32, false, true)),
                        }
                    }
                    _ => {
                        eprintln!("{:?} unimplemented", call.task);
                        let args = if let Some(args) = &call.args {
                            args.ordered.iter().map(|v| v.as_ref().map(|v| self.type_check(v))).collect()
                        } else {
                            unimplemented!()
                        };
                        expr::Expr {
                            value: expr::ExprKind::SysTfCall(Box::new(call.task.clone()), args),
                            span: expr.span,
                            ty: Ty::Void,
                        }
                    }
                }
            }
            ExprKind::FuncCall { ref expr, ref args, .. } => {
                let ty = if let ast::ExprKind::HierName(HierId::Name(None, ref name)) = expr.value {
                    if let HierItem::FuncDecl(decl) = self.resolve(name) {
                        decl.ty.clone()
                    } else { unimplemented!() }
                } else { unimplemented!() };
                expr::Expr {
                    value: expr::ExprKind::FuncCall { expr: expr.clone(), args: args.clone() },
                    span: expr.span,
                    ty,
                }
            }
            // ConstCast(Box<Expr>),
            ExprKind::SignCast(sign, ref inside) => {
                let inside = self.type_check_int(inside);
                let sign = sign == Signing::Signed;
                let (width, two_state) = match inside.ty {
                    Ty::Int(ref intty) => (intty.width(), intty.two_state()),
                    _ => unreachable!(),
                };
                expr::Expr {
                    value: expr::ExprKind::SignCast(sign, Box::new(inside)),
                    span: expr.span,
                    ty: Ty::Int(IntTy::SimpleVec(width, two_state, sign)),
                }
            }
            ExprKind::TypeCast(ref ty, ref inside) => {
                // The type of type cast must be a constant expression. We also need to know the
                // actual type of the expression in order to cast properly.
                let (_, ty_eval) = self.eval_expr(ty);
                let inside = self.type_check(inside);
                match ty_eval {
                    // If it is evaluated to a type, then this is a type cast.
                    Val::Type(ty) => expr::Expr {
                        value: expr::ExprKind::TypeCast(Box::new(ty.clone()), Box::new(inside)),
                        span: expr.span,
                        ty: ty,
                    },
                    // If it is evaluated to an integer, then do a bitwidth cast while preserving
                    // signedness. According to the spec the expression inside must also be
                    // integral.
                    Val::Int(v) => {
                        let size = match v.get_two_state() {
                            Some(size) => size,
                            None => {
                                self.diag.report_fatal("type specifier of cast should be two state", ty.span);
                            }
                        };
                        let size = match size.to_usize() {
                            Some(0) |
                            None => {
                                self.diag.report_fatal("type specifier of cast should be positive", ty.span);
                            }
                            Some(size) => size,
                        };
                        let (two_state, sign) = match inside.ty {
                            Ty::Int(ref intty) => (intty.two_state(), intty.sign()),
                            _ => {
                                self.diag.report_fatal("expression inside width cast must be an integral value", ty.span);
                            }
                        };
                        expr::Expr {
                            value: expr::ExprKind::WidthCast(size, Box::new(inside)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(size, two_state, sign)),
                        }
                    }
                    _ => {
                        self.diag.report_fatal("type specifier of cast should either be a type or an integral value", ty.span);
                    }
                }
            }
            ExprKind::Unary(op, _, ref rhs) => {
                match op {
                    UnaryOp::Add |
                    UnaryOp::Sub => {
                        let conv = self.self_type_check_num(rhs);
                        let myty = match conv.ty {
                            Ty::Int(ref subty) => Ty::Int(IntTy::SimpleVec(subty.width(), false, subty.sign())),
                            Ty::Real(ref subty) => Ty::Real(*subty),
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Unary(op, Box::new(conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    UnaryOp::Not => {
                        let conv = self.self_type_check_int(rhs);
                        let myty = match conv.ty {
                            Ty::Int(ref subty) => Ty::Int(IntTy::SimpleVec(subty.width(), false, subty.sign())),
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Unary(op, Box::new(conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    UnaryOp::LNot => {
                        let conv = self.type_check_bool(rhs);
                        expr::Expr {
                            value: expr::ExprKind::Unary(op, Box::new(conv)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                    UnaryOp::And |
                    UnaryOp::Nand |
                    UnaryOp::Or |
                    UnaryOp::Nor |
                    UnaryOp::Xor |
                    UnaryOp::Xnor => {
                        let conv = self.self_type_check_int(rhs);
                        expr::Expr {
                            value: expr::ExprKind::Unary(op, Box::new(conv)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                }
            }
            ExprKind::Binary(ref lhs, op, _, ref rhs) => {
                match op {
                    BinaryOp::Add |
                    BinaryOp::Sub |
                    BinaryOp::Mul |
                    BinaryOp::Div |
                    BinaryOp::Mod => {
                        let (lhs_conv, rhs_conv) = self.self_type_check_num_2(lhs, rhs);
                        let myty = match (&lhs_conv.ty, &rhs_conv.ty) {
                            (Ty::Int(lsubty), Ty::Int(rsubty)) => {
                                Ty::Int(IntTy::SimpleVec(cmp::max(lsubty.width(), rsubty.width()), false, lsubty.sign() && rsubty.sign()))
                            }
                            (Ty::Real(subty), Ty::Real(_)) => Ty::Real(*subty),
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    BinaryOp::Power => {
                        let (lhs_conv, mut rhs_conv) = self.self_type_check_num_2(lhs, rhs);
                        let ctx = match &rhs_conv.ty {
                            Ty::Int(subty) => Some((subty.sign(), subty.width())),
                            _ => None,
                        };
                        // Right hand side is self-determined
                        if let Some(ctx) = ctx {
                            self.propagate_size(&mut rhs_conv, ctx);
                        }
                        let myty = match &lhs_conv.ty {
                            Ty::Int(subty) => Ty::Int(IntTy::SimpleVec(subty.width(), false, subty.sign())),
                            Ty::Real(subty) => Ty::Real(*subty),
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    BinaryOp::And |
                    BinaryOp::Or |
                    BinaryOp::Xor |
                    BinaryOp::Xnor => {
                        let lhs_conv = self.self_type_check_int(lhs);
                        let rhs_conv = self.self_type_check_int(rhs);
                        let myty = match (&lhs_conv.ty, &rhs_conv.ty) {
                            (Ty::Int(lsubty), Ty::Int(rsubty)) => {
                                Ty::Int(IntTy::SimpleVec(cmp::max(lsubty.width(), rsubty.width()), false, lsubty.sign() && rsubty.sign()))
                            }
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    BinaryOp::Shl |
                    BinaryOp::LShr |
                    BinaryOp::AShr => {
                        let lhs_conv = self.self_type_check_int(lhs);
                        let mut rhs_conv = self.type_check_int(rhs);

                        // Make rhs_conv always unsigned
                        let rhs_ctx = match &rhs_conv.ty {
                            Ty::Int(subty) => (false, subty.width()),
                            _ => unreachable!(),
                        };
                        self.insert_cast(&mut rhs_conv, rhs_ctx);

                        let myty = match &lhs_conv.ty {
                            Ty::Int(subty) => Ty::Int(subty.clone()),
                            _ => unreachable!(),
                        };
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: myty,
                        }
                    }
                    BinaryOp::Eq |
                    BinaryOp::Neq => {
                        let mut lhs_conv = self.self_type_check(lhs);
                        let mut rhs_conv = self.self_type_check(rhs);
                        let ctx = match (&lhs_conv.ty, &rhs_conv.ty) {
                            (Ty::Int(lsubty), Ty::Int(rsubty)) =>
                                (lsubty.sign() && rsubty.sign(), cmp::max(lsubty.width(), rsubty.width())),
                            _ => unimplemented!(),
                        };
                        self.propagate_size(&mut lhs_conv, ctx);
                        self.propagate_size(&mut rhs_conv, ctx);
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                    BinaryOp::CaseEq |
                    BinaryOp::CaseNeq |
                    BinaryOp::WildEq |
                    BinaryOp::WildNeq => unimplemented!(),
                    BinaryOp::LAnd |
                    BinaryOp::LOr |
                    BinaryOp::Imply |
                    BinaryOp::Equiv => {
                        let lhs_conv = self.type_check_bool(lhs);
                        let rhs_conv = self.type_check_bool(rhs);
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                    BinaryOp::Lt |
                    BinaryOp::Leq |
                    BinaryOp::Gt |
                    BinaryOp::Geq => {
                        let (mut lhs_conv, mut rhs_conv) = self.self_type_check_num_2(lhs, rhs);
                        let ctx = match (&lhs_conv.ty, &rhs_conv.ty) {
                            (Ty::Int(lsubty), Ty::Int(rsubty)) => {
                                Some((lsubty.sign() && rsubty.sign(), cmp::max(lsubty.width(), rsubty.width())))
                            }
                            _ => None,
                        };
                        // If both side are integral, we need to have size propagated now
                        if let Some(ctx) = ctx {
                            self.propagate_size(&mut lhs_conv, ctx);
                            self.propagate_size(&mut rhs_conv, ctx);
                        }
                        expr::Expr {
                            value: expr::ExprKind::Binary(Box::new(lhs_conv), op, Box::new(rhs_conv)),
                            span: expr.span,
                            ty: Ty::Int(IntTy::SimpleVec(1, false, false)),
                        }
                    }
                }
            }
            // PrefixIncDec(IncDec, Option<Box<AttrInst>>, Box<Expr>),
            ExprKind::PostfixIncDec(ref lhs, _, incdec) => {
                // TODO: Maybe we want to check lvalue here?
                let lhs = self.type_check_int(lhs);
                let ty = lhs.ty.clone();
                expr::Expr {
                    value: expr::ExprKind::PostfixIncDec(Box::new(lhs), incdec),
                    span: expr.span,
                    ty: ty,
                }
            }
            ExprKind::Assign(ref lhs, ref rhs) => {
                let lhs = self.type_check(lhs);
                let rhs = self.type_check_assign(rhs, &lhs.ty);
                expr::Expr {
                    value: expr::ExprKind::Assign(Box::new(lhs), Box::new(rhs)),
                    span: expr.span,
                    ty: Ty::Void,
                }
            }
            ExprKind::NonblockAssign(ref lhs, ref rhs) => {
                let lhs = self.type_check(lhs);
                let rhs = self.type_check_assign(rhs, &lhs.ty);
                expr::Expr {
                    value: expr::ExprKind::NonblockAssign(Box::new(lhs), Box::new(rhs)),
                    span: expr.span,
                    ty: Ty::Void,
                }
            }
            // BinaryAssign(Box<Expr>, BinaryOp, Box<Expr>),
            ExprKind::Paren(ref expr) => {
                let conv = self.self_type_check(expr);
                let ty = conv.ty.clone();
                expr::Expr {
                    value: expr::ExprKind::Paren(Box::new(conv)),
                    span: expr.span,
                    ty: ty,
                }
            }
            // MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),
            ExprKind::Cond(ref cond, _, ref true_expr, ref false_expr) => {
                let cond_conv = self.type_check_bool(cond);
                let mut true_conv = self.self_type_check(true_expr);
                let mut false_conv = self.self_type_check(false_expr);
                let myty = match (&true_conv.ty, &false_conv.ty) {
                    (Ty::Int(lsubty), Ty::Int(rsubty)) => Ty::Int(IntTy::SimpleVec(cmp::max(lsubty.width(), rsubty.width()), false, lsubty.sign() && rsubty.sign())),
                    _ => unimplemented!(),
                };
                expr::Expr {
                    value: expr::ExprKind::Cond(Box::new(cond_conv), Box::new(true_conv), Box::new(false_conv)),
                    span: expr.span,
                    ty: myty,
                }
            }
            ref v => {
                eprintln!("{:?}", v);
                unimplemented!();
            }
        }
    }

    /// Type check an expression, expecting it to be convertable to numerical
    pub fn self_type_check_num(&mut self, expr: &Expr) -> expr::Expr {
        let conv = self.self_type_check(expr);
        match conv.ty {
            Ty::Int(_) |
            Ty::Real(_) => (),
            _ => {
                self.diag.report_fatal("this expression is expected to be numerical", expr.span);
            }
        };
        conv
    }

    /// Type check two expressions, expecting them to be convertable to numerical.
    /// If one is real and another isn't, this function will insert implicit casts necessary to
    /// bring both to real.
    pub fn self_type_check_num_2(&mut self, a: &Expr, b: &Expr) -> (expr::Expr, expr::Expr) {
        let mut a_conv = self.self_type_check_num(a);
        let mut b_conv = self.self_type_check_num(b);
        let real = match (&a_conv.ty, &b_conv.ty) {
            // Both integer, okay. We will leave rest to the caller.
            (Ty::Int(_), Ty::Int(_)) => None,
            // Both real, add implicit type conversions if they are of different precision
            (Ty::Real(asubty), Ty::Real(bsubty)) => {
                if asubty != bsubty {
                    // TODO: Add implicit cast
                    unimplemented!();
                }
                None
            }
            // One real, one integral
            (Ty::Real(asubty), Ty::Int(bsubty)) => {
                Some((*asubty, 1, (bsubty.sign(), bsubty.width())))
            }
            (Ty::Int(asubty), Ty::Real(bsubty)) => {
                Some((*bsubty, 0, (asubty.sign(), asubty.width())))
            }
            _ => unreachable!(),
        };
        // If only one side is real, the other side will need to be treated as
        // self-determined.
        if let Some((_realty, expr, ctx)) = real {
            self.propagate_size(if expr == 0 { &mut a_conv } else { &mut b_conv } , ctx);
            // TODO: Add implicit cast to make it also real
            unimplemented!();
        }
        (a_conv, b_conv)
    }

    /// Type check an expression, expecting it to be integral
    pub fn self_type_check_int(&mut self, expr: &Expr) -> expr::Expr {
        let conv = self.self_type_check(expr);
        match conv.ty {
            Ty::Int(_) => (),
            _ => {
                self.diag.report_fatal("this expression is expected to be integral", expr.span);
            }
        };
        conv
    }

    pub fn insert_cast(&mut self, expr: &mut expr::Expr, ctx: (bool, usize)) {
        match expr.ty {
            Ty::Int(ref mut subty) => {
                let subty_sign = subty.sign();
                if subty.width() != ctx.1 {
                    // Width cast
                    let span = expr.span;
                    let old_subty = std::mem::replace(subty, IntTy::SimpleVec(ctx.1, false, subty_sign));
                    ::util::replace_with(&mut expr.value, |old_value| {
                        let old_expr = Box::new(expr::Expr {
                            value: old_value,
                            span: span,
                            ty: Ty::Int(old_subty)
                        });
                        let new_value = expr::ExprKind::WidthCast(
                            ctx.1,
                            old_expr
                        );
                        new_value
                    });
                }
                if subty_sign != ctx.0 {
                    // Insert a sign cast
                    let span = expr.span;
                    let old_subty = std::mem::replace(subty, IntTy::SimpleVec(ctx.1, false, ctx.0));
                    ::util::replace_with(&mut expr.value, |old_value| {
                        let old_expr = Box::new(expr::Expr {
                            value: old_value,
                            span: span,
                            ty: Ty::Int(old_subty)
                        });
                        let new_value = expr::ExprKind::SignCast(
                            ctx.0,
                            old_expr
                        );
                        new_value
                    });
                }
            }
            _ => unreachable!(),
        }
    }

    /// Propagate expression size down
    pub fn propagate_size(&mut self, expr: &mut expr::Expr, ctx: (bool, usize)) {
        match expr.value {
            expr::ExprKind::Const(ref mut val) => {
                match val {
                    Val::Int(val) => {
                        match &mut expr.ty {
                            Ty::Int(intty) => {
                                *val = val.extend_or_trunc(ctx.1);
                                val.signed = ctx.0;
                                *intty = IntTy::SimpleVec(ctx.1, intty.two_state(), ctx.0);
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
                return;
            },
            expr::ExprKind::HierName(..) => (),
            expr::ExprKind::EmptyQueue => (),
            expr::ExprKind::Concat(_) => (),
            expr::ExprKind::MultConcat(..) => (),
            // AssignPattern(Option<Box<DataType>>, AssignPattern),
            expr::ExprKind::Select(..) => (),
            expr::ExprKind::Member(..) => (),
            expr::ExprKind::SysTfCall(..) => (),
            expr::ExprKind::FuncCall { .. } => (),
            // ConstCast(Box<Expr>),
            expr::ExprKind::SignCast(..) => (),
            expr::ExprKind::TypeCast(..) => (),
            expr::ExprKind::WidthCast(..) => (),
            expr::ExprKind::Unary(op, ref mut rhs) => {
                match op {
                    UnaryOp::Add |
                    UnaryOp::Sub |
                    UnaryOp::Not => {
                        // Fix size of self
                        match &mut expr.ty {
                            Ty::Int(IntTy::SimpleVec(width, _, sign)) => {
                                *sign = ctx.0;
                                *width = ctx.1;
                            }
                            Ty::Int(_) => unimplemented!(),
                            // We don't need to handle non-number.
                            _ => return,
                        }
                        self.propagate_size(rhs, ctx);
                        return;
                    }
                    // These have no context-determined operands.
                    UnaryOp::LNot |
                    UnaryOp::And |
                    UnaryOp::Nand |
                    UnaryOp::Or |
                    UnaryOp::Nor |
                    UnaryOp::Xor |
                    UnaryOp::Xnor => (),
                }
            }
            expr::ExprKind::Binary(ref mut lhs, op, ref mut rhs) => {
                match op {
                    BinaryOp::Add |
                    BinaryOp::Sub |
                    BinaryOp::Mul |
                    BinaryOp::Div |
                    BinaryOp::Mod |
                    BinaryOp::And |
                    BinaryOp::Or |
                    BinaryOp::Xor |
                    BinaryOp::Xnor => {
                        // Fix size of self
                        match &mut expr.ty {
                            Ty::Int(IntTy::SimpleVec(width, _, sign)) => {
                                *sign = ctx.0;
                                *width = ctx.1;
                            }
                            Ty::Int(_) => unimplemented!(),
                            // We don't need to handle non-number.
                            _ => return,
                        }
                        self.propagate_size(lhs, ctx);
                        self.propagate_size(rhs, ctx);
                        return;
                    }
                    BinaryOp::Power |
                    BinaryOp::Shl |
                    BinaryOp::LShr |
                    BinaryOp::AShr => {
                        // Fix size of self
                        match &mut expr.ty {
                            Ty::Int(IntTy::SimpleVec(width, _, sign)) => {
                                *sign = ctx.0;
                                *width = ctx.1;
                            }
                            Ty::Int(_) => unimplemented!(),
                            _ => unreachable!(),
                        }
                        self.propagate_size(lhs, ctx);
                        return;
                    }
                    // These operator has no context-determined operands.
                    BinaryOp::Eq |
                    BinaryOp::Neq |
                    BinaryOp::CaseEq |
                    BinaryOp::CaseNeq |
                    BinaryOp::WildEq |
                    BinaryOp::WildNeq |
                    BinaryOp::LAnd |
                    BinaryOp::LOr |
                    BinaryOp::Imply |
                    BinaryOp::Equiv |
                    BinaryOp::Lt |
                    BinaryOp::Leq |
                    BinaryOp::Gt |
                    BinaryOp::Geq => (),
                }
            }
            // Binary(Box<Expr>, BinaryOp, Option<Box<AttrInst>>, Box<Expr>),
            // PrefixIncDec(IncDec, Option<Box<AttrInst>>, Box<Expr>),
            expr::ExprKind::PostfixIncDec(..) => (),
            // Assign(Box<Expr>, Box<Expr>),
            // BinaryAssign(Box<Expr>, BinaryOp, Box<Expr>),
            expr::ExprKind::Paren(ref mut expr) => {
                self.propagate_size(expr, ctx);
                return;
            }
            // MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),
            expr::ExprKind::Cond(_, ref mut t, ref mut f) => {
                // Fix size of self
                match &mut expr.ty {
                    Ty::Int(IntTy::SimpleVec(width, _, sign)) => {
                        *sign = ctx.0;
                        *width = ctx.1;
                    }
                    Ty::Int(_) => unimplemented!(),
                    // We don't need to handle non-number.
                    _ => return,
                }
                self.propagate_size(t, ctx);
                self.propagate_size(f, ctx);
                return;
            }
            ref v => {
                eprintln!("{:?}", v);
                unimplemented!();
            }
        }
        self.insert_cast(expr, ctx)
    }

    /// Two-stage type check. First do self_type_check and then perform size_propagate. If
    /// target is not none, then the expression will be type-checked in an assignment-like
    /// context.
    pub fn type_check(&mut self, expr: &Expr) -> expr::Expr {
        let mut expr = self.self_type_check(expr);
        let ctx = match expr.ty {
            // If expression is type-checked to be a simple vector, we need to propagate size
            // back to all context-determined subexpressions.
            Ty::Int(IntTy::SimpleVec(width, _, sign)) => (sign, width),
            _ => return expr,
        };
        self.propagate_size(&mut expr, ctx);
        expr
    }

    /// Placeholder for those should be in assignment-context but haven't implemented yet.
    pub fn type_check_assign_todo(&mut self, expr: &Expr) -> expr::Expr {
        self.type_check(expr)
    }

    pub fn type_check_assign(&mut self, expr: &Expr, target: &Ty) -> expr::Expr {
        match &expr.value {
            // For an untyped assignmenet pattern in this context, we can automatically infer
            // its type.
            ExprKind::AssignPattern(None, pattern) => {
                let pattern = self.type_check_assign_pattern(target, pattern);
                expr::Expr {
                    value: expr::ExprKind::AssignPattern(Box::new(target.clone()), pattern),
                    span: expr.span,
                    ty: target.clone(),
                }
            }
            _ => {
                let mut expr = self.self_type_check(expr);
                let mut ctx = match expr.ty {
                    // If expression is type-checked to be a simple vector, we need to propagate size
                    // back to all context-determined subexpressions.
                    Ty::Int(IntTy::SimpleVec(width, _, sign)) => (sign, width),
                    _ => return expr,
                };
                match target {
                    Ty::Int(intty) => {
                        ctx.0 |= intty.sign();
                        ctx.1 = cmp::max(ctx.1, intty.width());
                    }
                    // TODO: Maybe should add cast here.
                    _ => (),
                }
                self.propagate_size(&mut expr, ctx);
                expr
            }
        }
    }

    /// Type check an expression, expecting it to be convertable to boolean
    pub fn type_check_bool(&mut self, expr: &Expr) -> expr::Expr {
        self.type_check(expr)
    }

    /// Type check an integral expression, with desired width and signing.
    pub fn type_check_int(&mut self, expr: &Expr) -> expr::Expr {
        let mut conv = self.self_type_check_int(expr);
        let ctx = match conv.ty {
            // If expression is type-checked to be a simple vector, we need to propagate size
            // back to all context-determined subexpressions.
            Ty::Int(IntTy::SimpleVec(width, _, sign)) => (sign, width),
            _ => return conv,
        };
        self.propagate_size(&mut conv, ctx);
        conv
    }

    /// Evaluate a constant expression.
    pub fn eval_checked_expr(&mut self, expr: &expr::Expr) -> Val {
        match &expr.value {
            expr::ExprKind::Const(val) => val.clone(),
            expr::ExprKind::HierName(name) => {
                match self.type_check_hier_id(name, expr.span).0 {
                    Some(hier) => {
                        match hier {
                            HierItem::GenVar(ref genvar) => {
                                Val::Int(LogicVec::from_integer(*genvar.value.borrow()))
                            },
                            _ => unimplemented!(),
                        }
                    }
                    None => unreachable!(),
                }
            }
            // EmptyQueue,
            expr::ExprKind::Concat(subexpr) => {
                // subexpr.len() is always > 1, which is guaranteed by parser.
                let mut val = match self.eval_checked_expr(subexpr.first().unwrap()) {
                    Val::Int(val) => val,
                    _ => unreachable!(),
                };
                for expr in subexpr.iter().skip(1) {
                    let subval = match self.eval_checked_expr(expr) {
                        Val::Int(val) => val,
                        _ => unreachable!(),
                    };  
                    val.concat_assign(&subval);
                }
                Val::Int(val)
            }
            // MultConcat(Box<Expr>, Box<Expr>),
            // AssignPattern(Option<Box<DataType>>, AssignPattern),
            // Select(Box<Expr>, Dim),
            // Member(Box<Expr>, Ident),
            expr::ExprKind::SysTfCall(task, args) => {
                match task.as_str() {
                    "$clog2" => (),
                    v => unimplemented!("{:?}", v),
                }
                let args: Vec<_> = args.iter().map(|v| self.eval_checked_expr(v.as_ref().unwrap())).collect();
                if let Val::Int(vec) = &args[0] {
                    Val::Int(LogicVec::from_biguint(
                        32, true,
                        BigUint::from_usize((vec.get_two_state().unwrap().to_usize().unwrap() as f64).log2().ceil() as usize).unwrap()
                    ))
                } else {
                    unreachable!();
                }
            }
            // ConstCast(Box<Expr>),
            expr::ExprKind::SignCast(sign, inside) => {
                let inside_val = self.eval_checked_expr(inside);
                if let Val::Int(mut v) = inside_val {
                    v.signed = *sign;
                    Val::Int(v)
                } else {
                    unreachable!()
                }
            }
            expr::ExprKind::TypeCast(_, inside) |
            expr::ExprKind::WidthCast(_, inside) => {
                let inside_val = self.eval_checked_expr(inside);
                let self_ty = &expr.ty;
                let inside_ty = &inside.ty;
                match (self_ty, inside_ty) {
                    (Ty::Int(ssubty), Ty::Int(isubty)) => {
                        if let Val::Int(mut v) = inside_val {
                            v.signed = isubty.sign();
                            // If width is different, do a sign/xz extension or truncation
                            if ssubty.width() != v.width() {
                                v = v.extend_or_trunc(ssubty.width())
                            }
                            // If we are casting from 4-state to 2-state, convert all X/Z into zeros
                            if ssubty.two_state() && !isubty.two_state() {
                                v = v.force_two_state()
                            }
                            v.signed = ssubty.sign();
                            Val::Int(v)
                        } else {
                            unreachable!();
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            expr::ExprKind::Unary(op, rhs) => {
                let val = self.eval_checked_expr(rhs);
                match op {
                    UnaryOp::Add => val,
                    UnaryOp::Sub => {
                        match val {
                            Val::Int(val) => {
                                Val::Int(-val)
                            }
                            _ => unreachable!(),
                        }
                    }
                    UnaryOp::Not => unimplemented!(),
                    UnaryOp::LNot => {
                        match val {
                            Val::Int(val) => {
                                Val::Int((!val.to_bool()).into())
                            }
                            _ => unimplemented!(),
                        }
                    }
                    UnaryOp::And |
                    UnaryOp::Nand |
                    UnaryOp::Or |
                    UnaryOp::Nor |
                    UnaryOp::Xor |
                    UnaryOp::Xnor => unimplemented!(),
                }
            }
            expr::ExprKind::Binary(lhs, op, rhs) => {
                let lval = self.eval_checked_expr(lhs);
                let rval = self.eval_checked_expr(rhs);
                match op {
                    BinaryOp::Add |
                    BinaryOp::Sub |
                    BinaryOp::Mul |
                    BinaryOp::Div |
                    BinaryOp::Mod |
                    BinaryOp::Power => {
                        match (lval, rval) {
                            (Val::Real(l), Val::Real(r)) => {
                                let mut ret = match op {
                                    BinaryOp::Add => l + r,
                                    BinaryOp::Sub => l - r,
                                    BinaryOp::Mul => l * r,
                                    BinaryOp::Div => l / r,
                                    BinaryOp::Mod => l % r,
                                    BinaryOp::Power => unimplemented!(),
                                    _ => unreachable!(),
                                };
                                if let Ty::Real(RealTy::Shortreal) = expr.ty {
                                    ret = ret as f32 as f64;
                                }
                                Val::Real(ret)
                            }
                            (Val::Int(mut lval), Val::Int(rval)) => {
                                match op {
                                    BinaryOp::Add => lval += &rval,
                                    BinaryOp::Sub => lval -= &rval,
                                    BinaryOp::Mul => lval *= &rval,
                                    BinaryOp::Div => lval /= &rval,
                                    BinaryOp::Mod => lval %= &rval,
                                    BinaryOp::Power => lval.pow_assign(&rval),
                                    _ => unreachable!(),
                                }
                                Val::Int(lval)
                            }
                            _ => unreachable!(),
                        }
                    },
                    BinaryOp::And |
                    BinaryOp::Or |
                    BinaryOp::Xor |
                    BinaryOp::Xnor => {
                        match (lval, rval) {
                            (Val::Int(mut lval), Val::Int(rval)) => {
                                match op {
                                    BinaryOp::And => lval &= &rval,
                                    BinaryOp::Or => lval |= &rval,
                                    BinaryOp::Xor => lval ^= &rval,
                                    BinaryOp::Xnor => {
                                        lval ^= &rval;
                                        lval = !lval;
                                    }
                                    _ => unreachable!(),
                                }
                                Val::Int(lval)
                            }
                            _ => unreachable!(),
                        }
                    },
                    BinaryOp::Shl |
                    BinaryOp::LShr |
                    BinaryOp::AShr => {
                        if let (Val::Int(mut l), Val::Int(r)) = (lval, rval) {
                            match op {
                                BinaryOp::Shl => l <<= &r,
                                BinaryOp::LShr => l.l_shr(&r),
                                BinaryOp::AShr => l.a_shr(&r),
                                _ => unreachable!(),
                            }
                            Val::Int(l)
                        } else {
                            unreachable!();
                        }
                    }
                    // For all operations without context-determined operands, we will need to
                    // add a cast operator if width mismatches.
                    BinaryOp::Eq |
                    BinaryOp::Neq => {
                        let result = match (lval, rval) {
                            (Val::Real(l), Val::Real(r)) => {
                                match op {
                                    BinaryOp::Eq => l == r,
                                    BinaryOp::Neq => l != r,
                                    _ => unreachable!(),
                                }.into()
                            }
                            (Val::Int(l), Val::Int(r)) => {
                                match op {
                                    BinaryOp::Eq => l.logic_eq(&r),
                                    BinaryOp::Neq => !l.logic_eq(&r),
                                    _ => unreachable!(),
                                }
                            }
                            _ => unimplemented!(),
                        };
                        Val::Int(result.into())
                    }
                    BinaryOp::CaseEq |
                    BinaryOp::CaseNeq |
                    BinaryOp::WildEq |
                    BinaryOp::WildNeq => unimplemented!(),
                    BinaryOp::LAnd |
                    BinaryOp::LOr |
                    BinaryOp::Imply |
                    BinaryOp::Equiv=> {
                        let lbool = match lval {
                            Val::Int(val) => {
                                val.to_bool()
                            }
                            _ => unimplemented!(),
                        };
                        let rbool = match rval {
                            Val::Int(val) => {
                                val.to_bool()
                            }
                            _ => unimplemented!(),
                        };
                        let result = match (op, lbool, rbool) {
                            (BinaryOp::LAnd, LogicValue::Zero, _) |
                            (BinaryOp::LAnd, _, LogicValue::Zero) => LogicValue::Zero,
                            (BinaryOp::LAnd, LogicValue::One, LogicValue::One) => LogicValue::One,

                            (BinaryOp::LOr, LogicValue::Zero, LogicValue::Zero) => LogicValue::Zero,
                            (BinaryOp::LOr, LogicValue::One, _) |
                            (BinaryOp::LOr, _, LogicValue::One) => LogicValue::One,

                            (BinaryOp::Imply, LogicValue::One, LogicValue::Zero) => LogicValue::Zero,
                            (BinaryOp::Imply, LogicValue::Zero, _) |
                            (BinaryOp::Imply, _, LogicValue::One) => LogicValue::One,

                            (BinaryOp::Equiv, LogicValue::One, LogicValue::Zero) |
                            (BinaryOp::Equiv, LogicValue::Zero, LogicValue::One) => LogicValue::Zero,
                            (BinaryOp::Equiv, LogicValue::Zero, LogicValue::Zero) |
                            (BinaryOp::Equiv, LogicValue::One, LogicValue::One) => LogicValue::One,
                            
                            _ => LogicValue::X,
                        };
                        Val::Int(result.into())
                    }
                    BinaryOp::Lt |
                    BinaryOp::Leq |
                    BinaryOp::Gt |
                    BinaryOp::Geq => {
                        let result = match (lval, rval) {
                            (Val::Real(l), Val::Real(r)) => {
                                match op {
                                    BinaryOp::Lt => l < r,
                                    BinaryOp::Leq => l <= r,
                                    BinaryOp::Gt => l <= r,
                                    BinaryOp::Geq => l >= r,
                                    _ => unreachable!(),
                                }.into()
                            }
                            (Val::Int(l), Val::Int(r)) => {
                                match op {
                                    BinaryOp::Lt => l.lt(&r),
                                    BinaryOp::Leq => l.le(&r),
                                    BinaryOp::Gt => !l.le(&r),
                                    BinaryOp::Geq => !l.lt(&r),
                                    _ => unreachable!(),
                                }
                            }
                            _ => unimplemented!(),
                        };
                        Val::Int(result.into())
                    }
                }
            }
            // PrefixIncDec(IncDec, Option<Box<AttrInst>>, Box<Expr>),
            expr::ExprKind::PostfixIncDec(lhs, incdec) => {
                if let expr::ExprKind::HierName(ref name) = lhs.value {
                    let hier = match self.type_check_hier_id(name, lhs.span).0 {
                        None => unimplemented!(),
                        Some(hier) => hier,
                    };
                    let var = match hier {
                        HierItem::GenVar(ref var) => &var.value,
                        _ => unimplemented!(),
                    };
                    let old_value = *var.borrow();
                    let new_value = if let IncDec::Inc = incdec {
                        old_value + 1
                    } else {
                        old_value - 1
                    };
                    *var.borrow_mut() = new_value;
                    Val::Int(LogicVec::from_integer(old_value))
                } else {
                    unimplemented!("{:?}", lhs)
                }
            }
            // Assign(Box<Expr>, Box<Expr>),
            // BinaryAssign(Box<Expr>, BinaryOp, Box<Expr>),
            expr::ExprKind::Paren(expr) => {
                self.eval_checked_expr(&expr)
            }
            // MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),
            // Cond(Box<Expr>, Option<Box<AttrInst>>, Box<Expr>, Box<Expr>),
            v => {
                eprintln!("{:?}", v);
                unimplemented!();
            }
        }
    }

    pub fn eval_expr_i32(&mut self, expr: &Expr) -> i32 {
        if let (_, Val::Int(val)) = self.eval_expr(expr) {
            match val.get_two_state().and_then(|v| v.to_i32()) {
                None => {
                    self.diag.report_fatal(
                        "this expression must evaluate to two-state number",
                        expr.span
                    );
                },
                Some(v) => v,
            }
        } else {
            self.diag.report_fatal(
                "this expression must evaluate to integral number",
                expr.span
            );
        }
    }

    pub fn eval_expr_usize_positive(&mut self, expr: &Expr) -> usize {
        let value = self.eval_expr_i32(expr);
        if value <= 0 {
            self.diag.report_error("this expression must evaluate to positive number", expr.span);
            // Error recovery
            1
        } else {
            value as usize
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> (Ty, Val) {
        let conv = self.type_check(&expr);
        let val = self.eval_checked_expr(&conv);
        (conv.ty, val)
    }

    fn eval_expr_assign(&mut self, expr: &Expr, target: &Ty) -> (Ty, Val) {
        let conv = self.type_check_assign(&expr, target);
        let val = self.eval_checked_expr(&conv);
        (conv.ty, val)
    }

    /// Evaluate a constant unpacked dimension and return a vector of bounds.
    pub fn eval_const_unpacked_dim(&mut self, dim: &Vec<Dim>) -> Vec<(i32, i32)> {
        dim.iter().map(|dim| {
            match &dim.value {
                DimKind::Range(a, b) => {
                    let ub = self.eval_expr_i32(a);
                    let lb = self.eval_expr_i32(b);
                    (ub, lb)
                }
                DimKind::Value(a) => {
                    let size = self.eval_expr_usize_positive(a) as i32;
                    (0, size - 1)
                }
                _ => self.diag.report_fatal("unexpected dimension format", dim.span),
            }
        }).collect()
    }
}
