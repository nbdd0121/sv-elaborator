//! Reconstruct AST from elaborated constructs.

use syntax::ast::*;
use syntax::tokens::*;
use source::Span;
use num::{BigInt, Zero};
use number::{LogicVec, LogicNumber, LogicValue};
use super::ty::{Ty, IntTy, Struct, Enum};
use super::expr::Val;
use super::hier::{self, HierItem};

pub fn reconstruct(source: &hier::Source) -> Vec<Vec<Item>> {
    let reconstructor = Reconstructor { source };
    reconstructor.reconstruct()
}

struct Reconstructor<'a> {
    source: &'a hier::Source,
}

impl<'a> Reconstructor<'a> {
    /// Given a i32, reconstruct the corresponding corresponding constant value.
    pub fn reconstruct_int(&self, val: i32, span: Span) -> Expr {
        let expr = Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
            LogicNumber {
                value: LogicVec::from(32, true, ::num::FromPrimitive::from_i32(val.abs()).unwrap()),
                sized: false, 
            }
        ), span)), span);
        if val >= 0 {
            expr
        } else {
            Spanned::new(ExprKind::Unary(UnaryOp::Sub, None, Box::new(expr)), span)
        }
    }

    // Given a LogicVec, reconstruct the corresponding corresponding constant value.
    pub fn reconstruct_const(&self, val: &LogicVec, span: Span) -> Expr {
        Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
            LogicNumber {
                value: val.clone(),
                sized: true, 
            }
        ), span)), span)
    }

    /// Given an ty::Enum, reconstruct ast::EnumDecl
    pub fn reconstruct_enum(&self, enu: &Enum, prefix: &str) -> EnumDecl {
        let base = self.reconstruct_ty_int(&enu.base, Span::none());
        let members = enu.elements.borrow().iter().map(|(name, val)| {
            DeclAssign {
                name: Ident::new(format!("{}_{}", prefix, name.value), name.span),
                dim: Vec::new(),
                init: Some(Box::new(self.reconstruct_val_int(&enu.base, val, Span::none()))),
            }
        }).collect();
        EnumDecl {
            ty: Some(Box::new(base)),
            members,
        }
    }

    /// Given an ty::Struct, reconstruct ast::AggrDecl
    pub fn reconstruct_struct(&self, struc: &Struct) -> AggrDecl {
        let members = struc.members.iter().map(|(ty, name, init)| {
            let astty = self.reconstruct_ty_int(&ty, Span::none());
            AggrMember {
                attr: None,
                ty: astty,
                list: vec![DeclAssign {
                    name: name.clone(),
                    dim: Vec::new(),
                    init: init.as_ref().map(|init| {
                        Box::new(self.reconstruct_val_int(&ty, &init, Span::none()))
                    }),
                }],
            }
        }).collect();

        AggrDecl {
            kind: AggrType::Struct,
            packed: true,
            sign: if struc.sign { Signing::Signed } else { Signing::Unsigned },
            members,
        }
    }

    pub fn reconstruct_ty_int(&self, ty: &IntTy, span: Span) -> DataType {
        let mut inner = ty;
        let mut dim = Vec::new();
        while let IntTy::Array(element, ub, lb) = inner {
            inner = &**element;
            // Synthesis fake expression nodes from constants
            let a_expr = Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
                LogicNumber {
                    value: LogicVec::from(32, true, ::num::FromPrimitive::from_i32(*ub).unwrap()),
                    sized: false,
                }
            ), span)), span);
            let b_expr = Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
                LogicNumber {
                    value: LogicVec::from(32, true, ::num::FromPrimitive::from_i32(*lb).unwrap()),
                    sized: false,
                }
            ), span)), span);
            dim.push(Spanned::new(DimKind::Range(Box::new(a_expr), Box::new(b_expr)), span))
        }
        let kind = match inner {
            IntTy::Array(..) => unreachable!(),
            IntTy::Logic(two_state, sign) => {
                DataTypeKind::IntVec(
                    if *two_state { IntVecTy::Bit } else { IntVecTy::Logic },
                    if *sign { Signing::Signed} else { Signing::Unsigned },
                    dim
                )
            }
            IntTy::SimpleVec(width, two_state, sign) => {
                // Synthesis fake expression nodes from constants
                let a_expr = Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
                    LogicNumber {
                        value: LogicVec::from(32, true, ::num::FromPrimitive::from_usize(width - 1).unwrap()),
                        sized: false,
                    }
                ), span)), span);
                let b_expr = Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
                    LogicNumber {
                        value: LogicVec::from(32, true, BigInt::zero()),
                        sized: false,
                    }
                ), span)), span);
                dim.push(Spanned::new(DimKind::Range(Box::new(a_expr), Box::new(b_expr)), span));
                DataTypeKind::IntVec(
                    if *two_state { IntVecTy::Bit } else { IntVecTy::Logic },
                    if *sign { Signing::Signed} else { Signing::Unsigned},
                    dim
                )
            }
            IntTy::Struct(struc) => {
                DataTypeKind::HierName(None, Ident::new(format!("struct_{}", self.source.structs.iter().position(|x| x == struc).unwrap()), Span::none()), dim)
            }
            IntTy::Enum(enu) => {
                DataTypeKind::HierName(None, Ident::new(format!("enum_{}", self.source.enums.iter().position(|x| x == enu).unwrap()), Span::none()), dim)
            }
        };
        Spanned::new(kind, span)
    }

    pub fn reconstruct_ty(&self, ty: &Ty, span: Span) -> (DataType, Vec<Dim>) {
        let kind = match ty {
            Ty::Type => DataTypeKind::Type,
            Ty::Int(subty) => return (self.reconstruct_ty_int(subty, span), Vec::new()),
            Ty::String => DataTypeKind::String,
            v => unimplemented!("{:?}", v),
        };
        (Spanned::new(kind, span), Vec::new())
    }

    pub fn reconstruct_val_int(&self, ty: &IntTy, val: &LogicVec, span: Span) -> Expr {
        match ty {
            IntTy::SimpleVec(_, false, _) => {
                self.reconstruct_const(val, span)
            }
            IntTy::Enum(_) => {
                let ty = self.reconstruct_ty_int(ty, span);
                let val = self.reconstruct_const(val, span);
                Spanned::new(ExprKind::TypeCast(
                    Box::new(Spanned::new(ExprKind::Type(Box::new(ty)), span)),
                    Box::new(val)
                ), span)
            }
            _ => unimplemented!(),
        }
    }

    pub fn reconstruct_val(&self, ty: &Ty, val: &Val, span: Span) -> Expr {
        let kind = match ty {
            Ty::Type => {
                let ty = if let Val::Type(ty) = val { ty } else { unreachable!()};
                let (ty, dim) = self.reconstruct_ty(ty, span);
                // TODO: How to deal with dim
                assert!(dim.len() == 0);
                ExprKind::Type(Box::new(ty))
            }
            Ty::Int(subty) => {
                let val = if let Val::Int(v) = val { v } else { unreachable!() };
                return self.reconstruct_val_int(subty, val, span);
            }
            Ty::FixStr(_) => {
                let val = if let Val::FixStr(v) = val { v } else { unreachable!() };
                ExprKind::Literal(Spanned::new(TokenKind::StringLiteral(val.clone()), span))
            }
            _ => unimplemented!(),
        };
        Spanned::new(kind, span)
    }

    pub fn reconstruct_item(&self, item: &HierItem, list: &mut Vec<Item>) {
        match item {
            HierItem::Param(decl) => {
                let (ty, dim) = self.reconstruct_ty(&decl.ty, Span::none());
                let expr = self.reconstruct_val(&decl.ty, &decl.init, Span::none());
                list.push(Item::ParamDecl(Box::new(ParamDecl {
                    kw: decl.kw,
                    ty: Some(Box::new(ty)),
                    list: vec![DeclAssign {
                        name: decl.name.clone(),
                        dim,
                        init: Some(Box::new(expr)),
                    }]
                })));
            }
            HierItem::Type(decl) => {
                let (ty, dim) = self.reconstruct_ty(&decl.ty, Span::none());
                list.push(Item::Typedef(None, Box::new(ty), Box::new(decl.name.clone()), dim));
            }
            HierItem::Design(decl) => {
                // Reconstruct all instantiations and display them here
                for (_, inst) in decl.instances.borrow().iter() {
                    list.push(self.reconstruct_instantiation(inst));
                }
            }
            HierItem::Instance(decl) => {
                list.push(Item::HierInstantiation(Box::new(HierInstantiation {
                    attr: None,
                    name: decl.inst.name.clone(),
                    param: None,
                        inst: vec![HierInst {
                        name: decl.name.clone(),
                        dim: decl.dim.iter().map(|(ub, lb)| {
                            let ub_expr = self.reconstruct_int(*ub, Span::none());
                            let lb_expr = self.reconstruct_int(*lb, Span::none());
                            Spanned::new(DimKind::Range(Box::new(ub_expr), Box::new(lb_expr)), Span::none())
                        }).collect(),
                        ports: decl.port.clone(),
                    }]
                })))
            }
            HierItem::Other(item) => list.push(Item::clone(item)),
            HierItem::OtherName => (),
            HierItem::GenBlock(genblk) => {
                // Reconstruct all interior items
                let mut sublist = Vec::new();
                for item in &genblk.scope.items {
                    self.reconstruct_item(item, &mut sublist);
                }
                list.push(Item::IfGen(Box::new(IfGen {
                    attr: None,
                    if_block: vec![{
                        (self.reconstruct_const(&LogicValue::One.into(), Span::none()), GenBlock {
                            name: genblk.name.clone(),
                            items: sublist,
                        })
                    }],
                    else_block: None,
                })));
            },
            HierItem::LoopGenBlock(loopgenblk) => {
                for (_, genblk) in loopgenblk.instances.borrow().iter() {
                    // Reconstruct all interior items
                    let mut sublist = Vec::new();
                    for item in &genblk.scope.items {
                        self.reconstruct_item(item, &mut sublist);
                    }
                    list.push(Item::IfGen(Box::new(IfGen {
                        attr: None,
                        if_block: vec![{
                            (self.reconstruct_const(&LogicValue::One.into(), Span::none()), GenBlock {
                                name: genblk.name.clone(),
                                items: sublist,
                            })
                        }],
                        else_block: None,
                    })));
                }
            }
            HierItem::Modport(_) => (),
            HierItem::Enum(..) => (),
            v => unimplemented!("{:?}", std::mem::discriminant(v)),
        }
    }

    pub fn reconstruct_instantiation(&self, inst: &hier::DesignInstantiation) -> Item {
        // Reconstruct all parameters
        let mut params = Vec::new();
        for item in &inst.scope.items {
            if let HierItem::Param(_) = item {
                self.reconstruct_item(item, &mut params);
            } else {
                break;
            }
        }
        let params: Vec<_> = params.into_iter().map(|x| {
            if let Item::ParamDecl(decl) = x { *decl } else { unreachable!() }
        }).collect();

        // Reconstruct all ports
        let mut ports = Vec::new();
        for item in inst.scope.items.iter().skip(params.len()) {
            match item {
                HierItem::DataPort(decl) => {
                    let (ty, dim) = self.reconstruct_ty(&decl.ty, Span::none());
                    ports.push(PortDecl::Data(decl.dir, decl.net.clone(), Box::new(ty), vec![DeclAssign {
                        name: decl.name.clone(),
                        dim,
                        init: decl.init.clone(),
                    }]));
                }
                HierItem::InterfacePort(decl) => {
                    ports.push(PortDecl::Interface(Some(Box::new(decl.inst.name.clone())), None, vec![DeclAssign {
                        name: decl.name.clone(),
                        dim: Vec::new(),
                        init: None,
                    }]));
                }
                _ => break,
            }
        }

        // Now chunk them into params, port and items
        let mut list = Vec::new();
        for item in inst.scope.items.iter().skip(params.len() + ports.len()) {
            self.reconstruct_item(&item, &mut list);
        }

        Item::DesignDecl(Box::new(DesignDecl {
            attr: None,
            kw: inst.ast.kw,
            lifetime: inst.ast.lifetime,
            name: inst.name.clone(),
            pkg_import: Vec::new(),
            param: if !params.is_empty() { Some(params) } else { None },
            port: ports,
            items: list,
        }))
    }

    pub fn reconstruct(&self) -> Vec<Vec<Item>> {
        let mut units = Vec::new();

        // Build global unit: contain packages and typedefs
        let mut list = Vec::new();
        list.extend(self.source.enums.iter().enumerate().map(|(index, enu)| {
            let prefix = format!("enum_{}", index);
            let enu = self.reconstruct_enum(enu, &prefix);
            let ty = Spanned::new(DataTypeKind::Enum(enu, Vec::new()), Span::none());
            let name = Ident::new(prefix, Span::none());
            Item::Typedef(None, Box::new(ty), Box::new(name), Vec::new())
        }));
        list.extend(self.source.structs.iter().enumerate().map(|(index, struc)| {
            let struc = self.reconstruct_struct(struc);
            let ty = Spanned::new(DataTypeKind::Aggr(struc, Vec::new()), Span::none());
            let name = Ident::new(format!("struct_{}", index), Span::none());
            Item::Typedef(None, Box::new(ty), Box::new(name), Vec::new())
        }));
        list.extend(self.source.pkgs.iter().map(|(_, decl)| {
            let mut list = Vec::new();
            for item in &decl.scope.items {
                self.reconstruct_item(item, &mut list);
            }
            Item::PkgDecl(Box::new(PkgDecl {
                attr: None,
                lifetime: Lifetime::Static,
                name: decl.name.clone(),
                items: list
            }))
        }));
        units.push(list);

        for unit in &self.source.units { 
            let mut list = Vec::new();
            for item in &unit.items {
                self.reconstruct_item(item, &mut list);
            }
            units.push(list);
        }
        units
    }
}
