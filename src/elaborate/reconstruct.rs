//! Reconstruct AST from elaborated constructs.

use std::collections::HashMap;

use syntax::ast::{self, *};
use syntax::tokens::*;
use source::Span;
use number::{LogicVec, LogicNumber, LogicValue};
use super::ty::{Ty, IntTy, Struct, Enum};
use super::expr::{self, Val};
use super::hier::{self, HierItem};

pub fn reconstruct(source: &hier::Source) -> Vec<Vec<Item>> {
    let mut reconstructor = Reconstructor {
        source,
        global_qualify: true,
        simple_ty_map: HashMap::new(),
    };
    reconstructor.reconstruct()
}

/// Given a i32, reconstruct the corresponding corresponding constant value.
pub fn reconstruct_usize(val: usize) -> Expr {
    Spanned::new_unspanned(ExprKind::Literal(Spanned::new_unspanned(TokenKind::IntegerLiteral(
        LogicNumber {
            value: LogicVec::from(32, true, ::num::FromPrimitive::from_usize(val).unwrap()),
            sized: false, 
        }
    ))))
}


/// Given a i32, reconstruct the corresponding corresponding constant value.
pub fn reconstruct_i32(val: i32) -> Expr {
    let expr = reconstruct_usize(val.abs() as usize);
    if val >= 0 {
        expr
    } else {
        Spanned::new(ExprKind::Unary(UnaryOp::Sub, None, Box::new(expr)), Span::none())
    }
}

struct Reconstructor<'a> {
    source: &'a hier::Source,
    /// Whether reference to structs and enums need to be qualified by "global_types::"
    global_qualify: bool,
    /// Map from a type to an global_types:: identifier
    simple_ty_map: HashMap<Ty, Ident>,
}

impl<'a> Reconstructor<'a> {
    // Given a LogicVec, reconstruct the corresponding corresponding constant value.
    pub fn reconstruct_const(&self, val: &LogicVec, span: Span) -> Expr {
        Spanned::new(ExprKind::Literal(Spanned::new(TokenKind::IntegerLiteral(
            LogicNumber {
                value: val.clone(),
                sized: val.width() != 32 || !val.signed, 
            }
        ), span)), span)
    }

    /// Given an ty::Enum, reconstruct ast::EnumDecl
    pub fn reconstruct_enum(&mut self, enu: &Enum, prefix: &str) -> EnumDecl {
        let base = match &enu.base {
            // int is the default type so don't print it out.
            IntTy::SimpleVec(32, true, true) => None,
            v => Some(Box::new(self.reconstruct_ty_int(v, Span::none()))),
        };
        let members = enu.elements.borrow().iter().map(|(name, val)| {
            DeclAssign {
                name: Ident::new(format!("{}_{}", prefix, name.value), name.span),
                dim: Vec::new(),
                init: Some(Box::new(self.reconstruct_const(val, Span::none()))),
            }
        }).collect();
        EnumDecl {
            ty: base,
            members,
        }
    }

    /// Given an ty::Struct, reconstruct ast::AggrDecl
    pub fn reconstruct_struct(&mut self, struc: &Struct) -> AggrDecl {
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
            let a_expr = reconstruct_i32(*ub);
            let b_expr = reconstruct_i32(*lb);
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
            IntTy::SimpleVec(8, true, true) => DataTypeKind::IntAtom(IntAtomTy::Byte, None),
            IntTy::SimpleVec(16, true, true) => DataTypeKind::IntAtom(IntAtomTy::Shortint, None),
            IntTy::SimpleVec(32, true, true) => DataTypeKind::IntAtom(IntAtomTy::Int, None),
            IntTy::SimpleVec(64, true, true) => DataTypeKind::IntAtom(IntAtomTy::Longint, None),
            IntTy::SimpleVec(32, false, true) => DataTypeKind::IntAtom(IntAtomTy::Integer, None),
            IntTy::SimpleVec(64, false, false) => DataTypeKind::IntAtom(IntAtomTy::Time, None),
            IntTy::SimpleVec(width, two_state, sign) => {
                // Synthesis fake expression nodes from constants
                let a_expr = reconstruct_usize(*width - 1);
                let b_expr = reconstruct_usize(0);
                dim.push(Spanned::new(DimKind::Range(Box::new(a_expr), Box::new(b_expr)), span));
                DataTypeKind::IntVec(
                    if *two_state { IntVecTy::Bit } else { IntVecTy::Logic },
                    if *sign { Signing::Signed} else { Signing::Unsigned},
                    dim
                )
            }
            IntTy::Struct(struc) => {
                DataTypeKind::HierName(
                    if self.global_qualify {
                        Some(Scope::Name(None, Box::new(Ident::new_unspanned("global_types".to_owned()))))
                    } else {
                        None
                    },
                    Ident::new_unspanned(format!("struct_{}", self.source.structs.iter().position(|x| x == struc).unwrap())),
                    dim
                )
            }
            IntTy::Enum(enu) => {
                DataTypeKind::HierName(
                    if self.global_qualify {
                        Some(Scope::Name(None, Box::new(Ident::new_unspanned("global_types".to_owned()))))
                    } else {
                        None
                    },
                    Ident::new_unspanned(format!("enum_{}", self.source.enums.iter().position(|x| x == enu).unwrap())),
                    dim
                )
            }
        };
        Spanned::new(kind, span)
    }

    pub fn reconstruct_ty(&self, ty: &Ty, span: Span) -> (DataType, Vec<Dim>) {
        let kind = match ty {
            Ty::Type => DataTypeKind::Type,
            Ty::Int(subty) => return (self.reconstruct_ty_int(subty, span), Vec::new()),
            Ty::String => DataTypeKind::String,
            Ty::FixStr(_) => DataTypeKind::String, // TODO: May need to fix this
            Ty::Array(base, ub, lb) => {
                let (ty, mut dim) = self.reconstruct_ty(base, Span::none());
                let ast_dim = Spanned::new_unspanned(DimKind::Range(
                    Box::new(reconstruct_i32(*ub)),
                    Box::new(reconstruct_i32(*lb))
                ));
                dim.insert(0, ast_dim);
                return (ty, dim)
            }
            v => unimplemented!("{:?}", v),
        };
        (Spanned::new(kind, span), Vec::new())
    }

    pub fn reconstruct_ty_simple(&mut self, ty: &Ty) -> DataType {
        let (mut ast_ty, dim) = self.reconstruct_ty(ty, Span::none());
        assert!(dim.len() == 0);
        match ast_ty.value {
            // These are already simple types
            DataTypeKind::IntAtom(_, None) |
            DataTypeKind::Real(_) |
            DataTypeKind::HierName(..) => (),
            _ => {
                if !self.simple_ty_map.contains_key(&ty) {
                    let ident = Ident::new_unspanned(format!("type_{}", self.simple_ty_map.len()));
                    self.simple_ty_map.insert(ty.clone(), ident);
                }
                let ident = self.simple_ty_map[&ty].clone();
                ast_ty.value = DataTypeKind::HierName(
                    Some(Scope::Name(None, Box::new(Ident::new_unspanned("global_types".to_owned())))),
                    ident,
                    Vec::new(),
                );
            }
        }
        ast_ty
    }

    pub fn reconstruct_val_int(&mut self, ty: &IntTy, val: &LogicVec, span: Span) -> Expr {
        match ty {
            IntTy::SimpleVec(_, false, _) => {
                self.reconstruct_const(val, span)
            }
            _ => {
                let ty = self.reconstruct_ty_simple(&Ty::Int(ty.clone()));
                let val = self.reconstruct_const(val, span);
                Spanned::new(ExprKind::TypeCast(
                    Box::new(Spanned::new(ExprKind::Type(Box::new(ty)), span)),
                    Box::new(val)
                ), span)
            }
        }
    }

    pub fn reconstruct_val(&mut self, ty: &Ty, val: &Val, span: Span) -> Expr {
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

    pub fn reconstruct_dim(&mut self, dim: &expr::Dim) -> ast::Dim {
        let kind = match dim.value {
            expr::DimKind::Value(ref expr) => {
                ast::DimKind::Value(Box::new(self.reconstruct_expr(expr)))
            }
            expr::DimKind::Range(ub, lb) => {
                let ast_ub = reconstruct_i32(ub);
                let ast_lb = reconstruct_i32(lb);
                ast::DimKind::Range(Box::new(ast_ub), Box::new(ast_lb))
            }
            expr::DimKind::PlusRange(ref expr, width) => {
                let ast_expr = self.reconstruct_expr(expr);
                let ast_width = reconstruct_usize(width);
                ast::DimKind::PlusRange(Box::new(ast_expr), Box::new(ast_width))
            }
            expr::DimKind::MinusRange(ref expr, width) => {
                let ast_expr = self.reconstruct_expr(expr);
                let ast_width = reconstruct_usize(width);
                ast::DimKind::MinusRange(Box::new(ast_expr), Box::new(ast_width))
            }
        };
        Spanned::new(kind, dim.span)
    }

    pub fn reconstruct_expr(&mut self, expr: &expr::Expr) -> ast::Expr {
        let kind = match expr.value {
            expr::ExprKind::Const(ref val) => {
                self.reconstruct_val(&expr.ty, val, expr.span).value
            }
            expr::ExprKind::HierName(ref name) => {
                ast::ExprKind::HierName(name.clone())
            }
            expr::ExprKind::EmptyQueue => unimplemented!(),
            expr::ExprKind::Concat(ref list) => {
                let ast_list = list.iter().map(|expr| self.reconstruct_expr(expr)).collect();
                ast::ExprKind::Concat(ast_list, None)
            }
            expr::ExprKind::MultConcat(mul, ref subexpr) => {
                let ast_mul = reconstruct_usize(mul);
                let ast_subexpr = self.reconstruct_expr(subexpr);
                ast::ExprKind::MultConcat(Box::new(ast_mul), Box::new(ast_subexpr), None)
            }
            expr::ExprKind::AssignPattern(ref ty, ref pattern) => {
                let ast_ty = Some(Box::new(self.reconstruct_ty_simple(ty)));
                let ast_pattern = match pattern {
                    expr::AssignPattern::Simple(list) => {
                        ast::AssignPattern::Simple(
                            list.iter().map(|item| self.reconstruct_expr(item)).collect()
                        )
                    }
                    _ => unimplemented!(),
                };
                ast::ExprKind::AssignPattern(ast_ty, ast_pattern)
            }
            expr::ExprKind::Select(ref parent, ref dim) => {
                let ast_parent = self.reconstruct_expr(parent);
                let ast_dim = self.reconstruct_dim(dim);
                match ast_parent.value {
                    ast::ExprKind::HierName(name) => {
                        let spanned = Spanned::new(name, ast_parent.span);
                        ast::ExprKind::HierName(HierId::Select(Box::new(spanned), Box::new(ast_dim)))
                    }
                    _ => unimplemented!(),
                }
            }
            expr::ExprKind::Member(ref parent, ref name) => {
                let ast_parent = self.reconstruct_expr(parent);
                match ast_parent.value {
                    ast::ExprKind::HierName(id) => {
                        let spanned = Spanned::new(id, ast_parent.span);
                        ast::ExprKind::HierName(HierId::Member(Box::new(spanned), Box::new(name.clone())))
                    }
                    _ => unimplemented!(),
                }
            }
            expr::ExprKind::SysTfCall(ref name, ref args) => {
                let args = args
                    .iter()
                    .map(|arg| arg.as_ref().map(|arg| Box::new(self.reconstruct_expr(arg))))
                    .collect();

                ast::ExprKind::SysTfCall(Box::new(SysTfCall {
                    task: Spanned::clone(name),
                    args: Some(Args {
                        ordered: args,
                        named: Vec::new(),
                    })
                }))
            }
            expr::ExprKind::FuncCall { ref expr, ref args } => {
                ast::ExprKind::FuncCall {
                    expr: expr.clone(),
                    attr: None,
                    args: args.clone(),
                }
            }
            expr::ExprKind::ConstCast(..) => unimplemented!(),
            expr::ExprKind::TypeCast(ref ty, ref rhs) => {
                let ast_ty = self.reconstruct_ty_simple(ty);
                let ast_ty_expr = Spanned::new(ast::ExprKind::Type(Box::new(ast_ty)), Span::none());
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::TypeCast(Box::new(ast_ty_expr), Box::new(ast_rhs))
            }
            expr::ExprKind::SignCast(sign, ref rhs) => {
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::SignCast(if sign { Signing::Signed } else { Signing::Unsigned }, Box::new(ast_rhs))
            }
            expr::ExprKind::WidthCast(width, ref rhs) => {
                let ast_ty = reconstruct_usize(width);
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::TypeCast(Box::new(ast_ty), Box::new(ast_rhs))
            }
            expr::ExprKind::Unary(op, ref expr) => {
                ast::ExprKind::Unary(op, None, Box::new(self.reconstruct_expr(expr)))
            }
            expr::ExprKind::Binary(ref lhs, op, ref rhs) => {
                let ast_lhs = self.reconstruct_expr(lhs);
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::Binary(Box::new(ast_lhs), op, None, Box::new(ast_rhs))
            }
            expr::ExprKind::PrefixIncDec(..) => unimplemented!(),
            expr::ExprKind::PostfixIncDec(ref lhs, op) => {
                ast::ExprKind::PostfixIncDec(Box::new(self.reconstruct_expr(lhs)), None, op)
            }
            expr::ExprKind::Assign(ref lhs, ref rhs) => {
                let ast_lhs = self.reconstruct_expr(lhs);
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::Assign(Box::new(ast_lhs), Box::new(ast_rhs))
            }
            expr::ExprKind::NonblockAssign(ref lhs, ref rhs) => {
                let ast_lhs = self.reconstruct_expr(lhs);
                let ast_rhs = self.reconstruct_expr(rhs);
                ast::ExprKind::NonblockAssign(Box::new(ast_lhs), Box::new(ast_rhs))
            }
            expr::ExprKind::BinaryAssign(..) => unimplemented!(),
            expr::ExprKind::Paren(ref expr) => {
                let ast_expr = self.reconstruct_expr(expr);
                ast::ExprKind::Paren(Box::new(ast_expr))
            }
            expr::ExprKind::MinTypMax(..) => unimplemented!(),
            expr::ExprKind::Cond(ref cond, ref t, ref f) => {
                let ast_cond = self.reconstruct_expr(cond);
                let ast_t = self.reconstruct_expr(t);
                let ast_f = self.reconstruct_expr(f);
                ast::ExprKind::Cond(Box::new(ast_cond), None, Box::new(ast_t), Box::new(ast_f))
            }
        };
        Spanned::new(kind, expr.span)
    }

    pub fn reconstruct_stmt(&mut self, stmt: &expr::Stmt) -> ast::Stmt {
        let kind = match &stmt.value {
            expr::StmtKind::Empty => ast::StmtKind::Empty,
            expr::StmtKind::TimingCtrl(ctrl, stmt) => ast::StmtKind::TimingCtrl(
                ctrl.clone(),
                Box::new(self.reconstruct_stmt(stmt))
            ),
            expr::StmtKind::If { uniq, cond, success, failure } => {
                let cond = self.reconstruct_expr(cond);
                let t = self.reconstruct_stmt(success);
                let f = failure.as_ref().map(|f| self.reconstruct_stmt(f));
                ast::StmtKind::If(*uniq, Box::new(cond), Box::new(t), f.map(Box::new))
            },
            expr::StmtKind::Case { uniq, kw, expr, items } => {
                let expr = self.reconstruct_expr(expr);
                let items = items.iter().map(|(conds, stmt)| {
                    let conds = conds.iter().map(|cond| self.reconstruct_expr(cond)).collect();
                    let stmt = self.reconstruct_stmt(stmt);
                    (conds, stmt)
                }).collect();
                ast::StmtKind::Case {
                    uniq: *uniq,
                    kw: *kw,
                    expr: Box::new(expr),
                    items,
                }
            },
            expr::StmtKind::For { ty, init, cond, update, body } => {
                let ty = ty.as_ref().map(|ty| self.reconstruct_ty_simple(ty));
                let init = init.iter().map(|expr| self.reconstruct_expr(expr)).collect();
                let cond = cond.as_ref().map(|expr| Box::new(self.reconstruct_expr(expr)));
                let update = update.iter().map(|expr| self.reconstruct_expr(expr)).collect();
                let body = Box::new(self.reconstruct_stmt(body));
                ast::StmtKind::For {
                    ty: ty.map(Box::new),
                    init, cond, update, body
                }
            },
            expr::StmtKind::Assert { kind, expr, success, failure } => {
                let expr = Box::new(self.reconstruct_expr(expr));
                let success = success.as_ref().map(|stmt| Box::new(self.reconstruct_stmt(stmt)));
                let failure = failure.as_ref().map(|stmt| Box::new(self.reconstruct_stmt(stmt)));
                ast::StmtKind::Assert {
                    kind: *kind,
                    expr,
                    success,
                    failure,
                }
            },
            expr::StmtKind::SeqBlock(list) => {
                let list = list.iter().map(|stmt| self.reconstruct_stmt(stmt)).collect();
                ast::StmtKind::SeqBlock(list)
            }
            expr::StmtKind::Expr(expr) => {
                let expr = self.reconstruct_expr(expr);
                ast::StmtKind::Expr(Box::new(expr))
            }
            expr::StmtKind::DataDecl(_decl) => {
                unimplemented!()
            }
        };
        ast::Stmt {
            label: stmt.label.clone(),
            attr: None,
            value: kind,
        }
    }

    pub fn reconstruct_item(&mut self, item: &HierItem, list: &mut Vec<Item>) {
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
            HierItem::DataDecl(decl) => {
                let (ty, dim) = self.reconstruct_ty(&decl.ty, Span::none());
                let init = decl.init.as_ref().map(|expr| Box::new(self.reconstruct_expr(expr)));
                list.push(Item::DataDecl(Box::new(DataDecl {
                    attr: None,
                    has_const: false,
                    lifetime: decl.lifetime,
                    ty,
                    list: vec![DeclAssign {
                        name: decl.name.clone(),
                        dim,
                        init: init,
                    }]
                })));
            }
            HierItem::FuncDecl(decl) => {
                let (ty, dim) = self.reconstruct_ty(&decl.ty, Span::none());
                assert!(dim.is_empty());
                list.push(Item::FuncDecl(Box::new(FuncDecl {
                    attr: None,
                    lifetime: decl.lifetime,
                    ty,
                    name: decl.name.clone(),
                    ports: decl.ports.clone(),
                    stmts: decl.stmts.clone(),
                })));
            }
            HierItem::TaskDecl(decl) => {
                list.push(Item::TaskDecl(Box::new(TaskDecl {
                    attr: None,
                    lifetime: decl.lifetime,
                    name: decl.name.clone(),
                    ports: decl.ports.clone(),
                    stmts: decl.stmts.clone(),
                })));
            }
            HierItem::ContinuousAssign(expr) => {
                list.push(Item::ContinuousAssign(vec![self.reconstruct_expr(expr)]));
            }
            HierItem::Always(kw, stmt) => {
                list.push(Item::Always(*kw, Box::new(self.reconstruct_stmt(stmt))));
            }
            HierItem::Design(decl) => {
                // Reconstruct all instantiations and display them here
                for (_, inst) in decl.instances.borrow().iter() {
                    list.push(self.reconstruct_instantiation(decl, inst));
                }
            }
            HierItem::Instance(decl) => {
                list.push(Item::HierInstantiation(Box::new(HierInstantiation {
                    attr: None,
                    name: decl.inst.get_instance().name.clone(),
                    param: None,
                        inst: vec![HierInst {
                        name: decl.name.clone(),
                        dim: decl.dim.iter().map(|(ub, lb)| {
                            let ub_expr = reconstruct_i32(*ub);
                            let lb_expr = reconstruct_i32(*lb);
                            Spanned::new(DimKind::Range(Box::new(ub_expr), Box::new(lb_expr)), Span::none())
                        }).collect(),
                        ports: PortConn::Ordered(decl.port.iter().map(|port| {
                            (None, port.as_ref().map(|port| Box::new(self.reconstruct_expr(port))))
                        }).collect()),
                    }]
                })))
            }
            HierItem::Other(item) => list.push(Item::clone(item)),
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
                            name: genblk.name.clone().map(Box::new),
                            items: sublist,
                        })
                    }],
                    else_block: None,
                })));
            }
            HierItem::LoopGenBlock(_) => unreachable!(),
            HierItem::Modport(modport) => {
                let decl_list = modport.scope.items.iter().map(|item| {
                    let item = match item {
                        HierItem::DataPort(decl) => decl,
                        _ => unreachable!(),
                    };
                    ModportPortDecl::Simple(
                        None,
                        item.dir,
                        vec![ModportSimplePort::Named(item.name.clone())]
                    )
                }).collect();
                list.push(Item::ModportDecl(None, vec![(modport.name.clone(), decl_list)]));
            },
            HierItem::Enum(enu, index) => {
                let enum_index = self.source.enums.iter().position(|x| x == enu).unwrap();
                let element_name = enu.elements.borrow()[*index].0.clone();
                let expr = Box::new(Spanned::new_unspanned(ExprKind::HierName(
                    HierId::Name(
                        Some(Scope::Name(None, Box::new(Ident::new_unspanned("global_types".to_owned())))),
                        Box::new(Ident::new_unspanned(format!("enum_{}_{}", enum_index, element_name)))
                    )
                )));

                list.push(Item::ParamDecl(Box::new(ParamDecl {
                    kw: Keyword::Parameter,
                    ty: None,
                    list: vec![DeclAssign {
                        name: element_name,
                        dim: Vec::new(),
                        init: Some(expr),
                    }]
                })));
            }
            v => unimplemented!("{:?}", std::mem::discriminant(v)),
        }
    }

    pub fn reconstruct_instantiation(&mut self, decl: &hier::DesignDecl, inst: &hier::DesignInstantiation) -> Item {
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
                    let intf = Some(Box::new(decl.inst.get_instance().name.clone()));
                    let modport = decl.modport.as_ref().map(|modport| Box::new(modport.name.clone()));
                    let dim = decl.dim.iter().map(|(ub, lb)| {
                        let ub = reconstruct_i32(*ub);
                        let lb = reconstruct_i32(*lb);
                        Spanned::new_unspanned(DimKind::Range(Box::new(ub), Box::new(lb)))
                    }).collect();
                    ports.push(PortDecl::Interface(intf, modport, vec![DeclAssign {
                        name: decl.name.clone(),
                        dim,
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
            kw: decl.ast.kw,
            lifetime: decl.ast.lifetime,
            name: inst.name.clone(),
            pkg_import: Vec::new(),
            param: if !params.is_empty() { Some(params) } else { None },
            port: ports,
            items: list,
        }))
    }

    pub fn reconstruct(&mut self) -> Vec<Vec<Item>> {
        // Packages
        let mut list: Vec<_> = self.source.pkgs.iter().map(|(_, decl)| {
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
        }).collect();

        let mut units = Vec::new();
        for unit in &self.source.units { 
            let mut list = Vec::new();
            for item in &unit.items {
                self.reconstruct_item(item, &mut list);
            }
            units.push(list);
        }

        // Build global types package, insert before all packages
        list.insert(0, {
            self.global_qualify = false;
            let mut types = Vec::new();
            types.extend(self.source.enums.iter().enumerate().map(|(index, enu)| {
                let prefix = format!("enum_{}", index);
                let enu = self.reconstruct_enum(enu, &prefix);
                let ty = Spanned::new(DataTypeKind::Enum(enu, Vec::new()), Span::none());
                let name = Ident::new(prefix, Span::none());
                Item::Typedef(None, Box::new(ty), Box::new(name), Vec::new())
            }));
            types.extend(self.source.structs.iter().enumerate().map(|(index, struc)| {
                let struc = self.reconstruct_struct(struc);
                let ty = Spanned::new(DataTypeKind::Aggr(struc, Vec::new()), Span::none());
                let name = Ident::new(format!("struct_{}", index), Span::none());
                Item::Typedef(None, Box::new(ty), Box::new(name), Vec::new())
            }));
            types.extend(self.simple_ty_map.iter().map(|(ty, name)| {
                let (ty, dim) = self.reconstruct_ty(ty, Span::none());
                Item::Typedef(None, Box::new(ty), Box::new(name.clone()), dim)
            }));
            Item::PkgDecl(Box::new(PkgDecl {
                attr: None,
                lifetime: Lifetime::Static,
                name: Ident::new_unspanned("global_types".to_owned()),
                items: types
            }))
        });
        units.insert(0, list);

        units
    }
}
