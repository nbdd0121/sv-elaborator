use std::rc::Rc;

use super::expr::*;

pub trait EhtVisitor {
    fn do_visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.value {
            ExprKind::Const(..) => (),
            ExprKind::HierName(_) => (),
            ExprKind::EmptyQueue => (),
            ExprKind::Concat(list) => for item in list { self.visit_expr(item) },
            ExprKind::MultConcat(_, subexpr) => self.visit_expr(subexpr),
            ExprKind::AssignPattern(_, pattern) => {
                match pattern {
                    AssignPattern::Simple(list) => {
                        for item in list {
                            self.visit_expr(item);
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            ExprKind::Select(parent, dim) => {
                self.visit_expr(parent);
                match &mut dim.value {
                    DimKind::Range(..) => (),
                    DimKind::Value(expr) |
                    DimKind::PlusRange(expr, _) |
                    DimKind::MinusRange(expr, _) => self.visit_expr(expr),
                }
            }
            ExprKind::Member(parent, _) => self.visit_expr(parent),
            ExprKind::SysTfCall(_, args) => {
                for arg in args {
                    if let Some(arg) = arg { self.visit_expr(arg) }
                }
            }
            ExprKind::FuncCall { .. } => (),
            ExprKind::ConstCast(..) => unimplemented!(),
            ExprKind::TypeCast(_, rhs) |
            ExprKind::SignCast(_, rhs) |
            ExprKind::WidthCast(_, rhs) => self.visit_expr(rhs),
            ExprKind::Unary(_, expr) => self.visit_expr(expr),
            ExprKind::Binary(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::PrefixIncDec(..) => unimplemented!(),
            ExprKind::PostfixIncDec(lhs, _) => self.visit_expr(lhs),
            ExprKind::Assign(lhs, rhs) |
            ExprKind::NonblockAssign(lhs, rhs) |
            ExprKind::BinaryAssign(lhs, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Paren(expr) => self.visit_expr(expr),
            ExprKind::MinTypMax(..) => unimplemented!(),
            ExprKind::Cond(cond, t, f) => {
                self.visit_expr(cond);
                self.visit_expr(t);
                self.visit_expr(f);
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        self.do_visit_expr(expr);
    }

    fn do_visit_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.value {
            StmtKind::Empty => (),
            StmtKind::TimingCtrl(_ctrl, stmt) => {
                self.visit_stmt(stmt);
            },
            StmtKind::If { cond, success, failure, .. } => {
                self.visit_expr(cond);
                self.visit_stmt(success);
                if let Some(f) = failure { self.visit_stmt(f); }
            },
            StmtKind::Case { expr, items, .. } => {
                self.visit_expr(expr);
                for (conds, stmt) in items {
                    for cond in conds { self.visit_expr(cond); }
                    self.visit_stmt(stmt);
                }
            },
            StmtKind::For { init, cond, update, body, .. } => {
                for expr in init { self.visit_expr(expr); }
                if let Some(expr) = cond { self.visit_expr(expr); }
                for expr in update { self.visit_expr(expr); }
                self.visit_stmt(body);
            },
            StmtKind::Assert { expr, success, failure, .. } => {
                self.visit_expr(expr);
                if let Some(stmt) = success { self.visit_stmt(stmt); }
                if let Some(stmt) = failure { self.visit_stmt(stmt); }
            },
            StmtKind::SeqBlock(list) => {
                for stmt in list {
                    self.visit_stmt(stmt);
                }
            }
            StmtKind::Expr(expr) => {
                self.visit_expr(expr);
            }
            StmtKind::DataDecl(decl) => {
                let decl = Rc::get_mut(decl).unwrap();
                if let Some(expr) = &mut decl.init { self.visit_expr(expr); }
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        self.do_visit_stmt(stmt);
    }
}
