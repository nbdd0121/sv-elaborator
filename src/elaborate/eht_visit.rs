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
            ExprKind::SysTfCall(..) => unimplemented!(),
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
            ExprKind::PostfixIncDec(..) => unimplemented!(),
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
}
