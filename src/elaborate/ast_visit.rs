use syntax::ast::*;

pub trait AstVisitor {
    fn do_visit_item(&mut self, item: &mut Item) {
        match item {
            Item::DesignDecl(decl) => {
                if let Some(param) = &mut decl.param {
                    for decl in param {
                        self.visit_param_decl(decl)
                    }
                }
                for port in &mut decl.port {
                    self.visit_port_decl(port)
                }
                for item in &mut decl.items {
                    self.visit_item(item)
                }
            }
            Item::PkgDecl(decl) => {
                for item in &mut decl.items {
                    self.visit_item(item)
                }
            }
            Item::PkgImport(_) => (),
            Item::ParamDecl(decl) => {
                self.visit_param_decl(decl)
            }
            Item::DataDecl(decl) => {
                self.visit_ty(&mut decl.ty);
                for assign in &mut decl.list {
                    for dim in &mut assign.dim {
                        self.visit_dim(dim);
                    }
                    if let Some(v) = &mut assign.init {
                        self.visit_expr(v);
                    }
                }
            }
            Item::Typedef(_, ty, _, dim) => {
                self.visit_ty(ty);
                for dim in dim {
                    self.visit_dim(dim);
                }
            }
            Item::TypedefIntf(_, expr, _, _) => self.visit_expr(expr),
            Item::ContinuousAssign(list) => {
                for expr in list {
                    self.visit_expr(expr)
                }
            }
            Item::Initial(stmt) => self.visit_stmt(stmt),
            Item::Always(_, stmt) => self.visit_stmt(stmt),
            Item::HierInstantiation(inst) => {
                if let Some(param) = &mut inst.param {
                    self.visit_args(param);
                }
                for single_inst in &mut inst.inst {
                    for dim in &mut single_inst.dim {
                        self.visit_dim(dim);
                    }
                    self.visit_args(&mut single_inst.ports);
                }
            }
            Item::GenRegion(item) => {
                for item in item {
                    self.visit_item(item);
                }
            }
            Item::LoopGen(gen) => {
                self.visit_expr(&mut gen.init);
                self.visit_expr(&mut gen.cond);
                self.visit_expr(&mut gen.update);
                for item in &mut gen.block.items {
                    self.visit_item(item);
                }
            }
            Item::IfGen(gen) => {
                for (cond, block) in &mut gen.if_block {
                    self.visit_expr(cond);
                    for item in &mut block.items {
                        self.visit_item(item);
                    }
                }
                if let Some(v) = &mut gen.else_block {
                    for item in &mut v.items {
                        self.visit_item(item);
                    }
                }
            }
            Item::SysTfCall(call) => self.visit_sys_tf_call(call),
            Item::ModportDecl(_, decl) => {
                for decl in decl {
                    for item in &mut decl.1 {
                        match item {
                            ModportPortDecl::Simple(_, _, list) => {
                                for item in list {
                                    match item {
                                        ModportSimplePort::Named(_name) => (),
                                        ModportSimplePort::Explicit(_name, expr) => {
                                            self.visit_expr(expr);
                                        }
                                    }
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
            }
        }
    }

    fn visit_item(&mut self, item: &mut Item) {
        self.do_visit_item(item);
    }

    fn visit_param_decl(&mut self, decl: &mut ParamDecl) {
        if let Some(ty) = &mut decl.ty {
            self.visit_ty(ty);
        }
        for item in &mut decl.list {
            for dim in &mut item.dim {
                self.visit_dim(dim);
            }
            if let Some(v) = &mut item.init {
                self.visit_expr(v);
            }
        }
    }

    fn visit_port_decl(&mut self, obj: &mut PortDecl) {
        match obj {
            PortDecl::Data(_, _, ty, list) => {
                self.visit_ty(ty);
                for item in list {
                    for dim in &mut item.dim {
                        self.visit_dim(dim);
                    }
                    if let Some(v) = &mut item.init {
                        self.visit_expr(v);
                    }
                }
            }
            PortDecl::Interface(_, _, list) => {
                for item in list {
                    for dim in &mut item.dim {
                        self.visit_dim(dim);
                    }
                    if let Some(v) = &mut item.init {
                        self.visit_expr(v);
                    }
                }
            }
            _ => unimplemented!(),
        }
    }
    
    fn visit_dim(&mut self, dim: &mut Dim) {
        match &mut dim.value {
            DimKind::Value(expr) => self.visit_expr(expr),
            DimKind::Range(ub, lb) |
            DimKind::PlusRange(ub, lb) |
            DimKind::MinusRange(ub, lb) => {
                self.visit_expr(ub);
                self.visit_expr(lb);
            }
            _ => unimplemented!(),
        }
    }

    fn visit_args(&mut self, arg: &mut Args) {
        for (_, expr) in &mut arg.ordered {
            if let Some(v) = expr { self.visit_expr(v); }
        }
        for (_, _, expr) in &mut arg.named {
            if let Some(v) = expr { self.visit_expr(v); }
        }
    }

    fn visit_enum(&mut self, en: &mut EnumDecl) {
        if let Some(v) = &mut en.ty {
            self.visit_ty(v);
        }
        for assign in &mut en.members {
            if let Some(v) = &mut assign.init {
                self.visit_expr(v);
            }
        }
    }

    fn do_visit_ty(&mut self, ty: &mut DataType) {
        match &mut ty.value {
            DataTypeKind::Type |
            DataTypeKind::IntAtom(..) |
            DataTypeKind::Real(_) => (),
            DataTypeKind::Implicit(_, dim) |
            DataTypeKind::IntVec(_, _, dim) => {
                for dim in dim {
                    self.visit_dim(dim);
                }
            }
            DataTypeKind::Aggr(aggr, dim) => {
                for member in &mut aggr.members {
                    self.visit_ty(&mut member.ty);
                    for assign in &mut member.list {
                        if let Some(v) = &mut assign.init {
                            self.visit_expr(v);
                        }
                    }
                }
                for dim in dim {
                    self.visit_dim(dim);
                }
            }
            DataTypeKind::Enum(en, dim) => {
                self.visit_enum(en);
                for dim in dim {
                    self.visit_dim(dim);
                }
            }
            DataTypeKind::String |
            DataTypeKind::Chandle => (),
            // VirtualInterface, // TODO
            DataTypeKind::Event => (),
            DataTypeKind::HierName(_, _, dim) => {
                for dim in dim {
                    self.visit_dim(dim);
                }
            }
            DataTypeKind::TypeRef(expr) => self.visit_expr(expr),
            DataTypeKind::Void => (),
            _ => unimplemented!(),
        }
    }

    fn visit_ty(&mut self, ty: &mut DataType) {
        self.do_visit_ty(ty);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.value {
            StmtKind::Empty => (),
            StmtKind::TimingCtrl(timing, substmt) => {
                match timing {
                    TimingCtrl::DelayCtrl(expr) => self.visit_expr(expr),
                    TimingCtrl::ExprEventCtrl(_expr) => (), // TODO
                    TimingCtrl::NameEventCtrl(..) => (),
                    TimingCtrl::ImplicitEventCtrl => (),
                }
                self.visit_stmt(substmt);
            }
            StmtKind::If(_, cond, tst, fst) => {
                self.visit_expr(cond);
                self.visit_stmt(tst);
                if let Some(v) = fst { self.visit_stmt(v); }
            }
            StmtKind::SeqBlock(stmts) => {
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
            }
            StmtKind::Expr(expr) => {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_sys_tf_call(&mut self, call: &mut SysTfCall) {
        if let Some(v) = &mut call.args {
            self.visit_args(v);
        }
    }

    fn do_visit_expr(&mut self, expr: &mut Expr) {
        match &mut expr.value {
            ExprKind::Type(ty) => self.visit_ty(ty),
            ExprKind::Literal(_) |
            ExprKind::HierName(..) => (),
            // EmptyQueue,
            ExprKind::Concat(list) => {
                for expr in list { self.visit_expr(expr); }
            }
            ExprKind::MultConcat(mul, list) => {
                self.visit_expr(mul);
                self.visit_expr(list);
            }
            ExprKind::AssignPattern(ty, pattern) => {
                if let Some(v) = ty {
                    self.visit_ty(v);
                }
                match pattern {
                    AssignPattern::Simple(list) => {
                        for expr in list { self.visit_expr(expr); }
                    }
                    AssignPattern::Keyed(list) => {
                        for (key, expr) in list {
                            // TODO: How to deal with key propertly?
                            self.visit_expr(key);
                            self.visit_expr(expr);
                        }
                    }
                    AssignPattern::Mult(mul, list) => {
                        self.visit_expr(mul);
                        for expr in list { self.visit_expr(expr); }
                    }
                }
            }
            ExprKind::Select(expr, dim) => {
                self.visit_expr(expr);
                self.visit_dim(dim);
            }
            ExprKind::Member(expr, _) => self.visit_expr(expr),
            ExprKind::SysTfCall(call) => self.visit_sys_tf_call(call),
            // ConstCast(Box<Expr>),
            // SignCast(Signing, Box<Expr>),
            ExprKind::TypeCast(target, expr) => {
                self.visit_expr(target);
                self.visit_expr(expr);
            }
            ExprKind::Unary(_, _, expr) => {
                self.visit_expr(expr);
            }
            ExprKind::Binary(lhs, _, _, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::PrefixIncDec(_, _, expr) => self.visit_expr(expr),
            ExprKind::PostfixIncDec(expr, _, _) => self.visit_expr(expr),
            ExprKind::Assign(lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            // BinaryAssign(Box<Expr>, BinaryOp, Box<Expr>),
            ExprKind::Paren(expr) => self.visit_expr(expr),
            // MinTypMax(Box<Expr>, Box<Expr>, Box<Expr>),
            ExprKind::Cond(cond, _, t, f) => {
                self.visit_expr(cond);
                self.visit_expr(t);
                self.visit_expr(f);
            }
            v => {
                eprintln!("{:?}", v);
                unimplemented!();
            }
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        self.do_visit_expr(expr);
    }
}