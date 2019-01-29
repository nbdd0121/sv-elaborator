use syntax::ast::*;
use syntax::tokens::{TokenKind, Keyword};

use util::IdentifyFirstLast;

use std::convert::AsRef;

pub struct PrettyPrint {
    output: String,
    indent: String,
}

impl PrettyPrint {
    pub fn new() -> Self {
        PrettyPrint {
            output: String::new(),
            indent: String::new(),
        }
    }

    fn indent(&mut self) {
        self.indent.push_str(&"    ");
    }

    fn unindent(&mut self) {
        let newlen = self.indent.len() - 4;
        self.indent.truncate(newlen)
    }

    pub fn append(&mut self, str: impl AsRef<str>) {
        self.output.push_str(str.as_ref());
    }

    fn indent_append(&mut self, str: impl AsRef<str>) {
        self.output.push_str(&self.indent);
        self.append(str);
    }

    pub fn take(self) -> String {
        self.output
    }

    /// Print a list of elements, separated by newline. Each item should assume it is inlined,
    /// i.e. it does not need to indent itself and shouldn't append newline
    fn print_comma_list_newline<T>(&mut self, obj: &Vec<T>, mut f: impl FnMut(&mut Self, &T)) {
        for (item, _, last) in obj.iter().identify_first_last() {
            self.indent_append("");
            f(self, item);
            if !last {
                self.append(",");
            }
            self.append("\n");
        }
    }

    fn print_sep_list<T>(&mut self, sep: &'static str, obj: &Vec<T>, mut f: impl FnMut(&mut Self, &T)) {
        for (item, _, last) in obj.iter().identify_first_last() {
            f(self, item);
            if !last {
                self.append(sep);
            }
        }
    }

    fn print_comma_list<T>(&mut self, obj: &Vec<T>, mut f: impl FnMut(&mut Self, &T)) {
        for (item, _, last) in obj.iter().identify_first_last() {
            f(self, item);
            if !last {
                self.append(", ");
            }
        }
    }

    fn print_dim(&mut self, obj: &Dim) {
        self.append("[");
        match &**obj {
            DimKind::Value(rhs) => {
                self.print_expr(rhs);
            }
            DimKind::Range(start, end) => {
                self.print_expr(start);
                self.append(":");
                self.print_expr(end);
            }
            DimKind::PlusRange(start, end) => {
                self.print_expr(start);
                self.append("+:");
                self.print_expr(end);
            }
            DimKind::MinusRange(start, end) => {
                self.print_expr(start);
                self.append("-:");
                self.print_expr(end);
            }
            DimKind::Unsized => (),
            DimKind::AssocWild => {
                self.append("*");
            }
        }
        self.append("]");
    }

    fn print_decl_assign(&mut self, obj: &DeclAssign) {
        self.append(format!("{}", obj.name));
        for dim in &obj.dim {
            self.print_dim(dim);
        }
        if let Some(ref v) = obj.init {
            self.append(" = ");
            self.print_expr(v);
        }
    }

    fn print_param_decl(&mut self, obj: &ParamDecl) {
        self.indent_append(format!("{} ", if obj.kw == Keyword::Parameter { "parameter" } else { "localparam" }));
        if let Some(ty) = &obj.ty {
            self.print_type(ty);
            self.append(" ");
        }
        for (item, _, last) in obj.list.iter().identify_first_last() {
            self.print_decl_assign(item);
            if !last {
                self.append(format!(", "));
            }
        }
    }

    fn print_port_decl(&mut self, obj: &PortDecl) {
        match obj {
            PortDecl::Data(dir, net, ty, list) => {
                self.indent_append(format!("{} ", dir));
                match net {
                    NetPortType::Builtin(netty) => self.append(format!("{} ", netty)),
                    NetPortType::Variable => self.append("var "),
                    NetPortType::Default => (),
                    _ => unimplemented!(),
                }
                self.print_type(&ty);
                self.append(" ");
                for (item, _, last) in list.iter().identify_first_last() {
                    self.append(format!("{}", item.name));
                    for dim in &item.dim {
                        self.print_dim(dim);
                    }
                    if let Some(ref v) = item.init {
                        self.append(" = ");
                        self.print_expr(v);
                    }
                    if !last {
                        self.append(format!(", "));
                    }
                }
            }
            PortDecl::Interface(intf, modport, list) => {
                if let Some(intf) = intf {
                    self.indent_append(format!("{}", intf));
                } else {
                    self.indent_append("interface");
                }
                if let Some(modport) = modport {
                    self.append(format!(".{}", modport));
                }
                for (item, _, last) in list.iter().identify_first_last() {
                    self.append(format!(" {}", item.name));
                    for dim in &item.dim {
                        self.print_dim(dim);
                    }
                    if let Some(ref v) = item.init {
                        self.append(" = ");
                        self.print_expr(v);
                    }
                    if !last {
                        self.append(format!(","));
                    }
                }
            }
            _ => {
                // TODO
                self.append("TODO");
            }
        }
    }

    fn print_module_decl(&mut self, obj: &DesignDecl) {
        self.append(format!("{}", obj.kw));
        self.indent();
        if obj.lifetime == Lifetime::Automatic {
            self.append(format!(" automatic"));
        }
        self.append(format!(" {}", obj.name));
        for items in &obj.pkg_import {
            self.append("\nimport ");
            self.print_comma_list(items, |this, PkgImportItem(pkg, item)| {
                this.append(format!("{}", pkg));
                this.append("::");
                match item {
                    None => this.append("*"),
                    Some(v) => this.append(format!("{}", v)),
                }
            });
            self.append(";");
        }
        if let Some(ref v) = obj.param {
            self.append(format!(" #(\n"));
            for (item, _, last) in v.iter().identify_first_last() {
                self.print_param_decl(item);
                self.append(format!("{}\n", if { last } { "" } else { "," }));
            }
            self.append(format!(")"));
        }
        if obj.port.len() != 0 {
            self.append(format!(" (\n"));
            for (item, _, last) in obj.port.iter().identify_first_last() {
                self.print_port_decl(item);
                self.append(format!("{}\n", if { last } { "" } else { "," }));
            }
            self.append(format!(")"));
        }
        self.append(format!(";\n"));
        for item in &obj.items {
            self.indent_append("");
            self.print_item(item);
            self.append("\n");
        }
        self.unindent();
        self.indent_append(format!("end{}", obj.kw));
    }

    fn print_pkg_decl(&mut self, obj: &PkgDecl) {
        self.append("package");
        self.indent();
        if obj.lifetime == Lifetime::Automatic {
            self.append(" automatic");
        }
        self.append(format!(" {};\n", obj.name));
        for item in &obj.items {
            self.indent_append("");
            self.print_item(item);
            self.append("\n");
        }
        self.unindent();
        self.indent_append("endpackage");
    }

    fn print_func_decl(&mut self, obj: &FuncDecl) {
        self.append("function ");
        if obj.lifetime == Lifetime::Automatic {
            self.append("automatic ");
        }
        self.print_type(&obj.ty);
        self.append(format!(" {}", obj.name));
        if obj.ports.len() != 0 {
            self.append(format!(" (\n"));
            self.indent();
            for (item, _, last) in obj.ports.iter().identify_first_last() {
                self.print_port_decl(item);
                self.append(format!("{}\n", if { last } { "" } else { "," }));
            }
            self.unindent();
            self.indent_append(format!(")"));
        }
        self.append(";\n");
        self.indent();
        for stmt in &obj.stmts {
            self.indent_append("");
            self.print_stmt(stmt);
            self.append("\n");
        }
        self.unindent();
        self.indent_append("endfunction");
    }

    fn print_task_decl(&mut self, obj: &TaskDecl) {
        self.append("task ");
        if obj.lifetime == Lifetime::Automatic {
            self.append("automatic ");
        }
        self.append(format!("{}", obj.name));
        if obj.ports.len() != 0 {
            self.append(format!(" (\n"));
            self.indent();
            for (item, _, last) in obj.ports.iter().identify_first_last() {
                self.print_port_decl(item);
                self.append(format!("{}\n", if { last } { "" } else { "," }));
            }
            self.unindent();
            self.indent_append(format!(")"));
        }
        self.append(";\n");
        self.indent();
        for stmt in &obj.stmts {
            self.indent_append("");
            self.print_stmt(stmt);
            self.append("\n");
        }
        self.unindent();
        self.indent_append("endtask");
    }

    fn print_data_decl(&mut self, obj: &DataDecl) {
        if obj.has_const {
            self.append("const ");
        }
        if let DataTypeKind::Implicit(..) = *obj.ty {
            self.append("var ");
        }
        self.print_type(&obj.ty);
        self.append(" ");
        for (item, _, last) in obj.list.iter().identify_first_last() {
            self.print_decl_assign(item);
            if !last {
                self.append(format!(", "));
            }
        }
        self.append(";");
    }

    fn print_hier_instantiation(&mut self, obj: &HierInstantiation) {
        self.append(format!("{}", obj.name));
        if let Some(v) = &obj.param {
            self.append(" #");
            self.print_args(v, false);
        }
        for (item, first, _) in obj.inst.iter().identify_first_last() {
            if !first {
                self.append(",");
            }
            self.append(format!(" {}", item.name));
            for dim in &item.dim {
                self.print_dim(dim);
            }
            self.append(" (\n");
            self.indent();
            match item.ports {
                PortConn::Ordered(ref list) => {
                    for ((_, v), _, last) in list.iter().identify_first_last() {
                        self.indent_append("");
                        if let Some(v) = v {
                            self.print_expr(v);
                        }
                        if !last {
                            self.append(",\n");
                        }
                    }
                }
                PortConn::Named(ref list) => {
                    for ((_, conn), _, last) in list.iter().identify_first_last() {
                        match conn {
                            NamedPortConn::Explicit(name, expr) => {
                                self.indent_append(format!(".{}(", name));
                                if let Some(v) = expr {
                                    self.print_expr(v);
                                }
                                self.append(")");
                            }
                            NamedPortConn::Implicit(name) => {
                                self.indent_append(format!(".{}", name));
                            }
                            NamedPortConn::Wildcard => {
                                self.indent_append(".*");
                            }
                        }
                        if !last {
                            self.append(",\n");
                        }
                    }
                }
            }
            self.append("\n");
            self.unindent();
            self.indent_append(")");
        }
        self.append(";");
    }

    fn print_args(&mut self, obj: &Args, inline: bool) {
        self.append("(");
        if !inline {
            self.append("\n");
            self.indent();
        }
        for (v, _, last) in obj.ordered.iter().identify_first_last() {
            if !inline { self.indent_append(""); }
            if let Some(v) = v {
                self.print_expr(v);
            }
            if !last || !obj.named.is_empty() {
                self.append(if inline { ", " } else { ",\n" });
            }
        }
        for ((id, v), _, last) in obj.named.iter().identify_first_last() {
            if !inline { self.indent_append(""); }
            self.append(format!(".{}(", id));
            if let Some(v) = v {
                self.print_expr(v);
            }
            self.append(")");
            if !last {
                self.append(if inline { ", " } else { ",\n" });
            }
        }
        if !inline {
            self.append("\n");
            self.unindent();
            self.indent_append("");
        }
        self.append(")");
    }

    pub fn print_item(&mut self, obj: &Item) {
        match obj {
            Item::DesignDecl(v) => self.print_module_decl(v),
            Item::PkgDecl(v) => self.print_pkg_decl(v),
            Item::FuncDecl(v) => self.print_func_decl(v),
            Item::TaskDecl(v) => self.print_task_decl(v),
            Item::PkgImport(items) => {
                self.append("import ");
                self.print_comma_list(items, |this, PkgImportItem(pkg, item)| {
                    this.append(format!("{}", pkg));
                    this.append("::");
                    match item {
                        None => this.append("*"),
                        Some(v) => this.append(format!("{}", v)),
                    }
                });
                self.append(";");
            }
            Item::HierInstantiation(v) => self.print_hier_instantiation(v),
            Item::ContinuousAssign(list) => {
                self.append("assign ");
                for (item, _, last) in list.iter().identify_first_last() {
                    self.print_expr(item);
                    if !last {
                        self.append(", ");
                    }
                }
                self.append(";");
            }
            Item::Initial(stmt) => {
                self.append("initial ");
                self.print_stmt(&stmt);
            }
            Item::Always(kw, stmt) => {
                self.append(format!("{} ", kw));
                self.print_stmt(&stmt);
            }
            Item::GenRegion(list) => {
                self.append("generate\n");
                self.indent();
                for item in list {
                    self.indent_append("");
                    self.print_item(item);
                    self.append("\n");
                }
                self.unindent();
                self.indent_append("endgenerate");
            }
            Item::LoopGen(gen) => {
                self.append("for (");
                if gen.genvar {
                    self.append("genvar ");
                }
                self.append(format!("{} = ", gen.id));
                self.print_expr(&gen.init);
                self.append("; ");
                self.print_expr(&gen.cond);
                self.append("; ");
                self.print_expr(&gen.update);
                self.append(") ");
                self.print_gen_block(&gen.block);
            }
            Item::IfGen(gen) => {
                for ((cond, block), first, _) in gen.if_block.iter().identify_first_last() {
                    if !first {
                        self.append(" else ");
                    }
                    self.append("if (");
                    self.print_expr(cond);
                    self.append(") ");
                    self.print_gen_block(block);
                }
                if let Some(v) = &gen.else_block {
                    self.append(" else ");
                    self.print_gen_block(&v);
                }
            }
            Item::SysTfCall(tf) => {
                self.print_sys_tf_call(tf);
                self.append(";");
            }
            Item::ParamDecl(decl) => {
                self.append(format!("{} ", if decl.kw == Keyword::Parameter { "parameter" } else { "localparam" }));
                if let Some(ty) = &decl.ty {
                    self.print_type(ty);
                    self.append(" ");
                }
                for (item, _, last) in decl.list.iter().identify_first_last() {
                    self.print_decl_assign(item);
                    if !last {
                        self.append(format!(", "));
                    }
                }
                self.append(";");
            }
            Item::DataDecl(decl) => self.print_data_decl(decl),
            Item::NetDecl(decl) => {
                self.append(format!("{} ", decl.net));
                self.print_type(&decl.ty);
                self.append(" ");
                for (item, _, last) in decl.list.iter().identify_first_last() {
                    self.print_decl_assign(item);
                    if !last {
                        self.append(format!(", "));
                    }
                }
                self.append(";");
            }
            Item::TypedefIntf(_attr, intf, ty, id) => {
                self.append("typedef ");
                self.print_hier_id(intf);
                self.append(format!(".{} {};", ty, id));
            }
            Item::Typedef(_attr, ty, id, dim) => {
                self.append("typedef ");
                self.print_type(&ty);
                self.append(format!(" {}", id));
                for dim in dim {
                    self.print_dim(dim);
                }
                self.append(";");
            }
            Item::ModportDecl(_attr, decl) => {
                self.append("modport ");
                self.print_comma_list(decl, |this, decl| {
                    this.append(format!("{} (\n", decl.0));
                    this.indent();
                    this.print_comma_list_newline(&decl.1, |this, item| {
                        match item {
                            ModportPortDecl::Simple(_attr, dir, list) => {
                                this.append(format!("{} ", dir));
                                this.print_comma_list(list, |this, item| {
                                    match item {
                                        ModportSimplePort::Named(name) => this.append(format!("{}", name)),
                                        ModportSimplePort::Explicit(name, expr) => {
                                            this.append(format!(".{}(", name));
                                            this.print_expr(expr);
                                            this.append(")");
                                        }
                                    }
                                });
                            }
                            _ => unimplemented!(),
                        }
                    });
                    this.unindent();
                    this.indent_append(")");
                });
                self.append(";");
            }
            Item::Comment(str) => {
                self.indent_append("/* ");
                self.append(str);
                self.append("*/");
            }
        }
    }

    fn print_gen_block(&mut self, obj: &GenBlock) {
        self.append("begin");
        if let Some(v) = &obj.name {
            self.append(format!(": {}", v));
        }
        self.append("\n");
        self.indent();
        for item in &obj.items {
            self.indent_append("");
            self.print_item(item);
            self.append("\n");
        }
        self.unindent();
        self.indent_append("end");
    }

    //
    // Expression and data type
    //

    fn print_type(&mut self, obj: &DataType) {
        match &**obj {
            DataTypeKind::Type => self.append("type"),
            DataTypeKind::IntVec(kw, sign, dim) => {
                self.append(format!("{}", kw));
                if sign == &Signing::Signed {
                    self.append(" signed");
                }
                for dim in dim {
                    self.print_dim(dim);
                }
            }
            DataTypeKind::IntAtom(ty, sign) => {
                self.append(format!("{}", ty));
                if let Some(v) = sign {
                    self.append(format!("{}", v));
                }
            }
            DataTypeKind::String => self.append("string"),
            DataTypeKind::Aggr(aggr, dim) => {
                self.append(format!("{}", aggr.kind));
                if aggr.packed {
                    self.append(" packed");
                }
                if aggr.sign == Signing::Signed {
                    self.append(" signed");
                }
                self.append(" {\n");
                self.indent();
                for member in &aggr.members {
                    self.indent_append("");
                    self.print_type(&member.ty);
                    self.append(" ");
                    self.print_comma_list(&member.list, Self::print_decl_assign);
                    self.append(";\n");
                }
                self.unindent();
                self.indent_append("}");
                for dim in dim {
                    self.print_dim(dim);
                }
            }
            DataTypeKind::Enum(en, dim) => {
                self.append("enum ");
                if let Some(v) = &en.ty {
                    self.print_type(v);
                    self.append(" ");
                }
                self.append("{\n");
                self.indent();
                self.print_comma_list_newline(&en.members, Self::print_decl_assign);
                self.unindent();
                self.indent_append("}");
                for dim in dim {
                    self.print_dim(dim);
                }
            }
            DataTypeKind::Implicit(sign, dim) => {
                if sign == &Signing::Signed {
                    self.append("signed");
                }
                for dim in dim {
                    self.print_dim(dim);
                }
            }
            DataTypeKind::HierName(scope, name, dim) => {
                if let Some(scope) = scope {
                    self.append(Self::get_scope(scope));
                    self.append("::");
                }
                self.append(format!("{}", name));
                for dim in dim {
                    self.print_dim(dim);
                }
            }
            DataTypeKind::TypeRef(expr) => {
                self.append("type(");
                self.print_expr(expr);
                self.append(")");
            }
            _ => {
                eprintln!("{:?}", obj);
                unimplemented!()
            }
        }
    }

    fn get_scope(obj: &Scope) -> String {
        match obj {
            Scope::Unit => "$unit".to_owned(),
            Scope::Local => "local".to_owned(),
            Scope::Name(parent, this) => {
                if let Some(parent) = parent {
                    format!("{}::{}", Self::get_scope(parent), this)
                } else {
                    this.to_string()
                }
            }
        }
    }

    fn print_hier_id(&mut self, obj: &HierId) {
        match obj {
            HierId::Name(scope, id) => {
                if let Some(scope) = scope {
                    self.append(Self::get_scope(scope));
                    self.append("::");
                }
                self.append(format!("{}", id));
            }
            HierId::Member(name, id) => {
                self.print_hier_id(name);
                self.append(format!(".{}", id));
            }
            HierId::Select(name, sel) => {
                self.print_hier_id(name);
                self.print_dim(sel);
            }
            _ => unimplemented!(),
        }
    }

    fn print_sys_tf_call(&mut self, obj: &SysTfCall) {
        self.append(format!("${}", obj.task));
        if let Some(args) = &obj.args {
            self.print_args(args, true);
        }
    }

    pub fn print_expr(&mut self, obj: &Expr) {
        match &**obj {
            ExprKind::Type(v) => self.print_type(&v),
            ExprKind::Literal(v) => {
                match &**v {
                    TokenKind::IntegerLiteral(num) => self.append(format!("{}", num)),
                    TokenKind::UnbasedLiteral(val) => self.append(format!("'{}", val)),
                    // TODO: Escape
                    TokenKind::StringLiteral(str) => self.append(format!("\"{}\"", str)),
                    _ => {
                        eprintln!("{:?}", v);
                        unimplemented!();
                    }
                }
            }
            ExprKind::HierName(name) => {
                self.print_hier_id(name);
            }
            ExprKind::Concat(list, select) => {
                self.append("{");
                self.print_comma_list(list, |this, v| this.print_expr(v));
                self.append("}");
                if let Some(select) = select {
                    self.print_dim(select);
                }
            }
            ExprKind::MultConcat(mult, concat, select) => {
                self.append("{");
                self.print_expr(mult);
                self.print_expr(concat);
                self.append("}");
                if let Some(select) = select {
                    self.print_dim(select);
                }
            }
            ExprKind::AssignPattern(ty, pat) => {
                if let Some(v) = ty {
                    self.print_type(v);
                }
                self.append("'{");
                match pat {
                    AssignPattern::Simple(list) => {
                        self.print_comma_list(list, Self::print_expr);
                    }
                    AssignPattern::Keyed(list) => {
                        self.print_comma_list(list, |this, (k, v)| {
                            this.print_expr(k);
                            this.append(": ");
                            this.print_expr(v);
                        });
                    }
                    AssignPattern::Mult(mul, list) => {
                        self.print_expr(mul);
                        self.append("{");
                        self.print_comma_list(list, Self::print_expr);
                        self.append("}");
                    }
                }
                self.append("}");
            }
            ExprKind::SysTfCall(tf) => self.print_sys_tf_call(tf),
            ExprKind::FuncCall { expr, attr: _, args } => {
                self.print_expr(expr);
                if let Some(args) = args {
                    self.print_args(args, true);
                }
            }
            ExprKind::SignCast(sign, expr) => {
                self.append(format!("{}'(", sign));
                self.print_expr(expr);
                self.append(")");
            }
            ExprKind::TypeCast(ty, expr) => {
                self.print_expr(ty);
                self.append("'(");
                self.print_expr(expr);
                self.append(")");
            }
            ExprKind::Unary(op, _attr, expr) => {
                self.append(format!("{}", op));
                self.print_expr(expr);
            }
            ExprKind::Assign(lhr, rhs) => {
                self.print_expr(lhr);
                self.append(" = ");
                self.print_expr(rhs);
            }
            ExprKind::NonblockAssign(lhr, rhs) => {
                self.print_expr(lhr);
                self.append(" <= ");
                self.print_expr(rhs);
            }
            ExprKind::BinaryAssign(lhr, op, rhs) => {
                self.print_expr(lhr);
                self.append(format!(" {}= ", op));
                self.print_expr(rhs);
            }
            ExprKind::Binary(lhr, op, _attr, rhs) => {
                self.print_expr(lhr);
                self.append(format!(" {} ", op));
                self.print_expr(rhs);
            }
            ExprKind::PostfixIncDec(expr, _attr, op) => {
                self.print_expr(expr);
                self.append(format!("{}", op));
            }
            ExprKind::Paren(v) => {
                self.append("(");
                self.print_expr(v);
                self.append(")");
            }
            ExprKind::Cond(cond, _attr, t, f) => {
                self.print_expr(cond);
                self.append(" ? ");
                self.print_expr(t);
                self.append(" : ");
                self.print_expr(f);
            }
            _ => {
                eprintln!("{:?}", obj);
                unimplemented!()
            }
        }
    }

    fn print_event_expr(&mut self, obj: &EventExpr) {
        match obj {
            EventExpr::Item(item) => {
                if let Some(v) = item.edge {
                    self.append(format!("{} ", v));
                }
                self.print_expr(&item.expr);
                if let Some(v) = &item.iff {
                    self.append(" iff ");
                    self.print_expr(&v);
                }
            }
            EventExpr::List(v) => {
                self.print_sep_list(" or ", v, Self::print_event_expr)
            }
            EventExpr::Paren(v) => {
                self.append("(");
                self.print_event_expr(v);
                self.append(")");
            }
        }
    }

    fn print_timing_ctrl(&mut self, obj: &TimingCtrl) {
        match obj {
            TimingCtrl::ExprEventCtrl(expr) => {
                self.append("@(");
                self.print_event_expr(&expr);
                self.append(")");
            }
            _ => {
                eprintln!("{:?}", obj);
                unimplemented!();
            }
        }
    }

    pub fn print_stmt(&mut self, obj: &Stmt) {
        match &obj.value {
            StmtKind::Empty => {
                self.append(";");
            }
            StmtKind::TimingCtrl(timing, stmt) => {
                self.print_timing_ctrl(timing);
                self.append(" ");
                self.print_stmt(&stmt);
            }
            StmtKind::If(uniq, cond, t, f) => {
                if let Some(v) = uniq {
                    self.append(format!("{} ", v));
                }
                self.append("if (");
                self.print_expr(&cond);
                self.append(") ");
                self.print_stmt(&t);
                if let Some(v) = f {
                    self.append(" else ");
                    self.print_stmt(&v);
                }
            }
            StmtKind::Case { uniq, kw, expr, items } => {
                if let Some(v) = uniq {
                    self.append(format!("{} ", v));
                }
                self.append(format!("{} (", kw));
                self.print_expr(&expr);
                self.append(")\n");
                self.indent();
                for (cond, stmt) in items {
                    self.indent_append("");
                    if cond.is_empty() {
                        self.append("default: ");
                    } else {
                        self.print_comma_list(cond, |this, expr| {
                            this.print_expr(expr);
                        });
                        self.append(": ");
                    }
                    self.print_stmt(stmt);
                    self.append("\n");
                }
                self.unindent();
                self.indent_append("endcase");
            }
            StmtKind::For { ty, init, cond, update, body } => {
                self.append("for (");
                if let Some(ty) = ty {
                    self.print_type(ty);
                    self.append(" ");
                }
                self.print_comma_list(init, Self::print_expr);
                self.append("; ");
                if let Some(expr) = cond { self.print_expr(expr); }
                self.append("; ");
                self.print_comma_list(update, Self::print_expr);
                self.append(") ");
                self.print_stmt(body);
            }
            StmtKind::Assert { kind: (), expr, success, failure } => {
                self.append("assert (");
                self.print_expr(expr);
                self.append(")");
                if let Some(v) = success {
                    self.print_stmt(v);
                }
                if let Some(v) = failure {
                    self.append(" else ");
                    self.print_stmt(&v);
                }
            }
            StmtKind::SeqBlock(list) => {
                self.append("begin");
                if let Some(v) = &obj.label {
                    self.append(format!(": {}", v));
                }
                self.append("\n");
                self.indent();
                for item in list {
                    self.indent_append("");
                    self.print_stmt(item);
                    self.append("\n");
                }
                self.unindent();
                self.indent_append("end");
            }
            StmtKind::Expr(expr) => {
                self.print_expr(&expr);
                self.append(";");
            }
            StmtKind::DataDecl(decl) => self.print_data_decl(decl),
        }
    }
}