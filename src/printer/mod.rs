use super::parser::ast::*;
use lexer::TokenKind;
use lexer::{Keyword};

use util::IdentifyFirstLast;

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

    fn indent(&mut self, i: isize) {
        if i > 0 {
            self.indent.push_str(&" ".repeat(i as usize))
        } else {
            let newlen = (self.indent.len() as isize + i) as usize;
            self.indent.truncate(newlen)
        }
    }

    fn append<T: Into<String>>(&mut self, str: T) {
        self.output.push_str(&str.into());
    }

    fn indent_append<T: Into<String>>(&mut self, str: T) {
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

    fn print_dim(&mut self, obj: &Dim) {
        match &**obj {
            DimKind::Value(v) => {
                self.append("[");
                self.print_expr(v);
                self.append("]");
            }
            _ => unimplemented!(),
        }
    }

    fn print_param_decl(&mut self, obj: &ParamDecl) {
        self.indent_append(format!("{} ", if obj.kw == Keyword::Parameter { "parameter" } else { "localparam" }));
        // TODO Print type
        for (item, _, last) in obj.list.iter().identify_first_last() {
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

    fn print_module_decl(&mut self, obj: &ModuleDecl) {
        self.indent_append("module");
        self.indent(4);
        if obj.lifetime == Lifetime::Automatic {
            self.append(format!(" automatic"));
        }
        self.append(format!(" {}", obj.name));
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
                self.append(format!("{:?}{}\n", item, if { last } { "" } else { "," }));
            }
            self.append(format!(")"));
        }
        self.append(format!(";\n"));
        for item in &obj.items {
            self.print_item(item);
        }
        self.append(format!("endmodule\n"));
        self.indent(-4);
    }

    fn print_hier_instantiation(&mut self, obj: &HierInstantiation) {
        self.indent_append("");
        if let Some(_) = obj.attr {
            unimplemented!();
        }
        self.append(format!("{}", obj.name));
        if let Some(v) = &obj.param {
            self.append(" #(\n");
            self.indent(4);
            self.print_comma_list_newline(v, |this, t| {
                this.print_arg(t)
            });
            self.indent(-4);
            self.indent_append(")");
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
            self.indent(4);
            self.print_comma_list_newline(&item.ports, |this, t| {
                this.print_arg(t)
            });
            self.indent(-4);
            self.indent_append(")");
        }
        self.append(";\n");
    }

    fn print_arg(&mut self, obj: &Arg) {
        match obj {
            Arg::Ordered(_, None) => (),
            Arg::Ordered(_, Some(v)) => self.print_expr(v),
            Arg::NamedWildcard(_) => self.append(".*"), 
            Arg::Named(_, id, v) => {
                self.append(format!(".{}(", id));
                if let Some(v) = v {
                    self.print_expr(v);
                }
                self.append(")");
            }
        }
    }

    pub fn print_item(&mut self, obj: &Item) {
        match obj {
            Item::ModuleDecl(v) => self.print_module_decl(v),
            Item::HierInstantiation(v) => self.print_hier_instantiation(v),
            Item::ContinuousAssign(list) => {
                self.indent_append("assign ");
                for (item, _, last) in list.iter().identify_first_last() {
                    self.print_expr(item);
                    if !last {
                        self.append(format!(", "));
                    }
                }
                self.append(format!(";\n"));
            }
            Item::GenRegion(list) => {
                self.indent_append("generate\n");
                self.indent(4);
                for item in list {
                    self.print_item(item);
                }
                self.indent(-4);
                self.indent_append("endgenerate\n");
            }
            Item::LoopGen(gen) => {
                self.indent_append("for(");
                if gen.genvar {
                    self.append("genvar ");
                }
                self.append(format!("{} = ", gen.id));
                self.print_expr(&gen.init);
                self.append("; ");
                self.print_expr(&gen.cond);
                self.append("; ");
                self.print_expr(&gen.update);
                self.append(")\n");
                self.indent(4);
                self.print_item(&gen.block);
                self.indent(-4);
            }
            Item::IfGen(gen) => {
                self.indent_append("if(");
                self.print_expr(&gen.cond);
                self.append(")\n");
                self.indent(4);
                self.print_item(&gen.true_block);
                self.indent(-4);
                if let Some(v) = &gen.false_block {
                    self.indent_append("else\n");
                    self.indent(4);
                    self.print_item(&v);
                    self.indent(-4);
                }
            }
            Item::GenBlock(gen) => {
                self.indent(-4);
                self.indent_append("begin");
                if let Some(v) = &gen.name {
                    self.append(format!(": {}", v));
                }
                self.append("\n");
                self.indent(4);
                for item in &gen.items {
                    self.print_item(item);
                }
                self.indent(-4);
                self.indent_append("end\n");
                self.indent(4);
            }
            _ => unimplemented!(),
        }
    }

    fn get_hier_id(obj: &HierId) -> String {
        match obj {
            HierId::Name(None, id) => format!("{}", id),
            HierId::Name(Some(name), id) => format!("{}.{}", Self::get_hier_id(name), id),
            _ => unimplemented!(),
        }
    }

    pub fn print_expr(&mut self, obj: &Expr) {
        match &**obj {
            ExprKind::Literal(v) => {
                match &**v {
                    TokenKind::IntegerLiteral(num) => self.append(format!("{}", num)),
                    TokenKind::UnbasedLiteral(val) => self.append(format!("'{}", val)),
                    _ => unimplemented!(),
                }
            }
            ExprKind::HierName(scope, name) => {
                if let Some(_) = scope {
                    unimplemented!();
                }
                self.append(Self::get_hier_id(name));
            }
            ExprKind::Unary(op, expr) => {
                self.append(format!("{}", op));
                self.print_expr(expr);
            }
            ExprKind::Assign(lhr, op, rhs) |
            ExprKind::Binary(lhr, op, rhs) => {
                self.print_expr(lhr);
                self.append(format!(" {} ", op));
                self.print_expr(rhs);
            }
            ExprKind::PostfixIncDec(expr, op) => {
                self.print_expr(expr);
                self.append(format!("{}", op));
            }
            ExprKind::Paren(v) => {
                self.append("(");
                self.print_expr(v);
                self.append(")");
            }
            ExprKind::Member(lhs, id) => {
                self.print_expr(lhs);
                self.append(format!(".{}", id));
            }
            ExprKind::Select(lhs, sel) => {
                self.print_expr(lhs);
                self.append("[");
                match sel {
                    Select::Value(rhs) => {
                        self.print_expr(rhs);
                    }
                    Select::Range(start, end) => {
                        self.print_expr(start);
                        self.append(":");
                        self.print_expr(end);
                    }
                    Select::PlusRange(start, end) => {
                        self.print_expr(start);
                        self.append("+:");
                        self.print_expr(end);
                    }
                    Select::MinusRange(start, end) => {
                        self.print_expr(start);
                        self.append("-:");
                        self.print_expr(end);
                    }
                }
                self.append("]");
            }
            _ => {
                eprintln!("{:?}", obj);
                unimplemented!()
            }
        }
    }
}