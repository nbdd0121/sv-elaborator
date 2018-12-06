use super::parser::ast::*;
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

    fn print_param_decl(&mut self, obj: &ParamDecl) {
        self.indent_append(format!("{} ", if obj.kw == Keyword::Parameter { "parameter" } else { "localparam" }));
        // TODO Print type
        for (item, _, last) in obj.list.iter().identify_first_last() {
            self.append(format!("{}", item.name));
            // pub dim: Vec<Dim>,
            if let Some(ref _v) = item.init {
                self.append(format!(" = something"));
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
        self.append(format!("{} ", obj.name));
        for (item, first, _) in obj.inst.iter().identify_first_last() {
            if !first {
                self.append(", ");
            }
            self.append(format!("{}()", item.name));
        }
        self.append(";\n");
    }

    pub fn print_item(&mut self, obj: &Item) {
        match obj {
            Item::ModuleDecl(v) => self.print_module_decl(v),
            Item::HierInstantiation(v) => self.print_hier_instantiation(v),
            Item::ContinuousAssign(list) => {
                self.indent_append("assign ");
                for (item, _, last) in list.iter().identify_first_last() {
                    self.append(format!("{:?}", item));
                    if !last {
                        self.append(format!(", "));
                    }
                }
                self.append(format!("\n"));
            } 
            _ => unimplemented!(),
        }
    }
}