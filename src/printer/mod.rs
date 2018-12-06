use super::parser::ast::*;
use lexer::{Keyword};

use util::IdentifyFirstLast;

use std::io::{Result, Write};

pub trait PrettyPrint {
    fn print(&self, f: &mut Write, indent: &str) -> Result<()>;
}

impl PrettyPrint for ParamDecl {
    fn print(&self, f: &mut Write, indent: &str) -> Result<()> {
        write!(f, "{}{} ", indent, if self.kw == Keyword::Parameter { "parameter" } else { "localparam" });
        // TODO Print type
        for (item, _, last) in self.list.iter().identify_first_last() {
            write!(f, "{}", item.name);
            // pub dim: Vec<Dim>,
            if let Some(ref _v) = item.init {
                write!(f, " = something");
            }
            if !last {
                write!(f, ", ");
            }
        }
        Ok(())
    }
}

impl PrettyPrint for ModuleDecl {
    fn print(&self, f: &mut Write, indent: &str) -> Result<()> {
        let new_ident = indent.to_owned() + "    ";
        write!(f, "module")?;
        if self.lifetime == Lifetime::Automatic {
            write!(f, " automatic")?;
        }
        write!(f, " {}", self.name)?;
        if let Some(ref v) = self.param {
            write!(f, " #(\n")?;
            for (item, _, last) in v.iter().identify_first_last() {
                item.print(f, &new_ident)?;
                write!(f, "{}\n", if { last } { "" } else { "," });
            }
            write!(f, ")")?;
        }
        if self.port.len() != 0 {
            write!(f, " (\n")?;
            for (item, _, last) in self.port.iter().identify_first_last() {
                write!(f, "{:?}{}\n", item, if { last } { "" } else { "," });
            }
            write!(f, ")")?;
        }
        write!(f, ";\n")?;
        for item in &self.items {
            item.print(f, &new_ident)?;
        }
        write!(f, "endmodule\n")?;
        Ok(())
    }
}

impl PrettyPrint for Item {
    fn print(&self, f: &mut Write, indent: &str) -> Result<()> {
        match self {
            Item::ModuleDecl(v) => v.print(f, indent),
            Item::ContinuousAssign(list) => {
                write!(f, "{}assign ", indent)?;
                for (item, _, last) in list.iter().identify_first_last() {
                    write!(f, "{:?}", item)?;
                    if !last {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "\n")?;
                Ok(())
            } 
            _ => unimplemented!(),
        }
    }
}