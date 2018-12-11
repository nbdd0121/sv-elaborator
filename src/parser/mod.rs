pub mod ast;

use self::ast::*;
use super::lexer::{Token, TokenKind, Keyword, Operator, Delim, DelimGroup};
use super::source::{SrcMgr, Diagnostic, DiagMgr, Severity, Pos, Span, Spanned};

use std::mem;
use std::rc::Rc;
use std::collections::VecDeque;
use std::borrow::Borrow;

pub struct Parser {
    mgr: Rc<SrcMgr>,
    diag: Rc<DiagMgr>,
    lexer: VecDeque<Token>,
    eof: Token,
}

//
// Data types internal to parser
//
#[derive(Clone, Copy, PartialEq, Eq)]
enum ArgOption {
    Param,
    Port,
    Arg,
}

/// Disambiguated item that can possibly start with identifier.
enum ItemDAB {
    DataDecl,
    HierInst,
    IntfPort,
    NetDecl,
}

/// SystemVerilog Parser.
///
/// The complexity of SystemVerilog comes from determing what an identifer means. There are few
/// cases that we might be confused:
/// When parsing item, we might be confused by:
/// * hierarchical instantiation
/// * data declaration
/// * interface port declaration
/// * net declartion
///
/// In statement context, we might be confused by:
/// * data declaration
/// * identifier in expression
///
/// Whenever implicit data type is allowed, we might be confused by:
/// * used-defined data type
/// * implicit data type followed by identifier
///
/// When parsing param expression, we might be confused by:
/// * data type
/// * identifier in expression
///
/// When parsing port, we need to disambiguate interface port declaration in addition to implicit
/// data type.
///
/// We disambiguate hierachical instantiation we need to lookahead a check if it is an
/// instantation. For interface port declaration vs data declaration, we will parse ambiguate terms
/// as data declaration outside ANSI port declaration (as later is more common and it is easy to
/// convert). In ANSI port declaration we do other way around, as interface port has not directions
/// and so we kind-of assume if directions are omitted it should be an interface port (It still
/// possible that it ends up being a variable port with direction inherited from previous ports,
/// but conversion is also easy in this case.
///
/// For param expression case, we coded the parser so that the expression parsing function will
/// accept all data types as expressions. By making data types as subset of expressions, we don't
/// need to disambiguate anymore. We only have to perform expression-to-type conversion when needed
///
/// For net-type and data-type, we cannot tell from user-defined net-type vs data-type.
/// We will parse everything as data-type, and fix later if semantics analysis found out that
/// out guess is wrong.
///
/// We've noted that all identifiers are actually legal data types, so for implicit data type
/// followed by identifier case we will parse them as data type first and when we see no
/// identifiers following we can perform conversion.
impl Parser {
    pub fn new(mgr: Rc<SrcMgr>, diag: Rc<DiagMgr>, lexer: VecDeque<Token>) -> Parser {
        let last_pos = lexer.back().map(|x| x.span.end).unwrap_or(Pos(0));
        Parser {
            mgr,
            diag,
            lexer: lexer,
            eof: Spanned::new(TokenKind::Eof, last_pos.span_to(last_pos))
        }
    }

    fn consume(&mut self) -> Token {
        match self.lexer.pop_front() {
            Some(v) => v,
            None => self.eof.clone(),
        }
    }

    fn peek(&mut self) -> &Token {
        match self.lexer.front() {
            Some(v) => v,
            None => &self.eof,
        }
    }

    fn peek_n(&mut self, n: usize) -> &Token {
        if self.lexer.len() > n {
            &self.lexer[n]
        } else {
            &self.eof
        }
    }

    fn pushback(&mut self, tok: Token) {
        self.lexer.push_front(tok);
    }

    /// Parse a delimited group of tokens. After calling the callback, the token stream must
    /// be empty.
    fn delim_group<T, F: FnMut(&mut Self) -> T>(
        &mut self, mut stream: Box<DelimGroup>, mut f: F
    ) -> T {
        let mut delim_eof = Spanned::new(TokenKind::Eof, stream.close.span);
        mem::swap(&mut self.lexer, &mut stream.tokens);
        mem::swap(&mut self.eof, &mut delim_eof);
        let ret = f(self);
        self.expect_eof();
        self.lexer = stream.tokens;
        self.eof = delim_eof;
        ret
    }

    fn consume_if_delim(&mut self, expected: Delim) -> Option<Box<DelimGroup>> {
        let toksp = self.consume();
        match toksp.value {
            TokenKind::DelimGroup(delim, _) if delim == expected => {
                if let TokenKind::DelimGroup(_, grp) = toksp.value {
                    Some(grp)
                } else {
                    unreachable!()
                }
            }
            _ => {
                self.pushback(toksp);
                None
            }
        }
    }

    fn consume_if_eof(&mut self) -> Option<()> {
        match self.peek().value {
            TokenKind::Eof => {
                self.consume();
                Some(())
            }
            _ => None,
        }
    }

    fn consume_if_id(&mut self) -> Option<Ident> {
        let toksp = self.consume();
        if let TokenKind::Id(name) = toksp.value {
            Some(Spanned::new(name, toksp.span))
        } else {
            self.pushback(toksp);
            None
        }
    }

    fn consume_if<T: Borrow<TokenKind>>(&mut self, token: T) -> Option<Token> {
        if &self.peek().value == token.borrow() {
            Some(self.consume())
        } else {
            None
        }
    }

    /// consume_if for users who don't need the token to be returnd
    fn check<T: Borrow<TokenKind>>(&mut self, token: T) -> bool {
        if &self.peek().value == token.borrow() {
            self.consume();
            true
        } else {
            false
        }
    }

    fn expect_delim(&mut self, expected: Delim) -> Box<DelimGroup> {
        match self.consume_if_delim(expected) {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, format!("expected open delimiter {:#?}", expected), span.clone());
                // Error recovery
                let fake_open = Spanned::new(TokenKind::Unknown, span);
                let fake_close = Spanned::new(TokenKind::Unknown, span);
                Box::new(DelimGroup {
                    open: fake_open,
                    close: fake_close,
                    tokens: VecDeque::new(),
                })
            }
            Some(v) => v,
        }
    }

    fn expect_eof(&mut self) {
        if let TokenKind::Eof = self.peek().value {} else {
            let span = self.peek().span;
            self.report_span(Severity::Error, "unexpected extra token", span);
        }
    }

    fn expect_id(&mut self) -> Ident {
        match self.consume_if_id() {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, "expected identifier", span.clone());
                // Error recovery
                Spanned::new("".to_owned(), span)
            }
            Some(v) => v,
        }
    }

    fn expect<T: Borrow<TokenKind>>(&mut self, token: T) -> Token {
        let token = token.borrow();
        match self.consume_if(token) {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, format!("expected token {:?}", token), span.clone());
                // Error recovery
                Spanned::new(TokenKind::Unknown, span)
            }
            Some(v) => v,
        }
    }

    fn report_span<M: Into<String>>(&self, severity: Severity, msg: M, span: Span) {
        self.report_diag(Diagnostic::new(
            severity,
            msg.into(),
            span,
        ))
    }

    fn report_diag(&self, diag: Diagnostic) {
        self.diag.report(diag)
    }

    //
    // Utility functions
    //

    /// Unwrap `Option` with sensible error message
    fn unwrap<T: AstNode>(&mut self, t: Option<T>) -> T {
        match t {
            None => {
                let span = self.peek().span;
                match T::recovery(span) {
                    None => {
                        self.report_span(
                            Severity::Fatal,
                            format!("{} support is not completed yet", T::name()),
                            span
                        );
                        unreachable!()
                    }
                    Some(v) => {
                        self.report_span(
                            Severity::Error,
                            format!("expected {}", T::name()),
                            span
                        );
                        v
                    }
                }
            }
            Some(v) => v,
        }
    }

    /// Unwrap `Option` with sensible error message
    fn parse_unwrap<T: AstNode, F: FnMut(&mut Self) -> Option<T>> (
        &mut self, mut f: F
    ) -> T {
        let result = f(self);
        self.unwrap(result)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim<T, F: FnMut(&mut Self) -> T>(
        &mut self, delim: Delim, f: F
    ) -> T {
        let delim = self.expect_delim(delim);
        self.delim_group(delim, f)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim_spanned<T, F: FnMut(&mut Self) -> T>(
        &mut self, delim: Delim, f: F
    ) -> Spanned<T> {
        let span = self.peek().span;
        let delim = self.expect_delim(delim);
        Spanned::new(self.delim_group(delim, f), span)
    }


    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim<T, F: FnMut(&mut Self) -> T>(
        &mut self, delim: Delim, f: F
    ) -> Option<T> {
        match self.consume_if_delim(delim) {
            None => None,
            Some(v) => Some(self.delim_group(v, f)),
        }
    }

    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim_spanned<T, F: FnMut(&mut Self) -> T>(
        &mut self, delim: Delim, f: F
    ) -> Option<Spanned<T>> {
        match **self.peek() {
            TokenKind::DelimGroup(d, _) if d == delim => (),
            _ => return None,
        }
        let token = self.consume();
        if let TokenKind::DelimGroup(_, grp) = token.value {
            Some(Spanned::new(self.delim_group(grp, f), token.span))
        } else {
            unreachable!();
        }
    }

    /// Parse until `None` is returned, and organize parsed items into a list.
    fn parse_list<T, F: FnMut(&mut Self) -> Option<T>>(&mut self, mut f: F) -> Vec<T> {
        let mut vec = Vec::new();
        loop {
            let result = f(self);
            match result {
                None => break,
                Some(v) => vec.push(v),
            }
        }
        vec
    }

    /// Parse a comma seperated list. We require `F` to return a `Option<T>` as it will make
    /// diagnostics easier by being able to catch trailing comma easily.
    /// * `empty`: If true, empty list is allowed
    /// * `trail`: If true, trailing comma is allowed
    fn parse_comma_list<T, F: FnMut(&mut Self) -> Option<T>>(
        &mut self, empty: bool, trail: bool, mut f: F
    ) -> Vec<T> {
        let mut vec = Vec::new();

        // Parse first element
        let result = f(self);
        match result {
            None => {
                // If we failed and this is the first element, then we get an empty list
                if !empty {
                    let span = self.peek().span.clone();
                    self.report_span(Severity::Error, "empty list not allowed", span);
                }
                return vec
            }
            Some(v) => vec.push(v),
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Comma) {
                None => break,
                Some(v) => v,
            };
            let result = f(self);
            match result {
                None => {
                    if !trail {
                        // TODO: We could place a FixItHint here.
                        self.report_span(
                            Severity::Error,
                            "trailing comma is not allowed; consider removing it",
                            comma.span
                        );
                    }
                    break;
                }
                Some(v) => vec.push(v),
            }
        }

        vec
    }

    /// Parse a comma seperated list, but the list will be built externally. Similarly to above
    /// expects boolean instead of Option<T>.
    /// * `empty`: If true, empty list is allowed
    /// * `trail`: If true, trailing comma is allowed
    fn parse_comma_list_unit<F: FnMut(&mut Self) -> bool>(
        &mut self, empty: bool, trail: bool, mut f: F
    ) {
        // Parse first element
        if !f(self) {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, "empty list not allowed", span);
            }
            return
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Comma) {
                None => break,
                Some(v) => v,
            };
            if !f(self) {
                if !trail {
                    // TODO: We could place a FixItHint here.
                    self.report_span(
                        Severity::Error,
                        "trailing comma is not allowed; consider removing it",
                        comma.span
                    );
                }
                break;
            }
        }
    }

    /// Parse a seperated list, but do not attempt to build a vector.
    fn parse_sep_list_unit<F: FnMut(&mut Self) -> bool>(
        &mut self, sep: Operator, empty: bool, trail: bool, mut f: F
    ) {
        // Parse first element
        if !f(self) {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span;
                self.report_span(Severity::Error, "empty list not allowed", span);
            }
            return
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Operator(sep)) {
                None => break,
                Some(v) => v,
            };
            if !f(self) {
                if !trail {
                    // TODO: We could place a FixItHint here.
                    self.report_span(
                        Severity::Error,
                        format!("trailing {:#?} is not allowed; consider removing it", sep),
                        comma.span
                    );
                }
                break;
            }
        }
    }

    /// Check if the list contains invalid elements, and remove them.
    fn check_list<T, F: FnMut(&mut Self, &T) -> bool>(
        &mut self, mut f: F, list: &mut Vec<T>
    ) {
        list.retain(|x| f(self, x));
    }

    /// Parse an item/declaration. Even though different scope allows different items, as most
    /// items are easily distinguishable by a keyword, we parse them together.
    ///
    /// We've made some rearrangement of BNF to make it easier for us to parse (and most
    /// importantly make reduces conflicts)
    /// * timeunits_declaration will be parsed together with other declarations
    /// * Attributes will be parsed together
    /// * extern declarations are parsed here
    ///
    /// After our rearrangement (this does not exist in spec)
    /// ```bnf
    /// item ::= { attribute_instance } item_noattr
    /// item_noattr ::=
    /// # from 'description' (expanded)
    ///   *net_declaration*
    ///   *data_declaration*
    /// | timeunits_declaration
    /// | module_declaration
    /// | udp_declaration
    /// | interface_declaration
    /// | program_declaration
    /// | package_declaration
    /// | bind_directive
    /// | config_declaration
    /// | anonymous_program
    /// | package_export_declaration
    /// | task_declaration
    /// | function_declaration
    /// | checker_declaration
    /// | dpi_import_export
    /// | extern_constraint_declaration
    /// | class_declaration
    /// | class_constructor_declaration
    /// | local_parameter_declaration ;
    /// | parameter_declaration ;
    /// | covergroup_declaration
    /// | assertion_item_declaration
    /// # from 'module_item'
    /// | *udp_instantiation*
    /// | *interface_instantiation*
    /// | *program_instantiation*
    /// | *module_instantiation*
    /// | port_declaration
    /// | generate_region
    /// | specify_block
    /// | specparam_declaration
    /// | parameter_override
    /// | gate_instantiation
    /// | genvar_declaration
    /// | clocking_declaration
    /// | default clocking clocking_identifier ;
    /// | default disable iff expression_or_dist ;
    /// | assertion_item
    /// | continuous_assign
    /// | net_alias
    /// | initial_construct
    /// | final_construct
    /// | always_construct
    /// | loop_generate_construct
    /// | conditional_generate_construct
    /// | elaboration_system_task
    /// # from 'interface item'
    /// | modport_declaration
    /// | extern_tf_declaration 
    /// ```
    ///
    /// SystemVerilog supports following extern definitions:
    /// ```systemverilog
    /// extern module/macromodule
    /// extern interface
    /// extern program
    /// extern [pure/virtual/static/protected/local] task/function/forkjoin
    /// extern [static] constraint
    /// extern primitive
    /// ```
    fn parse_item_opt(&mut self) -> Option<Item> {
        let attr = self.parse_attr_inst_opt();
        match self.peek().value {
            TokenKind::Eof |
            TokenKind::Keyword(Keyword::Endmodule) |
            TokenKind::Keyword(Keyword::Endgenerate) |
            TokenKind::Keyword(Keyword::End) => None,
            // Externs are parsed together (even though they're not currently supported yet)
            TokenKind::Keyword(Keyword::Extern) => {
                let clone = self.peek().span.clone();
                self.report_span(Severity::Fatal, "extern is not supported", clone);
                unreachable!()
            }
            TokenKind::Keyword(Keyword::Import) => {
                // IMP: This can also be DPI import
                Some(Item::PkgImport(self.parse_pkg_import_decl()))
            }
            TokenKind::Keyword(Keyword::Parameter) |
            TokenKind::Keyword(Keyword::Localparam) => {
                Some(Item::ParamDecl(Box::new(self.parse_param_decl())))
            }
            // module_declaration
            TokenKind::Keyword(Keyword::Module) => {
                Some(Item::DesignDecl(Box::new(self.parse_design_unit(attr, Keyword::Module, Keyword::Endmodule))))
            }
            // udp_declaration
            TokenKind::Keyword(Keyword::Primitive) => {
                Some(Item::DesignDecl(Box::new(self.parse_design_unit(attr, Keyword::Primitive, Keyword::Endprimitive))))
            }
            // interface_declaration
            TokenKind::Keyword(Keyword::Interface) => {
                Some(Item::DesignDecl(Box::new(self.parse_design_unit(attr, Keyword::Interface, Keyword::Endinterface))))
            }
            // program_declaration
            TokenKind::Keyword(Keyword::Program) => {
                Some(Item::DesignDecl(Box::new(self.parse_design_unit(attr, Keyword::Program, Keyword::Endprogram))))
            }
            // package_declaration
            TokenKind::Keyword(Keyword::Package) => {
                Some(Item::DesignDecl(Box::new(self.parse_design_unit(attr, Keyword::Package, Keyword::Endpackage))))
            }
            // continuous_assign
            TokenKind::Keyword(Keyword::Assign) => Some(self.parse_continuous_assign()),
            // initial_construct
            TokenKind::Keyword(Keyword::Initial) => {
                self.consume();
                let stmt = self.parse_stmt();
                Some(Item::Initial(Box::new(stmt)))
            }
            // always_construct
            TokenKind::AlwaysKw(_) => Some(self.parse_always()),
            // generate_region
            TokenKind::Keyword(Keyword::Generate) => {
                let kw = self.consume();
                self.report_span(Severity::Warning, "there is no need for generate region", kw.span);
                let list = self.parse_list(Self::parse_item_opt);
                self.expect(TokenKind::Keyword(Keyword::Endgenerate));
                Some(Item::GenRegion(list))
            }
            // loop_generate_construct
            TokenKind::Keyword(Keyword::For) => Some(self.parse_loop_gen(attr)),
            // if_generate_construct
            TokenKind::Keyword(Keyword::If) => Some(self.parse_if_gen(attr)),
            // case_generate_construct
            TokenKind::CaseKw(CaseKw::Case) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "case_generate_construct is not supported", span);
                unreachable!()
            }
            // elaboration_system_task
            TokenKind::SystemTask(_) => {
                let tf = self.parse_sys_tf_call();
                self.expect(TokenKind::Semicolon);
                Some(Item::SysTfCall(Box::new(tf)))
            }
            // net_declaration
            TokenKind::Keyword(Keyword::Interconnect) => {
                unimplemented!();
            }
            // also net_declaration
            TokenKind::NetTy(_) => {
                unimplemented!();
            }
            // data_declaration. Either begin with const/var or explicit data type.
            TokenKind::Keyword(Keyword::Const) |
            TokenKind::Keyword(Keyword::Var) |
            TokenKind::Keyword(Keyword::Type) |
            TokenKind::IntAtomTy(_) |
            TokenKind::IntVecTy(_) |
            TokenKind::Keyword(Keyword::Reg) |
            TokenKind::NonIntTy(_) |
            TokenKind::Keyword(Keyword::Void) => {
                Some(Item::DataDecl(Box::new(self.parse_data_decl(attr))))
            }
            TokenKind::Keyword(kw) if Self::is_keyword_typename(kw) => {
                Some(Item::DataDecl(Box::new(self.parse_data_decl(attr))))
            }
            TokenKind::Id(_) => {
                match self.disambiguate_item() {
                    ItemDAB::HierInst => Some(self.parse_instantiation(attr)),
                    ItemDAB::DataDecl => Some(Item::DataDecl(Box::new(self.parse_data_decl(attr)))),
                    _ => {
                        let clone = self.peek().span.clone();
                        self.report_span(Severity::Fatal, "not implemented", clone);
                        unreachable!()
                    }
                }
            }
            _ => {
                let clone = self.peek().span.clone();
                self.report_span(Severity::Fatal, "not implemented", clone);
                unreachable!()
            }
        }
    }

    fn parse_item(&mut self) -> Item {
        self.parse_unwrap(Self::parse_item_opt)
    }

    /// Parse an entire compilation unit
    ///
    /// According to spec:
    /// ```bnf
    /// source_text ::= [ timeunits_declaration ] { description }
    /// description ::=
    ///   module_declaration
    /// | udp_declaration
    /// | interface_declaration
    /// | program_declaration
    /// | package_declaration
    /// | { attribute_instance } package_item
    /// | { attribute_instance } bind_directive
    /// | config_declaration
    /// ```
    ///
    /// Our rearrangement:
    /// ```bnf
    /// source_text ::= { item }
    /// ```
    /// TODO: We still need to check if these items can legally appear here.
    pub fn parse_source(&mut self) -> Vec<Item> {
        let list = self.parse_list(Self::parse_item_opt);
        list
    }

    /// Parse a module, interface or program. We processed attributes in parse_item_opt, and
    /// externs will not be processed here.
    ///
    /// Acccording to spec (excl interface & program as they're similar):
    /// ```bnf
    /// module_nonansi_header ::=
    ///   { attribute_instance } module_keyword [ lifetime ] module_identifier
    ///     { package_import_declaration } [ parameter_port_list ] list_of_ports ;
    /// module_ansi_header ::=
    ///   { attribute_instance } module_keyword [ lifetime ] module_identifier
    ///     { package_import_declaration } [ parameter_port_list ] [ list_of_port_declarations ]
    ///     ;
    /// module_declaration ::=
    ///   module_nonansi_header [ timeunits_declaration ] { module_item }
    ///     endmodule [ : module_identifier ]
    /// | module_ansi_header [ timeunits_declaration ] { non_port_module_item }
    ///     endmodule [ : module_identifier ]
    /// | { attribute_instance } module_keyword [ lifetime ] module_identifier ( .* ) ;
    ///     [ timeunits_declaration ] { module_item } endmodule [ : module_identifier ]
    /// | extern module_nonansi_header
    /// | extern module_ansi_header
    /// ```
    ///
    /// Our rearrangement:
    /// ```bnf
    /// module_header ::=
    ///   module_keyword [ lifetime ] module_identifier
    ///     { package_import_declaration } [ parameter_port_list ] [ port_list ] ;
    /// module_declaration ::=
    ///   module_header { item } endmodule [ : module_identifier ]
    /// ```
    /// TODO: We will need to check if items can legally appear in here.
    fn parse_design_unit(&mut self, attr: Option<Box<AttrInst>>, kw: Keyword, end_kw: Keyword) -> DesignDecl {
        self.consume();
        let lifetime = self.parse_lifetime();
        let name = self.expect_id();
        let pkg_import = self.parse_list(Self::parse_pkg_import_decl_opt);
        let param = self.parse_param_port_list();
        let port = self.parse_port_list();
        self.expect(TokenKind::Semicolon);
        let items = self.parse_list(Self::parse_item_opt);
        self.expect(TokenKind::Keyword(end_kw));

        if self.consume_if(TokenKind::Colon).is_some() {
            let id = self.expect_id();
            if *id != *name {
                self.report_span(
                    Severity::Error,
                    format!("identifer annotation at end does match declaration, should be '{}'", name),
                    id.span
                );
            }
        }

        DesignDecl {
            attr,
            kw,
            lifetime,
            name,
            pkg_import,
            param,
            port: port.unwrap_or_else(|| Vec::new()),
            items: items,
        }
    }

    //
    // A.1.3 Module parameters and ports
    //
    
    /// Parse a parameter port list.
    ///
    /// According to spec:
    /// ```bnf
    /// parameter_port_list ::=
    ///   # ( list_of_param_assignments { , parameter_port_declaration } )
    /// | # ( parameter_port_declaration { , parameter_port_declaration } )
    /// | # ( )
    /// parameter_port_declaration ::=
    ///   parameter_declaration
    /// | local_parameter_declaration
    /// | data_type list_of_param_assignments
    /// | type list_of_type_assignments
    /// ```
    ///
    /// Our rearrangement:
    /// ```bnf
    /// parameter_port_list ::=
    ///   # ( parameter_port_declaration { , parameter_port_declaration } )
    /// | # ( )
    /// parameter_port_declaration ::=
    ///   [ parameter | localparam ] [ data_type_or_implicit | type ] param_assignment
    fn parse_param_port_list(&mut self) -> Option<Vec<ParamDecl>> {
        if self.consume_if(TokenKind::Hash).is_none() {
            return None
        }

        self.parse_delim(Delim::Paren, |this| {
            let mut vec = Vec::new();

            // Default to parameter and un-typed
            let mut param_decl = ParamDecl {
                kw: Keyword::Parameter,
                ty: None,
                list: Vec::new()
            };

            this.parse_comma_list_unit(true, false, |this| {
                // If a new keyword is seen update it.
                match **this.peek() {
                    TokenKind::Eof => return false,
                    TokenKind::Keyword(e) if e == Keyword::Parameter || e == Keyword::Localparam => {
                        this.consume();
                        let old_decl = mem::replace(&mut param_decl, ParamDecl {
                            kw: e,
                            ty: None,
                            list: Vec::new()
                        });
                        if !old_decl.list.is_empty() {
                            vec.push(old_decl);
                        }
                    }
                    _ => (),
                };

                // If data type is specified, update kw and ty.
                let (ty, assign) = this.parse_data_type_decl_assign();
                if let Some(v) = ty {
                    let kw = param_decl.kw;
                    let old_decl = mem::replace(&mut param_decl, ParamDecl {
                        kw,
                        ty: Some(Box::new(v)),
                        list: Vec::new()
                    });
                    if !old_decl.list.is_empty() {
                        vec.push(old_decl);
                    }
                };
                param_decl.list.push(assign);
                true
            });

            if !param_decl.list.is_empty() {
                vec.push(param_decl);
            }
            Some(vec)
        })
    }

    /// Parse a port list.
    /// ```
    /// list_of_ports ::= ( port { , port } )
    /// list_of_port_declarations ::=
    ///   ( [ { attribute_instance} ansi_port_declaration { , { attribute_instance} ansi_port_declaration } ] )
    /// port ::=
    ///   [ port_expression ] | . port_identifier ( [ port_expression ] )
    /// port_expression ::=
    ///   port_reference | "{" port_reference { , port_reference } "}"
    /// port_reference ::=
    ///   port_identifier constant_select
    /// net_port_header ::=
    ///   [ port_direction ] net_port_type
    /// variable_port_header ::=
    ///   [ port_direction ] variable_port_type
    /// interface_port_header ::=
    ///   interface_identifier [ . modport_identifier ] | interface [ . modport_identifier ]
    /// ansi_port_declaration ::=
    ///   [ net_port_header | interface_port_header ] port_identifier { unpacked_dimension }
    ///     [ = constant_expression ]
    /// | [ variable_port_header ] port_identifier { variable_dimension } [ = constant_expression ]
    /// | [ port_direction ] . port_identifier ( [ expression ] )
    /// ```
    fn parse_port_list(&mut self) -> Option<Vec<PortDecl>> {
        self.parse_if_delim(Delim::Paren, |this| {
            if let Some(v) = this.consume_if(TokenKind::Operator(Operator::WildPattern)) {
                this.report_span(Severity::Fatal, "(.*) port declaration is not supported", v.span);
                unreachable!();
            }

            // If there are no ports, it doesn't matter about which style we're using.
            if this.consume_if_eof().is_some() {
                return Vec::new()
            }

            let mut ansi = true;
            let mut prev = None;
            let mut vec = Vec::new();

            this.parse_comma_list_unit(true, false, |this| {
                if this.consume_if_eof().is_some() {
                    return false
                }

                let dirsp = this.peek().span.clone();
                let dir = this.parse_port_dir();

                // Could only appear in non-ANSI declaration
                if prev.is_none() {
                    match this.peek().value {
                        TokenKind::DelimGroup(Delim::Brace, _) => {
                            ansi = false;
                            return false
                        }
                        _ => (),
                    }
                }

                // Explicit port declaration
                if let Some(_) = this.consume_if(TokenKind::Operator(Operator::Dot)) {
                    let name = Box::new(this.expect_id());
                    let expr = Box::new(this.parse_unwrap(|this| {
                        this.parse_delim(Delim::Paren, Self::parse_expr_opt)
                    }));

                    // If not specified, default to inout
                    let dir = dir.unwrap_or_else(|| {
                        match prev {
                            None | Some(PortDecl::Interface(..)) => PortDir::Inout,
                            Some(PortDecl::Data(dir, ..)) | Some(PortDecl::Explicit(dir, ..)) => dir,
                        }
                    });
                    
                    let decl = PortDecl::Explicit(dir, name, expr);
                    if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                        vec.push(v);
                    }
                    return true
                }

                // First try parse this as an interface port. Note that `interface_name id` is not
                // tellable from `typedef_name id`, in this case we parse it as interface port if
                // there is no direction, as we can easily convert if our guess is incorrect. The
                // otherway around is a bit harder.
                // If both none, then there is a chance that this is an interface port
                let is_intf = if let TokenKind::Keyword(Keyword::Interface) = **this.peek() {
                    // Okay, this is definitely an interface port
                    this.consume();
                    if this.consume_if(TokenKind::Operator(Operator::Dot)).is_some() {
                        let modport = this.expect_id();
                        Some((None, Some(Box::new(modport))))
                    } else {
                        Some((None, None))
                    }
                } else if let TokenKind::Id(_) = **this.peek() {
                    // If we see the dot, then this is definitely is a interface
                    if let TokenKind::Operator(Operator::Dot) = **this.peek_n(1) {
                        let intf = this.expect_id();
                        this.consume();
                        let modport = this.expect_id();
                        Some((Some(Box::new(intf)), Some(Box::new(modport))))
                    } else if dir.is_none() {
                        if let TokenKind::Id(_) = **this.peek_n(1) {
                            // This is of form "id id", we consider it as interface port if there is
                            // no direction.
                            let intf = this.expect_id();
                            Some((Some(Box::new(intf)), None))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };

                // This is an interface port declaration
                if let Some((a, b)) = is_intf {
                    // Interface should not be specified with direction
                    if !dir.is_none() {
                        this.report_span(
                            Severity::Error,
                            "interface declaration should not be specified together with direction",
                            dirsp
                        );
                    }
                    let decl = PortDecl::Interface(a, b, vec![this.parse_decl_assign()]);
                    if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                        vec.push(v);
                    }
                    return true;
                }

                // Parse net-type
                let net = if this.consume_if(TokenKind::Keyword(Keyword::Var)).is_some() {
                    Some(NetPortType::Variable)
                } else {
                    // TODO parse net-type
                    None
                };

                let (dtype, assign) = this.parse_data_type_decl_assign();

                // If they are all none, it means this is an ANSI port.
                if dir.is_none() && net.is_none() && dtype.is_none() && prev.is_none() {
                    ansi = false;
                    return false;
                }

                // Nothing specified, inherit everything
                if dir.is_none() && net.is_none() && dtype.is_none() {
                    match prev.as_mut().unwrap() {
                        PortDecl::Data(_, _, _, ref mut l) |
                        PortDecl::Interface(_, _, ref mut l) => {
                            l.push(assign);
                            return true;
                        }
                        // Well, if previously it is an explicit port we fall through
                        _ => (),
                    }
                }

                // If not specified, default to inout
                let dir = dir.unwrap_or_else(|| {
                    match prev {
                        None | Some(PortDecl::Interface(..)) => PortDir::Inout,
                        Some(PortDecl::Data(dir, ..)) | Some(PortDecl::Explicit(dir, ..)) => dir,
                    }
                });

                // If not specified, default to default nettype or variable
                let net = net.unwrap_or_else(|| {
                    match dir {
                        PortDir::Input | PortDir::Inout => NetPortType::Default,
                        PortDir::Output => match dtype.as_ref() {
                            None => NetPortType::Default,
                            Some(v) => match **v {
                                DataTypeKind::Implicit(..) => NetPortType::Default,
                                _ => NetPortType::Variable,
                            }
                        }
                        PortDir::Ref => NetPortType::Variable,
                    }
                });

                // Default to implicit wire
                let dtype = Box::new(dtype.unwrap_or_else(|| {
                    Spanned::new(
                        DataTypeKind::Implicit(Signing::Unsigned, Vec::new()), dirsp
                    )
                }));

                let decl = PortDecl::Data(dir, net, dtype, vec![assign]);
                if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                    vec.push(v);
                }

                return true
            });

            if !ansi {
                let span = this.peek().span.clone();
                this.report_span(Severity::Fatal, "non-ANSI port declaration is not yet supported", span);
                unreachable!();
            }

            if let Some(v) = prev {
                vec.push(v);
            }
            vec
        })
    }

    /// Parse a port direction
    /// ```bnf
    /// port_direction ::=
    ///   input | output | inout | ref
    /// ```
    fn parse_port_dir(&mut self) -> Option<PortDir> {
        match self.peek().value {
            TokenKind::PortDir(dir) => {
                self.consume();
                Some(dir)
            }
            _ => None,
        }
    }

    //
    // A.2.1.1 Parameter declarations
    //

    /// Parse a parameter declaration. See also `parse_param_port_list`.
    fn parse_param_decl(&mut self) -> ParamDecl {
        let kw = if let TokenKind::Keyword(kw) = *self.consume() {
            kw
        } else {
            unreachable!();
        };

        let (ty, assign) = self.parse_data_type_decl_assign();
        let mut list = vec![assign];
        if self.check(TokenKind::Comma) {
            self.parse_comma_list_unit(false, false, |this| {
                match this.parse_decl_assign_opt() {
                    None => false,
                    Some(v) => {
                        list.push(v);
                        true
                    }
                }
            });
        }

        self.expect(TokenKind::Semicolon);
        
        ParamDecl {
            kw,
            ty: ty.map(Box::new),
            list
        }
    }

    //
    // A.2.1.3 Type declarations
    //

    fn parse_data_decl(&mut self, attr: Option<Box<AttrInst>>) -> DataDecl {
        let has_const = self.check(TokenKind::Keyword(Keyword::Const));
        let _has_var = self.check(TokenKind::Keyword(Keyword::Var));
        let lifetime = self.parse_lifetime();
        let (ty, assign) = self.parse_data_type_decl_assign();
        let mut list = vec![assign];
        if self.check(TokenKind::Comma) {
            self.parse_comma_list_unit(false, false, |this| {
                match this.parse_decl_assign_opt() {
                    None => false,
                    Some(v) => {
                        list.push(v);
                        true
                    }
                }
            });
        }

        self.expect(TokenKind::Semicolon);
        
        DataDecl {
            attr,
            has_const,
            lifetime,
            ty: ty.unwrap_or_else(|| Spanned::new_unspanned(DataTypeKind::Implicit(Signing::Unsigned, Vec::new()))),
            list
        }
    }

    /// Parse a package import declaration
    fn parse_pkg_import_decl_opt(&mut self) -> Option<Vec<PkgImportItem>> {
        if self.consume_if(TokenKind::Keyword(Keyword::Import)).is_none() {
            return None;
        }
        let list = self.parse_comma_list(false, false, Self::parse_pkg_import_item_opt);
        self.expect(TokenKind::Semicolon);
        Some(list)
    }

    fn parse_pkg_import_decl(&mut self) -> Vec<PkgImportItem> {
        self.parse_pkg_import_decl_opt().unwrap()
    }

    /// Parse a package import item
    fn parse_pkg_import_item_opt(&mut self) -> Option<PkgImportItem> {
        let pkg = match self.consume_if_id() {
            None => return None,
            Some(v) => v,
        };
        self.expect(TokenKind::Operator(Operator::ScopeSep));
        let id = if self.check(TokenKind::Operator(Operator::Mul)) {
            None
        } else {
            Some(self.expect_id())
        };
        Some(PkgImportItem(pkg, id))
    }

    /// Parse a lifeime, defaulted to static
    /// ```bnf
    /// lifetime ::= static | automatic
    /// ```
    fn parse_lifetime(&mut self) -> Lifetime {
        match self.peek().value {
            TokenKind::Keyword(Keyword::Automatic) => {
                self.consume();
                Lifetime::Automatic
            }
            TokenKind::Keyword(Keyword::Static) => {
                self.consume();
                Lifetime::Static
            }
            _ => Lifetime::Static,
        }
    }

    //
    // A.2.2.1 Net and variable types
    //

    /// If this keyword can begin a data_type definition
    fn is_keyword_typename(kw: Keyword) -> bool {
        match kw {
            Keyword::Struct |
            Keyword::Union |
            Keyword::Enum |
            Keyword::String |
            Keyword::Chandle |
            Keyword::Virtual |
            Keyword::Interface |
            Keyword::Event |
            Keyword::Type => true,
            _ => false,
        }
    }

    /// Parse a data type (or implicit) followed a decl_assign.
    fn parse_data_type_decl_assign(&mut self) -> (Option<DataType>, DeclAssign) {
        let expr = self.parse_expr();
        let span = expr.span;
        let dtype = match self.conv_expr_to_type(expr) {
            None => {
                self.report_span(Severity::Fatal, "expected data type or identifier", span);
                // TODO: Do error recovery here.
                unreachable!()
            }
            Some(v) => v,
        };
        
        match self.consume_if_id() {
            Some(name) => {
                let mut dim = self.parse_list(Self::parse_dim_opt);
                self.check_list(Self::check_unpacked_dim, &mut dim);
                let init = match self.consume_if(TokenKind::Operator(Operator::Assign)) {
                    None => None,
                    Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
                };
                (Some(dtype), DeclAssign {
                    name,
                    dim,
                    init
                })
            }
            None => {
                match self.conv_type_to_id(dtype) {
                    Some((name, dim)) => {
                        let init = match self.consume_if(TokenKind::Operator(Operator::Assign)) {
                            None => None,
                            Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
                        };
                        (None, DeclAssign {
                            name,
                            dim,
                            init
                        })
                    }
                    None => {
                        self.report_span(Severity::Fatal, "data type should be followed by an identifier", span);
                        // TODO: Error recovery
                        unreachable!()
                    }
                }
            }
        }
    }

    /// Parse a net declaration or data declaration.
    fn parse_net_decl_assign(&mut self) {
        match **self.peek() {
            TokenKind::Keyword(Keyword::Interconnect) => {
                unimplemented!();
            }
            TokenKind::NetTy(_) => {
                unimplemented!();
            }
            // Possibily a custom-defined net-type.
            TokenKind::Id(_) => {
                // If there's delay control, then definitely net-type.
                if let TokenKind::Hash = **self.peek_n(1) {
                    unimplemented!();
                }
                // Otherwise this can either be a net declaration or data declaration.
                // We prefer to parse it as data declaration as it is more common. We can easily
                // convert it if we actually parsed it wrongly.
            }
            _ => (),
        }
        unimplemented!();
        // If we reached here we will parse everything as net type.
    }

    /// Parse a data_type that starts with a keyword. This does not exist in the spec, but is
    /// useful for parsing within combined data type & expression parser. This includes all
    /// data_type_or_implicit that begins with type keyword or dimension.
    ///
    /// ```bnf
    /// kw_data_type ::=
    ///   integer_vector_type [ signing ] { packed_dimension }
    /// | integer_atom_type [ signing ]
    /// | non_integer_type
    /// | struct_union [ packed [ signing ] ] { struct_union_member { struct_union_member } }
    ///   { packed_dimension }
    /// | enum [ enum_base_type ] { enum_name_declaration { , enum_name_declaration } }
    ///   { packed_dimension }
    /// | string
    /// | chandle
    /// | virtual [ interface ] interface_identifier [ parameter_value_assignment ] [ . modport_identifier ]
    /// | event
    /// | type_reference
    /// | [ signing ] { packed_dimension }
    /// ```
    ///
    /// The following BNFs exist in spec but they're parsed as expression:
    /// ```bnf
    /// | [ class_scope | package_scope ] type_identifier { packed_dimension }
    /// | class_type
    /// ```
    /// TODO: Better span in this function
    fn parse_kw_data_type(&mut self) -> Option<DataType> {
        match **self.peek() {
            TokenKind::IntVecTy(_) |
            TokenKind::Keyword(Keyword::Reg) => {
                // This makes Keyword::Reg turn into IntVecTy::Logic
                let kw = self.consume();
                let ty = if let TokenKind::IntVecTy(IntVecTy::Bit) = *kw {
                    IntVecTy::Bit
                } else {
                    IntVecTy::Logic
                };
                let sign = self.parse_signing();
                let dim = self.parse_list(Self::parse_pack_dim);
                Some(Spanned::new(DataTypeKind::IntVec(ty, sign, dim), kw.span))
            }
            TokenKind::Signing(sign) => {
                let span = self.consume().span;
                let dim = self.parse_list(Self::parse_pack_dim);
                Some(Spanned::new(DataTypeKind::Implicit(sign, dim), span))
            }
            TokenKind::Keyword(Keyword::Void) => {
                let token = self.consume();
                Some(Spanned::new(DataTypeKind::Void, token.span))
            }
            TokenKind::Keyword(Keyword::Type) => {
                let token = self.consume();
                // TODO: Might be parenthesis
                Some(Spanned::new(DataTypeKind::Type, token.span))
            }
            TokenKind::DelimGroup(Delim::Bracket, _) => {
                let span = self.peek().span;
                let dim = self.parse_list(Self::parse_pack_dim);
                Some(Spanned::new(DataTypeKind::Implicit(Signing::Unsigned, dim), span))
            }
            _ => {
                None
            }
        }
    }

    /// Convert an expression to a type. Useful when we try to parse thing as expression first due
    /// to ambiguity, then realised that it is actually a type.
    fn conv_expr_to_type(&mut self, expr: Expr) -> Option<DataType> {
        match expr.value {
            ExprKind::Type(ty) => Some(*ty),
            ExprKind::HierName(scope, name) => {
                Some(Spanned::new(DataTypeKind::HierName(scope, name, Vec::new()), expr.span))
            }
            ExprKind::Select(ty, dim) => {
                let mut ty = match self.conv_expr_to_type(*ty) {
                    None => return None,
                    Some(v) => v,
                };
                match *ty {
                    DataTypeKind::HierName(_, _, ref mut dimlist) => dimlist.push(dim),
                    // If this is a keyword typename, the dimension should already be handled.
                    _ => unreachable!(),
                };
                Some(ty)
            }
            _ => None,
        }
    }

    /// We made a observation that every identifier (with unpacked dimension) looks like an data
    /// type. This function tries to convert a data_type to an identifier (and a dimension list).
    fn conv_type_to_id(&mut self, ty: DataType) -> Option<(Ident, Vec<Dim>)> {
        match ty.value {
            // TODO: what about dimension
            DataTypeKind::HierName(None, HierId::Name(None, id), dim) => {
                Some((*id, dim))
            }
            _ => None,
        }
    }

    /// Parse a signing, defaulted to unsigned
    /// ```bnf
    /// signing ::= signed | unsigned
    /// ```
    fn parse_signing(&mut self) -> Signing {
        match **self.peek() {
            TokenKind::Signing(s) => {
                self.consume();
                s
            }
            _ => Signing::Unsigned,
        }
    }


    //
    // A.2.4 Declaration assignments
    //

    fn parse_decl_assign_opt(&mut self) -> Option<DeclAssign> {
        let mut ident = match self.consume_if_id() {
            None => return None,
            Some(v) => v,
        };
        let mut dim = self.parse_list(Self::parse_dim_opt);

        // If we see another ID here, it means that the ID we seen previously are probably a
        // type name that isn't declared. Raise a sensible warning here.
        if let Some(id) = self.consume_if_id() {
            self.report_span(
                Severity::Error,
                "this looks like a data type but it is not declared",
                ident.span
            );
            ident = id;
            dim = self.parse_list(Self::parse_dim_opt);
        }

        self.check_list(Self::check_unpacked_dim, &mut dim);
        let init = match self.consume_if(TokenKind::Operator(Operator::Assign)) {
            None => None,
            Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
        };
        Some(DeclAssign {
            name: ident,
            dim,
            init
        })
    }

    fn parse_decl_assign(&mut self) -> DeclAssign {
        self.parse_unwrap(Self::parse_decl_assign_opt)
    }

    //
    // A.2.5 Declaration ranges
    //

    /// Parse a dimension or a selection. This is called variable_dimension in the spec. We parse
    /// these together as sometimes we cannot distinguish between them, e.g. data declaration vs
    /// bit-select expression.
    ///
    /// ```bnf
    /// unpacked_dimension ::=  [ constant_range ] | [ constant_expression ]
    /// packed_dimension ::= [ constant_range ] | unsized_dimension
    /// associative_dimension ::= [ data_type ] | [ * ]
    /// variable_dimension ::=
    ///   unsized_dimension | unpacked_dimension | associative_dimension | queue_dimension
    /// queue_dimension ::= [ $ [ : constant_expression ] ]
    /// unsized_dimension ::= [ ]
    /// bit_select ::=
    ///   { [ expression ] }
    /// part_select_range ::=
    ///   constant_range | indexed_range
    /// indexed_range ::=
    ///   expression +: constant_expression | expression -: constant_expression
    /// ```
    fn parse_dim_opt(&mut self) -> Option<Dim> {
        self.parse_if_delim_spanned(Delim::Bracket, |this| {
            match **this.peek() {
                TokenKind::Eof => {
                    return DimKind::Unsized
                }
                TokenKind::Operator(Operator::Mul) => {
                    if let TokenKind::Eof = **this.peek_n(1) {
                        this.consume();
                        return DimKind::AssocWild
                    }
                }
                _ => (),
            }
            let expr = this.parse_expr();
            match **this.peek() {
                TokenKind::Colon => {
                    this.consume();
                    DimKind::Range(Box::new(expr), Box::new(this.parse_expr()))
                }
                TokenKind::Operator(Operator::PlusColon) => {
                    this.consume();
                    DimKind::PlusRange(Box::new(expr), Box::new(this.parse_expr()))
                }
                TokenKind::Operator(Operator::MinusColon) => {
                    this.consume();
                    DimKind::MinusRange(Box::new(expr), Box::new(this.parse_expr()))
                }
                _ => {
                    DimKind::Value(Box::new(expr))
                }
            }
        })
    }

    /// Check if a dimension is a legal unpacked dimension
    fn check_unpacked_dim(&mut self, dim: &Dim) -> bool {
        match **dim {
            DimKind::AssocWild |
            DimKind::PlusRange(..) |
            DimKind::MinusRange(..) |
            DimKind::Unsized => {
                self.report_span(
                    Severity::Error,
                    "this type of range is not allowed in unpacked dimension context",
                    dim.span
                );
                false
            }
            _ => true
        }
    }

    /// Parse a packed dimension
    fn parse_pack_dim(&mut self) -> Option<Dim> {
        let ret = match self.parse_dim_opt() {
            None => return None,
            Some(v) => v,
        };
        match *ret {
            DimKind::AssocWild |
            DimKind::PlusRange(..) |
            DimKind::MinusRange(..) |
            DimKind::Value(_) => {
                self.report_span(
                    Severity::Error,
                    "this type of range is not allowed in packed dimension context",
                    ret.span
                );
                None
            }
            _ => Some(ret)
        }
    }

    //
    // A.2.9 Interface declarations
    //

    
    //
    // A.4.1.1 Module instantiation
    //

    /// Try to tell which item does an identifier begin by looking ahead.
    fn disambiguate_item(&mut self) -> ItemDAB {
        let mut peek = 1;
        let mut has_hash = false;
        // This is an interface port
        if let TokenKind::Operator(Operator::Dot) = **self.peek_n(1) {
            return ItemDAB::IntfPort
        }
        // Skip over parameter assignment if any
        if let TokenKind::Hash = **self.peek_n(1) {
            has_hash = true;
            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(2) {
                peek = 3;
            } else {
                // Hash a hash but no parenthesis, then the hash should be delay control.
                // This is therefore a net_declaration
                return ItemDAB::NetDecl
            }
        }
        // For net declaration and instantiation, the next one must be an identifier
        if let TokenKind::Id(_) = **self.peek_n(peek) {
            peek += 1
        } else {
            return ItemDAB::DataDecl
        }
        // We must skip over dimension list if any
        while let TokenKind::DelimGroup(Delim::Bracket, _) = **self.peek_n(peek) {
            peek += 1
        }
        // Now we expect a opening paranthesis
        if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(peek) {
            // We've found an instantiation if we see parenthesis
            ItemDAB::HierInst
        } else {
            // Otherwise, if hash is seen, it's actually a parenthesised delay control.
            // This is a data declaration or net declaration, but as the former one is most common
            // we will treat it as data declaration, as it is not hard to convert if semantic
            // analysis found out otherwise
            if has_hash { ItemDAB::NetDecl } else { ItemDAB::DataDecl }
        }
    }

    /// Parse an instantiation.
    ///
    /// ```bnf
    /// instantiation ::=
    ///   identifier [ parameter_value_assignment ] hierarchical_instance
    ///   { , hierarchical_instance } ;
    /// ```
    fn parse_instantiation(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        let mod_name = self.expect_id();

        // Parse parameter value assignment
        let param = if self.check(TokenKind::Hash) {
            Some(self.parse_args(ArgOption::Param))
        } else {
            None
        };

        let list = self.parse_comma_list(false, false, |this| {
            let name = match this.consume_if_id() {
                None => return None,
                Some(v) => v,
            };

            let mut dim = this.parse_list(Self::parse_dim_opt);
            this.check_list(Self::check_unpacked_dim, &mut dim);
            let ports = this.parse_args(ArgOption::Port);
            Some(HierInst {
                name,
                dim,
                ports,
            })
        });

        self.expect(TokenKind::Semicolon);

        Item::HierInstantiation(Box::new(
            HierInstantiation {
                attr,
                param,
                name: mod_name,
                inst: list,
            }
        ))
    }

    /// Combined parser of parameter value assignment, port connections, and method argument list.
    ///
    /// ```bnf
    /// list_of_parameter_assignments ::=
    ///   ordered_parameter_assignment { , ordered_parameter_assignment }
    /// | named_parameter_assignment { , named_parameter_assignment }
    /// ordered_parameter_assignment ::=
    ///   param_expression
    /// named_parameter_assignment ::=
    ///   . parameter_identifier ( [ param_expression ] )
    /// list_of_arguments ::=
    ///   [ expression ] { , [ expression ] } { , . identifier ( [ expression ] ) }
    /// | . identifier ( [ expression ] ) { , . identifier ( [ expression ] ) }
    /// ```
    fn parse_args_opt(&mut self, option: ArgOption) -> Option<Vec<Arg>> {
        let mut named_seen = false;
        let mut ordered_seen = false;
        self.parse_if_delim(Delim::Paren, |this| {
            this.parse_comma_list(true, false, |this| {
                let attr = this.parse_attr_inst_opt();
                if option != ArgOption::Port && attr.is_some() {
                    this.report_span(
                        Severity::Error,
                        "attribute instances on argument is not allowed",
                        attr.as_ref().unwrap().span
                    );
                }
                if let Some(v) = this.consume_if(TokenKind::Operator(Operator::WildPattern)) {
                    if option != ArgOption::Port {
                        this.report_span(
                            Severity::Error,
                            ".* not allowed as argument",
                            v.span
                        );
                    }
                    named_seen = true;
                    Some(Arg::NamedWildcard(attr))
                } else if let Some(v) = this.consume_if(TokenKind::Operator(Operator::Dot)) {
                    let name = this.expect_id();
                    let expr = this.parse_delim_spanned(Delim::Paren, Self::parse_expr_opt);
                    if ordered_seen && option != ArgOption::Arg {
                        this.report_span(
                            Severity::Error,
                            "mixture of ordered and named argument is not allowed",
                            v.span.merge(expr.span)
                        );
                    }
                    named_seen = true;
                    Some(Arg::Named(attr, Box::new(name), expr.value.map(Box::new)))
                } else {
                    let expr = this.parse_expr_opt().map(Box::new);
                    if named_seen {
                        if let Some(expr) = &expr {
                            this.report_span(
                                Severity::Error,
                                "ordered argument cannot appear after named argument",
                                expr.span
                            );
                        } else {
                            // Return None so error message will be about trailing comma.
                            return None
                        }
                    }
                    ordered_seen = true;
                    Some(Arg::Ordered(attr, expr))
                }
            })
        })
    }

    fn parse_args(&mut self, option: ArgOption) -> Vec<Arg> {
        self.parse_unwrap(|this| this.parse_args_opt(option))
    }

    //
    // A.4.2 Generate instantiations
    //

    /// Parse a loop_generate_construct
    /// ```bnf
    /// loop_generate_construct ::=
    ///   for ( genvar_initialization ; genvar_expression ; genvar_iteration ) generate_block
    /// ```
    fn parse_loop_gen(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        // Eat the for keyword
        self.consume();
        let (genvar, id, init, cond, update) = 
            self.parse_delim(Delim::Paren, |this| {
                let genvar = this.check(TokenKind::Keyword(Keyword::Genvar));
                let id = this.expect_id();
                this.expect(TokenKind::Operator(Operator::Assign));
                let init = this.parse_expr();
                this.expect(TokenKind::Semicolon);
                let cond = this.parse_expr();
                this.expect(TokenKind::Semicolon);
                let update = this.parse_expr();
                (genvar, id, init, cond, update)
            });
        let block = self.parse_gen_block();
        Item::LoopGen(Box::new(LoopGen {
            attr,
            genvar,
            id,
            init,
            cond,
            update,
            block,
        }))
    }

    /// Parse a if_generate_construct
    /// ```bnf
    /// if_generate_construct ::=
    ///   if ( constant_expression ) generate_block [ else generate_block ]
    /// ```
    fn parse_if_gen(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        // Eat the if keyword
        self.consume();
        let cond = self.parse_delim(Delim::Paren, Self::parse_expr);
        let true_block = self.parse_gen_block();
        let false_block = if self.check(TokenKind::Keyword(Keyword::Else)) {
            Some(Box::new(self.parse_gen_block()))
        } else {
            None
        };

        Item::IfGen(Box::new(IfGen {
            attr,
            cond,
            true_block,
            false_block,
        }))
    }

    fn parse_gen_block(&mut self) -> Item {
        // A generate-block may begin with a label. It is treated as same as label after begin.
        let label = if let TokenKind::Id(_) = **self.peek() {
            if let TokenKind::Colon = **self.peek_n(1) {
                // This is actuall
                if let TokenKind::Keyword(Keyword::Begin) = **self.peek_n(2) {
                    let label = self.expect_id();
                    self.consume();
                    Some(label)
                } else { None }
            } else { None }
        } else { None };
        
        let begin = match self.consume_if(TokenKind::Keyword(Keyword::Begin)) {
            None => return self.parse_item(),
            Some(v) => v,
        };

        let name = if self.check(TokenKind::Colon) {
            Some(self.expect_id())
        } else {
            None
        };

        if let (Some(l), Some(n)) = (&label, &name) {
            if **l != **n {
                // IMP: Add a span about previous name
                self.report_span(
                    Severity::Error,
                    "block identifiers before and after 'begin' are not identical",
                    n.span
                );
            } else {
                self.report_span(
                    Severity::Warning,
                    "duplicate block identifiers before and after 'begin'",
                    n.span
                );
            }
        } else if let (Some(l), None) = (&label, &name) {
            self.report_diag(
                Diagnostic::new(
                    Severity::Warning,
                    "it is suggested to place block identifier after 'begin'",
                    l.span.merge(begin.span)
                ).fix_primary(format!("begin: {}", l))
            );
        }

        let name = name.or(label);
        let items = self.parse_list(Self::parse_item_opt);
        
        self.expect(TokenKind::Keyword(Keyword::End));

        if self.check(TokenKind::Colon) {
            let id = self.expect_id();
            match &name {
                None => self.report_span(
                    Severity::Error,
                    "identifer annotation at end does match declaration, should be empty",
                    id.span
                ),
                Some(v) => if **v != *id {
                    self.report_span(
                        Severity::Error,
                        format!("identifer annotation at end does match declaration, should be '{}'", v),
                        id.span
                    )
                }
            }
        }

        Item::GenBlock(Box::new(GenBlock {
            name: name.map(Box::new),
            items,
        }))
    }

    //
    // A.6.1 Continuous assignment and net alias statements
    //
    fn parse_continuous_assign(&mut self) -> Item {
        self.consume();
        // IMP: Parse drive_strength
        // IMP: Parse delay control
        let assignments = self.parse_comma_list(false, false, Self::parse_assign_expr);
        self.expect(TokenKind::Semicolon);
        Item::ContinuousAssign(assignments)
    }

    //
    // A.6.2 Procedural blocks and assignments
    //

    fn is_assign_op(op: Operator) -> bool {
        match op {
            Operator::Assign |
            Operator::AddEq |
            Operator::SubEq |
            Operator::MulEq |
            Operator::DivEq |
            Operator::ModEq |
            Operator::AndEq |
            Operator::OrEq |
            Operator::XorEq |
            Operator::LShlEq |
            Operator::LShrEq |
            Operator::AShlEq |
            Operator::AShrEq => true,
            _ => false,
        }
    }

    fn parse_always(&mut self) -> Item {
        let kw = if let TokenKind::AlwaysKw(kw) = *self.consume() {
            kw
        } else {
            unreachable!();
        };
        let stmt = self.parse_stmt();
        Item::Always(kw, Box::new(stmt))
    }

    //
    // A.6.3 Parallel and sequential blocks
    //
    fn parse_seq_block(&mut self, label: &mut Option<Ident>) -> StmtKind {
        let begin = self.consume();
        if self.check(TokenKind::Colon) {
            let id = self.expect_id();
            if let Some(v) = label {
                if *id != **v {
                    // IMP: Add a span about previous name
                    self.report_span(
                        Severity::Error,
                        "block identifiers before and after 'begin' are not identical",
                        id.span
                    );
                } else {
                    self.report_span(
                        Severity::Warning,
                        "duplicate block identifiers before and after 'begin'",
                        id.span
                    );
                }
            }
            *label = Some(id);
        } else if let Some(v) = label {
            self.report_diag(
                Diagnostic::new(
                    Severity::Warning,
                    "it is suggested to place block identifier after 'begin'",
                    v.span.merge(begin.span)
                ).fix_primary(format!("begin: {}", v))
            );
        }

        let items = self.parse_list(Self::parse_stmt_opt);
        
        self.expect(TokenKind::Keyword(Keyword::End));

        if self.check(TokenKind::Colon) {
            let id = self.expect_id();
            match label {
                None => self.report_span(
                    Severity::Error,
                    "identifer annotation at end does match declaration, should be empty",
                    id.span
                ),
                Some(v) => if **v != *id {
                    self.report_span(
                        Severity::Error,
                        format!("identifer annotation at end does match declaration, should be '{}'", v),
                        id.span
                    )
                }
            }
        }

        StmtKind::SeqBlock(items)
    }

    //
    // A.6.4 Statements
    //

    /// Parse a block item declaration, statement, or null statement.
    fn parse_stmt_opt(&mut self) -> Option<Stmt> {
        // These are common to all statements:
        // an optional identifier and an attribute.
        let mut label = if let TokenKind::Id(_) = **self.peek() {
            if let TokenKind::Colon = **self.peek_n(1) {
                let id = self.expect_id();
                self.consume();
                Some(id)
            } else { None }
        } else { None };
        let attr = self.parse_attr_inst_opt();

        let kind = match **self.peek() {
            TokenKind::Keyword(Keyword::End) => return None,
            // null_statement
            TokenKind::Semicolon => {
                self.consume();
                StmtKind::Empty
            }
            // conditional_statement or case_statement
            TokenKind::UniqPrio(uniq) => {
                let prio = self.consume();
                match **self.peek() {
                    TokenKind::Keyword(Keyword::If) => self.parse_if_stmt(Some(uniq)),
                    // TODO: case
                    _ => {
                        self.report_span(
                            Severity::Error,
                            "expected if or case statement after unique, unique0 or priority",
                            prio.span
                        );
                        // Error recovery
                        StmtKind::Empty
                    }
                }
            }
            // conditional_statement
            TokenKind::Keyword(Keyword::If) => self.parse_if_stmt(None),
            // seq_block
            TokenKind::Keyword(Keyword::Begin) => self.parse_seq_block(&mut label),
            // procedural_timing_control_statement
            TokenKind::Hash |
            TokenKind::CycleDelay |
            TokenKind::AtStar |
            TokenKind::At => self.parse_timing_ctrl_stmt(),
            _ => {
                let expr = self.parse_unwrap(Self::parse_assign_expr);
                self.expect(TokenKind::Semicolon);
                StmtKind::Expr(Box::new(expr))
            }
        };

        Some(Stmt {
            label: label.map(Box::new),
            attr,
            value: kind,
        })
    }

    fn parse_stmt(&mut self) -> Stmt {
        self.parse_unwrap(Self::parse_stmt_opt)
    }

    //
    // A.6.5 Timing control statements
    //

    fn parse_timing_ctrl_stmt(&mut self) -> StmtKind {
        let ctrl = self.parse_timing_ctrl();
        let stmt = self.parse_stmt();
        StmtKind::TimingCtrl(ctrl, Box::new(stmt))
    }

    fn parse_timing_ctrl(&mut self) -> TimingCtrl {
        match **self.peek() {
            TokenKind::Hash => unimplemented!(),
            TokenKind::CycleDelay => unimplemented!(),
            TokenKind::AtStar => {
                self.consume();
                TimingCtrl::ImplicitEventCtrl
            }
            TokenKind::At => {
                self.consume();
                match **self.peek() {
                    TokenKind::ParenedStar => {
                        self.consume();
                        TimingCtrl::ImplicitEventCtrl
                    }
                    TokenKind::DelimGroup(Delim::Paren, _) => {
                        TimingCtrl::ExprEventCtrl(
                            Box::new(self.parse_delim(Delim::Paren, Self::parse_event_expr))
                        )
                    }
                    _ => {
                        let scope = self.parse_scope();
                        let id = self.parse_unwrap(Self::parse_hier_id);
                        TimingCtrl::NameEventCtrl(
                            scope, id
                        )
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_event_expr_item(&mut self) -> EventExpr {
        if let Some(v) = self.parse_if_delim(Delim::Paren, Self::parse_event_expr) {
            return EventExpr::Paren(Box::new(v))
        }
        let edge = if let TokenKind::Edge(edge) = **self.peek() {
            self.consume();
            Some(edge)
        } else {
            None
        };
        let expr = Box::new(self.parse_expr());
        let iff = if self.check(TokenKind::Keyword(Keyword::Iff)) {
            Some(Box::new(self.parse_expr()))
        } else {
            None
        };
        EventExpr::Item(Box::new(EventExprItem {
            edge,
            expr,
            iff,
        }))
    }

    fn parse_event_expr(&mut self) -> EventExpr {
        let mut item = vec![self.parse_event_expr_item()];
        while self.check(TokenKind::Comma) || self.check(TokenKind::Keyword(Keyword::Or)) {
            item.push(self.parse_event_expr_item());
        }
        if item.len() > 1 {
            EventExpr::List(item)
        } else {
            item.pop().unwrap()
        }
    }

    //
    // A.6.6 Conditional statements
    //
    fn parse_if_stmt(&mut self, uniq: Option<UniqPrio>) -> StmtKind {
        self.consume();
        let cond = Box::new(self.parse_delim(Delim::Paren, Self::parse_expr));
        let true_stmt = Box::new(self.parse_stmt());
        let false_stmt = if self.check(TokenKind::Keyword(Keyword::Else)) {
            // TODO: Else if clause cannot begin with UniqPrio
            Some(Box::new(self.parse_stmt()))
        } else {
            None
        };
        StmtKind::If(uniq, cond, true_stmt, false_stmt)
    }

    //
    // A.8.2 Subroutine calls
    //

    fn parse_sys_tf_call(&mut self) -> SysTfCall {
        let task = {
            let token = self.consume();
            match token.value {
                TokenKind::SystemTask(name) => Spanned::new(name, token.span),
                _ => unreachable!(),
            }
        };
        let args = self.parse_args_opt(ArgOption::Arg);
        SysTfCall {
            task,
            args,
        }
    }

    //
    // A.8.3 Expressions
    //

    /// Parse an assignment expression
    ///
    /// Our rearrangement
    /// ```bnf
    /// assignment_expression ::=
    ///   expression
    /// | expression assignment_operator expression
    /// ```
    fn parse_assign_expr(&mut self) -> Option<Expr> {
        let expr = match self.parse_expr_opt() {
            None => return None,
            Some(v) => v,
        };

        match **self.peek() {
            TokenKind::Operator(op) if Self::is_assign_op(op) => {
                self.consume();
                let rhs = self.parse_expr();
                let span = expr.span.merge(rhs.span);
                Some(Spanned::new(ExprKind::Assign(Box::new(expr), op, Box::new(rhs)), span))
            }
            _ => Some(expr)
        }
    }

    /// Parse an expression (or data_type)
    ///
    /// In addition to "expression" and "data_type_or_implicit" defined in spec, we additionally
    /// parse "delay_or_event_control", "dynamic_array_new" and "class_new" (not yet stubbed) in
    /// procedural assignment context. We also parse "cond_predicate".
    ///
    /// According to the spec:
    /// ```bnf
    /// expression ::=
    ///   primary
    /// | unary_operator { attribute_instance } primary
    /// | inc_or_dec_expression
    /// | ( operator_assignment )
    /// | expression binary_operator { attribute_instance } expression
    /// | conditional_expression
    /// | inside_expression
    /// | tagged_union_expression
    /// ```
    ///
    /// Our rearrangement:
    /// ```bnf
    /// expression ::=
    ///   condition_expression
    /// | expression [ -> | <-> ] expression
    /// condition_expression ::=
    ///   cond_predicate
    /// | cond_predicate ? { attribute_instance } expression : expression
    /// cond_predicate ::=
    ///   cond_pattern
    /// | cond_predicate &&& cond_pattern
    /// cond_pattern ::=
    ///   tagged_union_expression [ matches pattern ]
    /// | binary_expression [ matches pattern ]
    /// binary_expression ::=
    ///   unary_expression
    /// | binary_expression binary_operator { attribute_instance } expression
    /// | inside_expression
    /// | dist_expression
    /// unary_expression ::=
    ///   primary
    /// | unary_operator { attribute_instance } primary
    /// | inc_or_dec_expression
    /// ```
    fn parse_expr_opt(&mut self) -> Option<Expr> {
        let lhs = self.parse_cond_expr_opt()?;
        match **self.peek() {
            TokenKind::Operator(Operator::Implies) |
            TokenKind::Operator(Operator::Equiv) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "-> and <-> not yet supported", span);
                unreachable!();
            }
            _ => Some(lhs),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_unwrap(Self::parse_expr_opt)
    }

    fn parse_cond_expr_opt(&mut self) -> Option<Expr> {
        let expr = self.parse_cond_pred_opt()?;
        if self.check(TokenKind::Operator(Operator::Question)) {
            let attr = self.parse_attr_inst_opt();
            let true_expr = Box::new(self.parse_expr());
            self.expect(TokenKind::Colon);
            let false_expr = Box::new(self.parse_expr());
            let span = expr.span.merge(false_expr.span);
            Some(Spanned::new(
                ExprKind::Cond(Box::new(expr), attr, true_expr, false_expr), span
            ))
        } else {
            Some(expr)
        }
    }

    fn parse_cond_pred_opt(&mut self) -> Option<Expr> {
        let expr = self.parse_cond_pattern_opt()?;
        if self.check(TokenKind::Operator(Operator::TripleAnd)) {
            let span = self.peek().span;
            self.report_span(Severity::Fatal, "expression_or_cond_pattern not yet supported", span);
            unreachable!();
        } else {
            Some(expr)
        }
    }

    fn parse_cond_pattern_opt(&mut self) -> Option<Expr> {
        let expr = match **self.peek() {
            // tagged_union_expression
            TokenKind::Keyword(Keyword::Tagged) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "tagged_union_expression not yet supported", span);
                unreachable!();
            }
            _ => self.parse_bin_expr(0)?,
        };
        if self.check(TokenKind::Keyword(Keyword::Matches)) {
            let span = self.peek().span;
            self.report_span(Severity::Fatal, "cond_pattern not yet supported", span);
            unreachable!();
        } else {
            Some(expr)
        }
    }
    
    /// Parse binary expression using precedence climing method which saves stack space.
    fn parse_bin_expr(&mut self, prec: i32) -> Option<Expr> {
        let mut expr = match self.parse_unary_expr() {
            None => return None,
            Some(v) => v,
        };

        loop {

            let (op, new_prec) = match **self.peek() {
                TokenKind::Operator(op) => {
                    match Self::get_bin_op_prec(op) {
                        // Can only proceed if precedence is higher
                        Some(v) if v > prec => (op, v),
                        _ => break,
                    }
                }
                TokenKind::Keyword(Keyword::Inside) |
                TokenKind::Keyword(Keyword::Dist) if 7 > prec => {
                    // 7 is the precedence of comparison operator.
                    let span = self.peek().span;
                    self.report_span(Severity::Fatal, "inside & dist not yet supported", span);
                    unreachable!();
                }
                _ => break,
            };

            self.consume();
            let attr = self.parse_attr_inst_opt();
            let rhs = self.parse_unwrap(|this| this.parse_bin_expr(new_prec));
            let span = expr.span.merge(rhs.span);
            expr = Spanned::new(ExprKind::Binary(Box::new(expr), op, attr, Box::new(rhs)), span);
        }

        Some(expr)
    }

    fn parse_unary_expr(&mut self) -> Option<Expr> {
        match **self.peek() {
            // inc_or_dec_operator { attribute_instance } variable_lvalue
            // unary_operator { attribute_instance } primary
            TokenKind::Operator(op) if Self::is_prefix_operator(op) => {
                let span = self.consume().span;
                let attr = self.parse_attr_inst_opt();
                let expr = self.parse_unwrap(Self::parse_primary);
                let span = span.merge(expr.span);
                Some(Spanned::new(ExprKind::Unary(op, attr, Box::new(expr)), span))
            }
            _ => {
                let expr = self.parse_primary()?;
                match **self.peek() {
                    // Inc/dec with attributes
                    TokenKind::DelimGroup(Delim::Attr, _) => {
                        match **self.peek_n(1) {
                            TokenKind::Operator(e @ Operator::Inc) |
                            TokenKind::Operator(e @ Operator::Dec) => {
                                let attr = self.parse_attr_inst_opt();
                                let span = expr.span.merge(self.consume().span);
                                Some(Spanned::new(ExprKind::PostfixIncDec(Box::new(expr), attr, e), span))
                            }
                            _ => Some(expr),
                        }
                    }
                    // Inc/dec without attributes
                    TokenKind::Operator(e @ Operator::Inc) |
                    TokenKind::Operator(e @ Operator::Dec) => {
                        let span = expr.span.merge(self.consume().span);
                        Some(Spanned::new(ExprKind::PostfixIncDec(Box::new(expr), None, e), span))
                    }
                    _ => Some(expr),
                }
            }
        }
    }

    /// Parse mintypmax expression
    ///
    /// ```bnf
    /// mintypmax_expression ::=
    ///   expression | expression : expression : expression
    /// ```
    fn parse_mintypmax_expr(&mut self) -> Option<Expr> {
        let expr = self.parse_expr_opt()?;
        if !self.check(TokenKind::Colon) {
            return Some(expr)
        }
        let typ = Box::new(self.parse_expr());
        self.expect(TokenKind::Colon);
        let max = Box::new(self.parse_expr());
        let span = expr.span.merge(max.span);
        Some(Spanned::new(ExprKind::MinTypMax(Box::new(expr), typ, max), span))
    }

    //
    // A.8.4 Primaries
    //

    /// Parse primary expression (or data_type) with cast.
    fn parse_primary(&mut self) -> Option<Expr> {
        let mut expr = match **self.peek() {
            TokenKind::Keyword(Keyword::Const) => {
                let span = self.consume().span;
                self.expect(TokenKind::Operator(Operator::Tick));
                let expr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt));
                Spanned::new(ExprKind::ConstCast(Box::new(expr.value)), span.merge(expr.span))
            }
            TokenKind::Signing(sign) => {
                if let TokenKind::Operator(Operator::Tick) = **self.peek_n(1) {
                    let span = self.consume().span;
                    self.expect(TokenKind::Operator(Operator::Tick));
                    let expr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt));
                    Spanned::new(ExprKind::SignCast(sign, Box::new(expr.value)), span.merge(expr.span))
                } else {
                    match self.parse_primary_nocast() {
                        None => return None,
                        Some(v) => v,
                    }
                }
            }
            _ => match self.parse_primary_nocast() {
                None => return None,
                Some(v) => v,
            }
        };
        loop {
            if self.consume_if(TokenKind::Operator(Operator::Tick)).is_none() {
                break
            }

            let nexpr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt));
            let span = expr.span.merge(nexpr.span);
            expr = Spanned::new(ExprKind::TypeCast(Box::new(expr), Box::new(nexpr.value)), span)
        }
        Some(expr)
    }

    /// Parse primary expression (or data_type), except for cast. Cast is special as it can take
    /// form `primary '(expr)` which introduces left recursion.
    ///
    /// According to spec
    /// ```bnf
    /// primary ::=
    ///   primary_literal
    /// | [ class_qualifier | package_scope ] hierarchical_identifier select
    /// | empty_queue
    /// | concatenation [ [ range_expression ] ]
    /// | multiple_concatenation [ [ range_expression ] ]
    /// | function_subroutine_call
    /// | let_expression
    /// | ( mintypmax_expression )
    /// | cast
    /// | assignment_pattern_expression
    /// | streaming_concatenation
    /// | sequence_method_call
    /// | this
    /// | $
    /// | null
    /// | data_type
    /// ```
    fn parse_primary_nocast(&mut self) -> Option<Expr> {
        match **self.peek() {
            // Case where this isn't an expression
            TokenKind::Eof => None,
            // primary_literal
            // $
            // null
            TokenKind::RealLiteral(_) |
            TokenKind::IntegerLiteral(_) |
            TokenKind::TimeLiteral(_) |
            TokenKind::UnbasedLiteral(_) |
            TokenKind::StringLiteral(_) | 
            TokenKind::Operator(Operator::Dollar) |
            TokenKind::Keyword(Keyword::Null) => {
                let tok = self.consume();
                let sp = tok.span;
                Some(Spanned::new(ExprKind::Literal(tok), sp))
            }
            // empty_queue
            // concatenation [ [ range_expression ] ]
            // multiple_concatenation [ [ range_expression ] ]
            // streaming_concatenation
            TokenKind::DelimGroup(Delim::Brace, _) => {
                Some(self.parse_delim_spanned(Delim::Brace, |this| {
                    match **this.peek() {
                        TokenKind::Eof => ExprKind::EmptyQueue,
                        TokenKind::Operator(Operator::LShr) |
                        TokenKind::Operator(Operator::LShl) => {
                            let span = this.peek().span;
                            this.report_span(Severity::Fatal, "streaming concat is not yet supported", span);
                            unreachable!();
                        }
                        _ => {
                            let expr = this.parse_expr();
                            if let TokenKind::DelimGroup(Delim::Brace, _) = **this.peek() {
                                // This is a multiple concatenation
                                let concat = this.parse_unwrap(Self::parse_primary_nocast);
                                ExprKind::MultConcat(Box::new(expr), Box::new(concat))
                            } else {
                                let mut list = vec![expr];
                                if this.check(TokenKind::Comma) {
                                    this.parse_comma_list_unit(false, false, |this| {
                                        match this.parse_expr_opt() {
                                            None => false,
                                            Some(v) => {
                                                list.push(v);
                                                true
                                            }
                                        }
                                    });
                                }
                                ExprKind::Concat(list)
                            }
                        }
                    }
                }))
            }
            // assignment_pattern_expression
            TokenKind::DelimGroup(Delim::TickBrace, _) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "assign pattern is not finished yet", span);
                unreachable!();
            }
            // ( mintypmax_expression )
            TokenKind::DelimGroup(Delim::Paren, _) => {
                Some(self.parse_delim_spanned(Delim::Paren, |this| {
                    ExprKind::Paren(Box::new(this.parse_unwrap(Self::parse_mintypmax_expr)))
                }))
            }
            // system_tf_call
            TokenKind::SystemTask(_) => {
                let tf = self.parse_sys_tf_call();
                // IMP: better span
                let span = tf.task.span.start.span_to(self.peek().span.start);
                Some(Spanned::new(ExprKind::SysTfCall(Box::new(tf)), span))
            }
            // The left-over possibilities are:
            // [ class_qualifier | package_scope ] hierarchical_identifier select
            // function_subroutine_call
            // let_expression
            // cast
            // sequence_method_call
            // this
            // data_type
            // We cannot really distinguish between them directly. But we noted they all begin
            // with a hierachical name (or keyword typename). So we parse it first, and then try
            // to parse the rest as postfix operation.
            // Keyword type names, parse as data type
            TokenKind::DelimGroup(Delim::Bracket, _) |
            TokenKind::Keyword(Keyword::Type) |
            TokenKind::IntAtomTy(_) |
            TokenKind::IntVecTy(_) |
            TokenKind::Keyword(Keyword::Reg) |
            TokenKind::NonIntTy(_) |
            TokenKind::Keyword(Keyword::Void) => {
                let ty = Box::new(self.parse_kw_data_type().unwrap());
                let span = ty.span;
                Some(Spanned::new(ExprKind::Type(ty), span))
            }
            TokenKind::Keyword(kw) if Self::is_keyword_typename(kw)  => {
                let ty = Box::new(self.parse_kw_data_type().unwrap());
                let span = ty.span;
                Some(Spanned::new(ExprKind::Type(ty), span))
            }
            // Otherwise, parse as name
            _ => {
                let begin_span = self.peek().span;
                let scope = self.parse_scope();
                let mut id = self.parse_hier_id();

                // Not a primary expressison
                if scope.is_none() && id.is_none() {
                    None
                } else {
                    // If we've seen the scopes then we must need to see the id
                    if scope.is_some() && id.is_none() {
                        let span = self.peek().span;
                        self.report_span(Severity::Error, "expected identifiers after scope", span);
                        // Error recovery
                        id = Some(HierId::Name(None, Box::new(Spanned::new_unspanned("".to_owned()))))
                    }
                    // TODO: This is a hack. Could do better
                    let span = begin_span.start.span_to(self.peek().span.end);
                    let mut expr = Spanned::new(ExprKind::HierName(scope, id.unwrap()), span);
                    
                    match **self.peek() {
                        // If next is '{, then this is actually an assignment pattern
                        TokenKind::DelimGroup(Delim::TickBrace, _) => {
                            let span = self.peek().span;
                            self.report_span(Severity::Fatal, "assign pattern is not finished yet", span);
                            unreachable!();
                        }
                        // This can be either function call or inc/dec expression, peek ahead
                        TokenKind::DelimGroup(Delim::Attr, _) => {
                            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(1) {
                                let span = self.peek().span;
                                self.report_span(Severity::Fatal, "function call not finished yet", span);
                                unreachable!();
                            } else {
                                Some(expr)
                            }
                        }
                        // Function call
                        TokenKind::DelimGroup(Delim::Paren, _) => {
                            let span = self.peek().span;
                            self.report_span(Severity::Fatal, "function call not finished yet", span);
                            unreachable!();
                        }
                        // Bit select
                        TokenKind::DelimGroup(Delim::Bracket, _) => Some(self.parse_select(expr)),
                        _ => Some(expr)
                    }
                }
            }
        }
    }

    /// Parse select expression
    /// select ::=
    ///   [ { . member_identifier bit_select } . member_identifier ] bit_select
    /// | [ [ part_select_range ] ]
    fn parse_select(&mut self, mut expr: Expr) -> Expr {
        loop {
            match **self.peek() {
                // Bit select
                TokenKind::DelimGroup(Delim::Bracket, _) => {
                    let sel = self.parse_dim_opt().unwrap();
                    // TODO better end span
                    let span = expr.span.end.span_to(self.peek().span.start);
                    expr = Spanned::new(ExprKind::Select(Box::new(expr), sel), span);
                }
                TokenKind::Operator(Operator::Dot) => {
                    self.consume();
                    let id = self.expect_id();
                    let span = expr.span.merge(id.span);
                    expr = Spanned::new(ExprKind::Member(Box::new(expr), id), span);
                }
                _ => return expr
            }
        }
    }

    //
    // A.8.6 Operators
    //

    fn is_prefix_operator(op: Operator) -> bool {
        match op {
            Operator::Add |
            Operator::Sub |
            Operator::LNot |
            Operator::Not |
            Operator::And |
            Operator::Nand |
            Operator::Or |
            Operator::Nor |
            Operator::Xor |
            Operator::Xnor => true,
            Operator::Inc |
            Operator::Dec => true,
            _ => false,
        }
    }

    /// Get precedence of binary operator. Exclude -> and ->>
    fn get_bin_op_prec(op: Operator) -> Option<i32> {
        match op {
            Operator::Power => Some(11),
            Operator::Mul |
            Operator::Div |
            Operator::Mod => Some(10),
            Operator::Add |
            Operator::Sub => Some(9),
            Operator::LShl |
            Operator::LShr |
            Operator::AShl |
            Operator::AShr => Some(8),
            Operator::Lt |
            Operator::Leq |
            Operator::Gt |
            Operator::Geq => Some(7),
            Operator::Eq |
            Operator::Neq |
            Operator::CaseEq |
            Operator::CaseNeq |
            Operator::WildEq |
            Operator::WildNeq => Some(6),
            Operator::And => Some(5),
            Operator::Xor |
            Operator::Xnor => Some(4),
            Operator::Or => Some(3),
            Operator::LAnd => Some(2),
            Operator::LOr => Some(1),
            _ => None,
        }
    }

    //
    // A.9.1 Attributes
    //
    fn parse_attr_inst_opt(&mut self) -> Option<Box<AttrInst>> {
        let attr = self.parse_if_delim_spanned(Delim::Attr, |this| {
            AttrInstStruct(
                this.parse_comma_list(false, false, |this| {
                    match this.consume_if_id() {
                        None => None,
                        Some(name) => {
                            let expr = if this.check(TokenKind::Operator(Operator::Assign)) {
                                Some(Box::new(this.parse_expr()))
                            } else {
                                None
                            };
                            Some(AttrSpec {
                                name,
                                expr
                            })
                        }
                    }
                })
            )
        });
        attr.map(Box::new)
    }

    //
    // A.9.3 Identifiers
    //

    /// Parse scope. This is more generous than any scoped names in SystemVerilog spec.
    /// ```bnf
    /// [ local :: | $unit :: ] [ identifier [ parameter_value_assignment ] :: ]
    /// ``` 
    fn parse_scope(&mut self) -> Option<Scope> {
        let mut scope = None;
        loop {
            match **self.peek() {
                TokenKind::Keyword(Keyword::Local) => {
                    let tok = self.consume();
                    if let Some(_) = scope {
                        self.report_span(Severity::Error, "local scope can only be the outermost scope", tok.span);
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep));
                }
                TokenKind::Keyword(Keyword::Unit) => {
                    let tok = self.consume();
                    if let Some(_) = scope {
                        self.report_span(Severity::Error, "$unit scope can only be the outermost scope", tok.span);
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep));
                }
                TokenKind::Id(_) => {
                    // Lookahead to check if this is actually a scope
                    match **self.peek_n(1) {
                        TokenKind::Operator(Operator::ScopeSep) => (),
                        TokenKind::Hash => {
                            if let TokenKind::DelimGroup(Delim::Paren,_) = **self.peek_n(2) {
                                if let TokenKind::Operator(Operator::ScopeSep) = **self.peek_n(3) {
                                } else {
                                    break
                                }
                            } else {
                                break
                            }
                        }
                        _ => break,
                    };
                    let ident = self.expect_id();
                    if self.consume_if(TokenKind::Hash).is_some() {
                        // TODO: Add parameter support
                        self.report_span(Severity::Fatal, "class parameter scope is not yet supported", ident.span);
                        unreachable!();
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep));
                    scope = Some(Scope::Name(scope.map(Box::new), Box::new(ident)))
                }
                _ => break,
            }
        }
        scope
    }

    /// Parse hierachical identifier
    fn parse_hier_id(&mut self) -> Option<HierId> {
        let mut id = None;
        self.parse_sep_list_unit(Operator::Dot, true, false, |this| {
            match **this.peek() {
                TokenKind::Keyword(Keyword::This) => {
                    let tok = this.consume();
                    if let Some(_) = id {
                        this.report_span(Severity::Error, "this can only be the outermost identifier", tok.span);
                    } else {
                        id = Some(HierId::This)
                    }
                }
                TokenKind::Keyword(Keyword::Super) => {
                    let tok = this.consume();
                    match id {
                        None | Some(HierId::This) => id = Some(HierId::Super),
                        Some(_) => {
                            this.report_span(Severity::Error, "super can only be the outermost identifier", tok.span);
                        }
                    }
                }
                TokenKind::Keyword(Keyword::Root) => {
                    let tok = this.consume();
                    if let Some(_) = id {
                        this.report_span(Severity::Error, "$root can only be the outermost identifier", tok.span);
                    } else {
                        id = Some(HierId::Root)
                    }
                }
                TokenKind::Id(_) => {
                    id = Some(HierId::Name(
                        // Hack to move id out temporarily
                        mem::replace(&mut id, None).map(Box::new),
                        Box::new(this.expect_id())
                    ))
                }
                _ => return false
            }
            true
        });
        id
    }
}