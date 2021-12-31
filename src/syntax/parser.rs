use super::super::source::{DiagMgr, Diagnostic, Pos, Severity, Span};
use super::ast::*;
use super::tokens::*;

use std::borrow::Borrow;
use std::collections::VecDeque;
use std::mem;

pub fn parse<'a>(diag: &'a DiagMgr, lexer: VecDeque<Token>) -> Vec<Item> {
    Parser::new(diag, lexer).parse_source()
}

struct Parser<'a> {
    diag: &'a DiagMgr,
    lexer: VecDeque<Token>,
    eof: Token,
    leq_as_assign: bool,
}

//
// Data types internal to parser
//

/// Disambiguated item that can possibly start with identifier.
enum ItemDAB {
    DataDecl,
    HierInst,
    IntfPort,
    NetDecl,
}

/// SystemVerilog Parser.
///
/// The complexity of SystemVerilog comes from determining what an identifer means. There are few
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
impl<'a> Parser<'a> {
    fn new(diag: &'a DiagMgr, lexer: VecDeque<Token>) -> Parser<'a> {
        let last_pos = lexer.back().map(|x| x.span.end).unwrap_or(Pos(0));
        Parser {
            diag,
            lexer: lexer,
            eof: Spanned::new(TokenKind::Eof, last_pos.span_to(last_pos)),
            leq_as_assign: false,
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
        &mut self,
        mut stream: Box<DelimGroup>,
        mut f: F,
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
            Some(Ident::new(name, toksp.span))
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
                self.diag.report_span(
                    Severity::Error,
                    format!("expected open delimiter {:#?}", expected),
                    span.clone(),
                );
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
        if let TokenKind::Eof = self.peek().value {
        } else {
            let span = self.peek().span;
            self.diag
                .report_span(Severity::Error, "unexpected extra token", span);
        }
    }

    fn expect_id(&mut self) -> Ident {
        match self.consume_if_id() {
            None => {
                let span = self.peek().span.clone();
                self.diag
                    .report_span(Severity::Error, "expected identifier", span.clone());
                // Error recovery
                Ident::new("".to_owned(), span)
            }
            Some(v) => v,
        }
    }

    fn expect<T: Borrow<TokenKind>>(&mut self, token: T) -> Token {
        let token = token.borrow();
        match self.consume_if(token) {
            None => {
                let span = self.peek().span.clone();
                self.diag.report_span(
                    Severity::Error,
                    format!("expected token {:?}", token),
                    span.clone(),
                );
                // Error recovery
                Spanned::new(TokenKind::Unknown, span)
            }
            Some(v) => v,
        }
    }

    fn unimplemented(&mut self) -> ! {
        let span = self.peek().span;
        self.diag.report_fatal("not yet implemented", span);
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
                        self.diag.report_fatal(
                            format!("{} support is not completed yet", T::name()),
                            span,
                        );
                    }
                    Some(v) => {
                        self.diag
                            .report_error(format!("expected {}", T::name()), span);
                        v
                    }
                }
            }
            Some(v) => v,
        }
    }

    /// Unwrap `Option` with sensible error message
    fn parse_unwrap<T: AstNode, F: FnOnce(&mut Self) -> Option<T>>(&mut self, f: F) -> T {
        let result = f(self);
        self.unwrap(result)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim<T, F: FnMut(&mut Self) -> T>(&mut self, delim: Delim, f: F) -> T {
        let delim = self.expect_delim(delim);
        self.delim_group(delim, f)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim_spanned<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        delim: Delim,
        f: F,
    ) -> Spanned<T> {
        let span = self.peek().span;
        let delim = self.expect_delim(delim);
        Spanned::new(self.delim_group(delim, f), span)
    }

    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim<T, F: FnMut(&mut Self) -> T>(&mut self, delim: Delim, f: F) -> Option<T> {
        match self.consume_if_delim(delim) {
            None => None,
            Some(v) => Some(self.delim_group(v, f)),
        }
    }

    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim_spanned<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        delim: Delim,
        f: F,
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
        &mut self,
        empty: bool,
        trail: bool,
        mut f: F,
    ) -> Vec<T> {
        let mut vec = Vec::new();

        // Parse first element
        let result = f(self);
        match result {
            None => {
                // If we failed and this is the first element, then we get an empty list
                if !empty {
                    let span = self.peek().span.clone();
                    self.diag
                        .report_span(Severity::Error, "empty list not allowed", span);
                }
                return vec;
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
                        self.diag.report_span(
                            Severity::Error,
                            "trailing comma is not allowed; consider removing it",
                            comma.span,
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
        &mut self,
        empty: bool,
        trail: bool,
        mut f: F,
    ) {
        // Parse first element
        if !f(self) {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span.clone();
                self.diag
                    .report_span(Severity::Error, "empty list not allowed", span);
            }
            return;
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
                    self.diag.report_span(
                        Severity::Error,
                        "trailing comma is not allowed; consider removing it",
                        comma.span,
                    );
                }
                break;
            }
        }
    }

    /// Parse a seperated list, but do not attempt to build a vector.
    fn parse_sep_list_unit<F: FnMut(&mut Self) -> bool>(
        &mut self,
        sep: TokenKind,
        empty: bool,
        trail: bool,
        mut f: F,
    ) {
        // Parse first element
        if !f(self) {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span;
                self.diag
                    .report_span(Severity::Error, "empty list not allowed", span);
            }
            return;
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(sep.clone()) {
                None => break,
                Some(v) => v,
            };
            if !f(self) {
                if !trail {
                    // TODO: We could place a FixItHint here.
                    self.diag.report_span(
                        Severity::Error,
                        format!("trailing {:#?} is not allowed; consider removing it", sep),
                        comma.span,
                    );
                }
                break;
            }
        }
    }

    /// Check if the list contains invalid elements, and remove them.
    fn check_list<T, F: FnMut(&mut Self, &T) -> bool>(&mut self, list: &mut Vec<T>, mut f: F) {
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
            TokenKind::Eof
            | TokenKind::Keyword(Keyword::Endmodule)
            | TokenKind::Keyword(Keyword::Endprimitive)
            | TokenKind::Keyword(Keyword::Endinterface)
            | TokenKind::Keyword(Keyword::Endprogram)
            | TokenKind::Keyword(Keyword::Endpackage)
            | TokenKind::Keyword(Keyword::Endgenerate)
            | TokenKind::Keyword(Keyword::End) => None,
            // Externs are parsed together (even though they're not currently supported yet)
            TokenKind::Keyword(Keyword::Extern) => {
                let span = self.peek().span;
                self.diag.report_fatal("extern is not supported", span);
            }
            TokenKind::Keyword(Keyword::Import) => {
                // IMP: This can also be DPI import
                Some(Item::PkgImport(self.parse_pkg_import_decl()))
            }
            TokenKind::Keyword(Keyword::Parameter) | TokenKind::Keyword(Keyword::Localparam) => {
                Some(Item::ParamDecl(Box::new(self.parse_param_decl())))
            }
            // module_declaration
            TokenKind::Keyword(Keyword::Module) => Some(Item::DesignDecl(Box::new(
                self.parse_design_unit(attr, Keyword::Module, Keyword::Endmodule),
            ))),
            // udp_declaration
            TokenKind::Keyword(Keyword::Primitive) => Some(Item::DesignDecl(Box::new(
                self.parse_design_unit(attr, Keyword::Primitive, Keyword::Endprimitive),
            ))),
            // interface_declaration
            TokenKind::Keyword(Keyword::Interface) => Some(Item::DesignDecl(Box::new(
                self.parse_design_unit(attr, Keyword::Interface, Keyword::Endinterface),
            ))),
            // program_declaration
            TokenKind::Keyword(Keyword::Program) => Some(Item::DesignDecl(Box::new(
                self.parse_design_unit(attr, Keyword::Program, Keyword::Endprogram),
            ))),
            // package_declaration
            TokenKind::Keyword(Keyword::Package) => {
                self.consume();
                let lifetime = self.parse_lifetime();
                let name = self.expect_id();
                self.expect(TokenKind::Semicolon);
                let items = self.parse_list(Self::parse_item_opt);
                self.expect(TokenKind::Keyword(Keyword::Endpackage));
                self.parse_end_annotation(Some(&name));
                Some(Item::PkgDecl(Box::new(PkgDecl {
                    attr,
                    lifetime,
                    name,
                    items: items,
                })))
            }
            // function_declaration
            TokenKind::Keyword(Keyword::Function) => Some(self.parse_func_decl(attr)),
            // task_declaration
            TokenKind::Keyword(Keyword::Task) => Some(self.parse_task_decl(attr)),
            // parameter_override
            TokenKind::Keyword(Keyword::Defparam) => {
                // We've decided not to support defparam at all ever even though the standard
                // still requires tools to support it. Defparam is a disaster to implement
                // properly.
                let span = self.peek().span;
                self.diag.report_fatal(
                    "defparam is deprecated, and is not supported by this tool.",
                    span,
                );
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
                self.diag.report_span(
                    Severity::Warning,
                    "there is no need for generate region",
                    kw.span,
                );
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
                self.diag
                    .report_fatal("case_generate_construct is not supported", span);
            }
            // elaboration_system_task
            TokenKind::SystemTask(_) => {
                // First parse as a standard system tf call.
                let tf = self.parse_sys_tf_call();
                // Check that this is an elaboration_system_task.
                match tf.task.as_str() {
                    // Also the syntax requires first argument of $fatal to be either 0,1,2
                    // literal we relax a little bit here and delay it until elaboration.
                    "fatal" | "error" | "warning" | "info" => (),
                    _ => {
                        self.diag.report_error(
                            "only elaboration system task can appear as an item",
                            tf.task.span,
                        );
                    }
                }
                self.expect(TokenKind::Semicolon);
                Some(Item::SysTfCall(Box::new(tf)))
            }
            // modport_declaration
            TokenKind::Keyword(Keyword::Modport) => Some(self.parse_modport_decl(attr)),
            // net_declaration
            TokenKind::Keyword(Keyword::Interconnect) => {
                unimplemented!();
            }
            // also net_declaration
            TokenKind::NetTy(_) => Some(Item::NetDecl(Box::new(self.parse_net_decl(attr)))),
            // typedef
            TokenKind::Keyword(Keyword::Typedef) => Some(self.parse_typedef(attr)),
            // data_declaration. Either begin with const/var or explicit data type.
            TokenKind::Keyword(Keyword::Const)
            | TokenKind::Keyword(Keyword::Var)
            | TokenKind::Keyword(Keyword::Automatic)
            | TokenKind::Keyword(Keyword::Static)
            | TokenKind::IntAtomTy(_)
            | TokenKind::IntVecTy(_)
            | TokenKind::Keyword(Keyword::Reg)
            | TokenKind::RealTy(_)
            | TokenKind::Keyword(Keyword::Struct)
            | TokenKind::Keyword(Keyword::Union)
            | TokenKind::Keyword(Keyword::Enum)
            | TokenKind::Keyword(Keyword::String)
            | TokenKind::Keyword(Keyword::Chandle)
            | TokenKind::Keyword(Keyword::Virtual)
            | TokenKind::Keyword(Keyword::Event)
            | TokenKind::Keyword(Keyword::Type)
            | TokenKind::Keyword(Keyword::Void) => {
                Some(Item::DataDecl(Box::new(self.parse_data_decl(attr))))
            }
            TokenKind::Id(_) => match self.disambiguate_item() {
                ItemDAB::HierInst => Some(self.parse_instantiation(attr)),
                ItemDAB::DataDecl => Some(Item::DataDecl(Box::new(self.parse_data_decl(attr)))),
                _ => {
                    let clone = self.peek().span.clone();
                    self.diag.report_fatal("not implemented", clone);
                }
            },
            _ => {
                let clone = self.peek().span.clone();
                self.diag.report_fatal("not implemented", clone);
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
    fn parse_source(&mut self) -> Vec<Item> {
        let list = self.parse_list(Self::parse_item_opt);
        list
    }

    /// Parse a end identifier annotation. Raises error for mismatch
    fn parse_end_annotation(&mut self, exp: Option<&Ident>) {
        if self.check(TokenKind::Colon) {
            let id = self.expect_id();
            match exp {
                None => self.diag.report_span(
                    Severity::Error,
                    "identifer annotation at end does match declaration, should be empty",
                    id.span,
                ),
                Some(v) => {
                    if **v != *id {
                        self.diag.report_span(
                        Severity::Error,
                        format!("identifer annotation at end does match declaration, should be '{}'", v),
                        id.span
                    )
                    }
                }
            }
        }
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
    fn parse_design_unit(
        &mut self,
        attr: Option<Box<AttrInst>>,
        kw: Keyword,
        end_kw: Keyword,
    ) -> DesignDecl {
        self.consume();
        let lifetime = self.parse_lifetime();
        let name = self.expect_id();
        let pkg_import = self.parse_list(Self::parse_pkg_import_decl_opt);
        let param = self.parse_param_port_list();
        let port = self.parse_port_list();
        self.expect(TokenKind::Semicolon);
        let items = self.parse_list(Self::parse_item_opt);
        self.expect(TokenKind::Keyword(end_kw));
        self.parse_end_annotation(Some(&name));
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
            return None;
        }

        self.parse_delim(Delim::Paren, |this| {
            let mut vec = Vec::new();

            // Default to parameter and un-typed
            let mut param_decl = ParamDecl {
                kw: Keyword::Parameter,
                ty: None,
                list: Vec::new(),
            };

            this.parse_comma_list_unit(true, false, |this| {
                // If a new keyword is seen update it.
                match **this.peek() {
                    TokenKind::Eof => return false,
                    TokenKind::Keyword(e @ Keyword::Parameter)
                    | TokenKind::Keyword(e @ Keyword::Localparam) => {
                        this.consume();
                        let old_decl = mem::replace(
                            &mut param_decl,
                            ParamDecl {
                                kw: e,
                                ty: None,
                                list: Vec::new(),
                            },
                        );
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
                    let old_decl = mem::replace(
                        &mut param_decl,
                        ParamDecl {
                            kw,
                            ty: Some(Box::new(v)),
                            list: Vec::new(),
                        },
                    );
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
            if let Some(v) = this.consume_if(TokenKind::WildPattern) {
                this.diag
                    .report_fatal("(.*) port declaration is not supported", v.span);
            }

            // If there are no ports, it doesn't matter about which style we're using.
            if this.consume_if_eof().is_some() {
                return Vec::new();
            }

            let mut ansi = true;
            let mut prev = None;
            let mut vec = Vec::new();

            this.parse_comma_list_unit(true, false, |this| {
                if this.consume_if_eof().is_some() {
                    return false;
                }

                let dirsp = this.peek().span.clone();
                let dir = this.parse_port_dir();

                // Could only appear in non-ANSI declaration
                if prev.is_none() {
                    match this.peek().value {
                        TokenKind::DelimGroup(Delim::Brace, _) => {
                            ansi = false;
                            return false;
                        }
                        _ => (),
                    }
                }

                // Explicit port declaration
                if let Some(_) = this.consume_if(TokenKind::Dot) {
                    let name = Box::new(this.expect_id());
                    let expr =
                        Box::new(this.parse_unwrap(|this| {
                            this.parse_delim(Delim::Paren, Self::parse_expr_opt)
                        }));

                    // If not specified, default to inout
                    let dir = dir.unwrap_or_else(|| match prev {
                        None | Some(PortDecl::Interface(..)) => PortDir::Inout,
                        Some(PortDecl::Data(dir, ..)) | Some(PortDecl::Explicit(dir, ..)) => dir,
                    });

                    let decl = PortDecl::Explicit(dir, name, expr);
                    if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                        vec.push(v);
                    }
                    return true;
                }

                // First try parse this as an interface port. Note that `interface_name id` is not
                // tellable from `typedef_name id`, in this case we parse it as interface port if
                // there is no direction, as we can easily convert if our guess is incorrect. The
                // otherway around is a bit harder.
                // If both none, then there is a chance that this is an interface port
                let is_intf = if let TokenKind::Keyword(Keyword::Interface) = **this.peek() {
                    // Okay, this is definitely an interface port
                    this.consume();
                    if this.consume_if(TokenKind::Dot).is_some() {
                        let modport = this.expect_id();
                        Some((None, Some(Box::new(modport))))
                    } else {
                        Some((None, None))
                    }
                } else if let TokenKind::Id(_) = **this.peek() {
                    // If we see the dot, then this is definitely is a interface
                    if let TokenKind::Dot = **this.peek_n(1) {
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
                        this.diag.report_span(
                            Severity::Error,
                            "interface declaration should not be specified together with direction",
                            dirsp,
                        );
                    }
                    let decl = PortDecl::Interface(a, b, vec![this.parse_decl_assign()]);
                    if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                        vec.push(v);
                    }
                    return true;
                }

                // Parse net-type
                let net = match **this.peek() {
                    TokenKind::Keyword(Keyword::Var) => {
                        this.consume();
                        Some(NetPortType::Variable)
                    }
                    TokenKind::NetTy(netty) => {
                        this.consume();
                        Some(NetPortType::Builtin(netty))
                    }
                    // TODO we might also have identifier net-type or interconnect type
                    _ => None,
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
                        PortDecl::Data(_, _, _, ref mut l)
                        | PortDecl::Interface(_, _, ref mut l) => {
                            l.push(assign);
                            return true;
                        }
                        // Well, if previously it is an explicit port we fall through
                        _ => (),
                    }
                }

                // If not specified, default to inout
                let dir = dir.unwrap_or_else(|| match prev {
                    None | Some(PortDecl::Interface(..)) => PortDir::Inout,
                    Some(PortDecl::Data(dir, ..)) | Some(PortDecl::Explicit(dir, ..)) => dir,
                });

                // If not specified, default to default nettype or variable
                let net = net.unwrap_or_else(|| match dir {
                    PortDir::Input | PortDir::Inout => NetPortType::Default,
                    PortDir::Output => match dtype.as_ref() {
                        None => NetPortType::Default,
                        Some(v) => match **v {
                            DataTypeKind::Implicit(..) => NetPortType::Default,
                            _ => NetPortType::Variable,
                        },
                    },
                    PortDir::Ref => NetPortType::Variable,
                });

                // Default to implicit wire
                let dtype = Box::new(dtype.unwrap_or_else(|| {
                    Spanned::new(DataTypeKind::Implicit(Signing::Unsigned, Vec::new()), dirsp)
                }));

                let decl = PortDecl::Data(dir, net, dtype, vec![assign]);
                if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                    vec.push(v);
                }

                return true;
            });

            if !ansi {
                let span = this.peek().span.clone();
                this.diag
                    .report_fatal("non-ANSI port declaration is not yet supported", span);
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

        let (ty, list) = self.parse_data_type_decl_assign_list();
        self.expect(TokenKind::Semicolon);
        ParamDecl {
            kw,
            ty: ty.map(Box::new),
            list,
        }
    }

    //
    // A.2.1.3 Type declarations
    //

    fn parse_data_decl(&mut self, attr: Option<Box<AttrInst>>) -> DataDecl {
        let has_const = self.check(TokenKind::Keyword(Keyword::Const));
        let _has_var = self.check(TokenKind::Keyword(Keyword::Var));
        let lifetime = self.parse_lifetime();
        let (ty, list) = self.parse_data_type_decl_assign_list();
        self.expect(TokenKind::Semicolon);
        DataDecl {
            attr,
            has_const,
            lifetime,
            ty: ty.unwrap_or_else(|| {
                Spanned::new_unspanned(DataTypeKind::Implicit(Signing::Unsigned, Vec::new()))
            }),
            list,
        }
    }

    /// According to the spec
    /// `bnf
    /// net_type [ drive_strength | charge_strength ] [ vectored | scalared ] data_type_or_implicit
    ///   [ delay3 ] list_of_net_decl_assignments ;
    /// | net_type_identifier [ delay_control ] list_of_net_decl_assignments ;
    /// | interconnect implicit_data_type [ # delay_value ] net_identifier { unpacked_dimension }
    ///   [ , net_identifier { unpacked_dimension } ] ;
    /// ```
    /// Note that during parsing we cannot tell the second variant apart from a data declaration if
    /// delay control is not present.
    fn parse_net_decl(&mut self, attr: Option<Box<AttrInst>>) -> NetDecl {
        // Currently we only handle first type of declaration
        let net = match self.consume().value {
            TokenKind::NetTy(netty) => netty,
            _ => unreachable!(),
        };
        // TODO: drive_strength | charge_strength
        // TODO: vectored | scalared
        let (ty, list) = self.parse_data_type_decl_assign_list();
        self.expect(TokenKind::Semicolon);
        NetDecl {
            attr,
            net,
            ty: ty.unwrap_or_else(|| {
                Spanned::new_unspanned(DataTypeKind::Implicit(Signing::Unsigned, Vec::new()))
            }),
            list,
        }
    }

    fn parse_typedef(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        self.consume();
        // First try to parse it as a forward typedef.
        match **self.peek() {
            TokenKind::Keyword(Keyword::Interface) => {
                let mut peek = 1;
                if let TokenKind::Keyword(Keyword::Class) = **self.peek_n(1) {
                    peek += 1;
                }
                if let TokenKind::Id(_) = **self.peek_n(peek) {
                    if let TokenKind::Semicolon = **self.peek_n(peek + 1) {
                        // This is a forward typedef
                        self.unimplemented();
                    }
                }
            }
            TokenKind::Keyword(Keyword::Enum)
            | TokenKind::Keyword(Keyword::Struct)
            | TokenKind::Keyword(Keyword::Union)
            | TokenKind::Keyword(Keyword::Class) => {
                if let TokenKind::Id(_) = **self.peek_n(1) {
                    if let TokenKind::Semicolon = **self.peek_n(2) {
                        // This is a forward typedef
                        self.unimplemented();
                    }
                }
            }
            _ => (),
        }
        // We parse the data type as expression and we need to distinguish between interface type
        // import vs normal typedef.
        let expr = self.parse_expr();
        // This is a type import
        match expr.value {
            ExprKind::HierName(HierId::Member(intf, ty)) => {
                let intf = *intf;
                let id = self.expect_id();
                self.expect(TokenKind::Semicolon);
                Item::TypedefIntf(attr, Box::new(intf), ty, Box::new(id))
            }
            _ => {
                let span = expr.span;
                let ty = match self.conv_expr_to_type(expr) {
                    None => {
                        self.diag.report_fatal("expected data type", span);
                        // TODO: Error recovery
                    }
                    Some(v) => v,
                };
                let id = self.expect_id();
                let dim = self.parse_list(Self::parse_dim_opt);
                self.expect(TokenKind::Semicolon);
                Item::Typedef(attr, Box::new(ty), Box::new(id), dim)
            }
        }
    }

    /// Parse a package import declaration
    fn parse_pkg_import_decl_opt(&mut self) -> Option<Vec<PkgImportItem>> {
        if self
            .consume_if(TokenKind::Keyword(Keyword::Import))
            .is_none()
        {
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
        self.expect(TokenKind::ScopeSep);
        let id = if self.check(TokenKind::BinaryOp(BinaryOp::Mul)) {
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

    /// Parse a data type (or implicit)
    fn parse_data_type(&mut self) -> DataType {
        let expr = self.parse_expr();
        let span = expr.span;
        match self.conv_expr_to_type(expr) {
            Some(v) => v,
            None => {
                self.diag
                    .report_span(Severity::Error, "expected data type", span);
                // Error recovery
                Spanned::new(DataTypeKind::Implicit(Signing::Unsigned, Vec::new()), span)
            }
        }
    }

    /// Parse a data type (or implicit) followed a decl_assign.
    fn parse_data_type_decl_assign(&mut self) -> (Option<DataType>, DeclAssign) {
        let expr = self.parse_expr();
        let span = expr.span;
        let dtype = match self.conv_expr_to_type(expr) {
            None => {
                self.diag
                    .report_fatal("expected data type or identifier", span);
                // TODO: Do error recovery here.
            }
            Some(v) => v,
        };

        match self.consume_if_id() {
            Some(name) => {
                let dim = self.parse_list(Self::parse_dim_opt);
                let init = match self.consume_if(TokenKind::Assign) {
                    None => None,
                    Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
                };
                (Some(dtype), DeclAssign { name, dim, init })
            }
            None => {
                match self.conv_type_to_id(dtype) {
                    Some((name, dim)) => {
                        let init = match self.consume_if(TokenKind::Assign) {
                            None => None,
                            Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
                        };
                        (None, DeclAssign { name, dim, init })
                    }
                    None => {
                        self.diag
                            .report_fatal("data type should be followed by an identifier", span);
                        // TODO: Error recovery
                    }
                }
            }
        }
    }

    /// Parse a data type (or implicit) followed a decl_assign_list.
    fn parse_data_type_decl_assign_list(&mut self) -> (Option<DataType>, Vec<DeclAssign>) {
        let (ty, assign) = self.parse_data_type_decl_assign();
        let mut list = vec![assign];
        if self.check(TokenKind::Comma) {
            self.parse_comma_list_unit(false, false, |this| match this.parse_decl_assign_opt() {
                None => false,
                Some(v) => {
                    list.push(v);
                    true
                }
            });
        }
        (ty, list)
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
    /// | void
    /// | [ signing ] { packed_dimension }
    /// ```
    ///
    /// The following BNFs exist in spec but they're parsed as expression:
    /// ```bnf
    /// | [ class_scope | package_scope ] type_identifier { packed_dimension }
    /// | class_type
    /// ```
    fn parse_kw_data_type(&mut self) -> DataType {
        match **self.peek() {
            TokenKind::IntVecTy(_) | TokenKind::Keyword(Keyword::Reg) => {
                // This makes Keyword::Reg turn into IntVecTy::Logic
                let kw = self.consume();
                let mut span = kw.span;
                let ty = if let TokenKind::IntVecTy(IntVecTy::Bit) = *kw {
                    IntVecTy::Bit
                } else {
                    IntVecTy::Logic
                };

                // Parse signing
                let sign = if let TokenKind::Signing(sign) = **self.peek() {
                    span = span.merge(self.consume().span);
                    sign
                } else {
                    Signing::Unsigned
                };

                // Parse dimension
                let mut dim = self.parse_list(Self::parse_dim_opt);
                if let Some(v) = dim.last() {
                    span = span.merge(v.span);
                }
                self.check_list(&mut dim, Self::check_packed_dim);

                Spanned::new(DataTypeKind::IntVec(ty, sign, dim), span)
            }
            TokenKind::IntAtomTy(ty) => {
                let mut span = self.consume().span;
                let sign = if let TokenKind::Signing(sign) = **self.peek() {
                    span = span.merge(self.consume().span);
                    Some(sign)
                } else {
                    None
                };
                Spanned::new(DataTypeKind::IntAtom(ty, sign), span)
            }
            TokenKind::RealTy(ty) => {
                let span = self.consume().span;
                Spanned::new(DataTypeKind::Real(ty), span)
            }
            TokenKind::Keyword(Keyword::Struct) | TokenKind::Keyword(Keyword::Union) => {
                self.parse_aggr_decl()
            }
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum_decl(),
            TokenKind::Keyword(Keyword::String) => {
                let span = self.consume().span;
                Spanned::new(DataTypeKind::String, span)
            }
            TokenKind::Keyword(Keyword::Chandle) => {
                let span = self.consume().span;
                Spanned::new(DataTypeKind::Chandle, span)
            }
            TokenKind::Keyword(Keyword::Event) => {
                let span = self.consume().span;
                Spanned::new(DataTypeKind::Event, span)
            }
            TokenKind::Keyword(Keyword::Virtual) => {
                self.unimplemented();
            }
            // type_reference
            TokenKind::Keyword(Keyword::Type) => {
                let token = self.consume();
                match self.parse_if_delim_spanned(Delim::Paren, Self::parse_expr) {
                    None => Spanned::new(DataTypeKind::Type, token.span),
                    Some(v) => Spanned::new(
                        DataTypeKind::TypeRef(Box::new(v.value)),
                        token.span.merge(v.span),
                    ),
                }
            }
            TokenKind::Keyword(Keyword::Void) => {
                let token = self.consume();
                Spanned::new(DataTypeKind::Void, token.span)
            }
            TokenKind::Signing(sign) => {
                let mut span = self.consume().span;
                let mut dim = self.parse_list(Self::parse_dim_opt);
                if let Some(v) = dim.last() {
                    span = span.merge(v.span);
                }
                self.check_list(&mut dim, Self::check_packed_dim);
                Spanned::new(DataTypeKind::Implicit(sign, dim), span)
            }
            TokenKind::DelimGroup(Delim::Bracket, _) => {
                let mut dim = self.parse_list(Self::parse_dim_opt);
                let span = dim.first().unwrap().span.merge(dim.last().unwrap().span);
                self.check_list(&mut dim, Self::check_packed_dim);
                Spanned::new(DataTypeKind::Implicit(Signing::Unsigned, dim), span)
            }
            _ => unreachable!(),
        }
    }

    /// Parse a struct or union declaration.
    fn parse_aggr_decl(&mut self) -> DataType {
        let (token, kind) = match **self.peek() {
            TokenKind::Keyword(Keyword::Struct) => (self.consume(), AggrType::Struct),
            TokenKind::Keyword(Keyword::Union) => (
                self.consume(),
                if self.check(TokenKind::Keyword(Keyword::Tagged)) {
                    AggrType::TaggedUnion
                } else {
                    AggrType::Union
                },
            ),
            _ => unimplemented!(),
        };
        let packed = self.check(TokenKind::Keyword(Keyword::Packed));
        let sign = self.parse_signing();
        let members = self.parse_delim_spanned(Delim::Brace, |this| {
            this.parse_list(|this| {
                // Return if this is the end
                match **this.peek() {
                    TokenKind::Eof => return None,
                    _ => (),
                }
                let attr = this.parse_attr_inst_opt();
                match **this.peek() {
                    TokenKind::Keyword(Keyword::Rand) | TokenKind::Keyword(Keyword::Randc) => {
                        this.unimplemented();
                    }
                    _ => (),
                }
                let span = this.peek().span;
                let (ty, list) = this.parse_data_type_decl_assign_list();
                this.expect(TokenKind::Semicolon);
                let ty = match ty {
                    None => {
                        this.diag
                            .report_fatal("data type of aggregate member cannot be implicit", span);
                        // TODO: Error recovery
                    }
                    Some(v) => v,
                };
                Some(AggrMember { attr, ty, list })
            })
        });
        let mut span = token.span.merge(members.span);
        let mut dim = self.parse_list(Self::parse_dim_opt);
        if let Some(v) = dim.last() {
            span = span.merge(v.span);
        }
        self.check_list(&mut dim, Self::check_packed_dim);
        Spanned::new(
            DataTypeKind::Aggr(
                AggrDecl {
                    kind,
                    packed,
                    sign,
                    members: members.value,
                },
                dim,
            ),
            span,
        )
    }

    /// Parse a enum declaration.
    fn parse_enum_decl(&mut self) -> DataType {
        let token = self.consume();
        let base = if let TokenKind::DelimGroup(Delim::Brace, _) = **self.peek() {
            None
        } else {
            Some(Box::new(self.parse_data_type()))
        };
        let members = self.parse_delim_spanned(Delim::Brace, |this| {
            this.parse_comma_list(false, false, Self::parse_decl_assign_opt)
        });
        let mut span = token.span.merge(members.span);

        // The declaration assignment list in enum is special: a dimension will dictate the
        // compiler to create multiple items instead of creating an array. So we have to check
        // if the dimensions are valid.
        for assign in &members.value {
            // The most common case where there are no dimensions. It is valid.
            if assign.dim.is_empty() {
                continue;
            }

            // Multiple dimensions are illegal
            if assign.dim.len() > 1 {
                self.diag.report_span(
                    Severity::Error,
                    "multiple dimensions are not allowed in enumeration",
                    assign.dim[1].span,
                );
            }

            // Check that the dimension values are integral literals.
            match assign.dim[0].value {
                DimKind::AssocWild
                | DimKind::PlusRange(..)
                | DimKind::MinusRange(..)
                | DimKind::Unsized => {
                    self.diag.report_span(
                        Severity::Error,
                        "this type of dimension is not allowed in enumeration",
                        assign.dim[0].span,
                    );
                }
                DimKind::Value(ref v) => {
                    if let ExprKind::Literal(Spanned {
                        value: TokenKind::IntegerLiteral(ref val),
                        ..
                    }) = v.value
                    {
                        if !val.value.is_two_state() {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be two state",
                                v.span,
                            );
                        }
                        if val.value.cmp_with_zero() != std::cmp::Ordering::Greater {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be positive",
                                v.span,
                            );
                        }
                    } else {
                        self.diag.report_span(
                            Severity::Error,
                            "only integral literals are allowed in generated names",
                            v.span,
                        );
                    }
                }
                DimKind::Range(ref ub, ref lb) => {
                    if let ExprKind::Literal(Spanned {
                        value: TokenKind::IntegerLiteral(ref val),
                        ..
                    }) = ub.value
                    {
                        if !val.value.is_two_state() {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be two state",
                                ub.span,
                            );
                        }
                        if val.value.cmp_with_zero() == std::cmp::Ordering::Less {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be non-negative",
                                ub.span,
                            );
                        }
                    } else {
                        self.diag.report_span(
                            Severity::Error,
                            "only integral literals are allowed in generated names",
                            ub.span,
                        );
                    }
                    if let ExprKind::Literal(Spanned {
                        value: TokenKind::IntegerLiteral(ref val),
                        ..
                    }) = lb.value
                    {
                        if !val.value.is_two_state() {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be two state",
                                lb.span,
                            );
                        }
                        if val.value.cmp_with_zero() == std::cmp::Ordering::Less {
                            self.diag.report_span(
                                Severity::Error,
                                "integral literals in generated names must be non-negative",
                                ub.span,
                            );
                        }
                    } else {
                        self.diag.report_span(
                            Severity::Error,
                            "only integral literals are allowed in enumeration",
                            lb.span,
                        );
                    }
                }
            }
        }

        let mut dim = self.parse_list(Self::parse_dim_opt);
        if let Some(v) = dim.last() {
            span = span.merge(v.span);
        }
        self.check_list(&mut dim, Self::check_packed_dim);
        Spanned::new(
            DataTypeKind::Enum(
                EnumDecl {
                    ty: base,
                    members: members.value,
                },
                dim,
            ),
            span,
        )
    }

    /// Convert an expression to a type. Useful when we try to parse thing as expression first due
    /// to ambiguity, then realised that it is actually a type.
    fn conv_expr_to_type(&mut self, expr: Expr) -> Option<DataType> {
        match expr.value {
            ExprKind::Type(ty) => Some(*ty),
            ExprKind::HierName(mut id) => {
                // Convert select expression into dimension list.
                let mut dimlist = Vec::new();
                if let HierId::Select(name, dim) = id {
                    dimlist.push(*dim);
                    id = name.value;
                }
                dimlist.reverse();

                // In data type, hierachical identifier is not allowed. It can only be
                let (scope, name) = match id {
                    HierId::Name(scope, name) => (scope, *name),
                    _ => {
                        self.diag.report_error(
                            "hierachical identifier cannot appear in data type",
                            expr.span,
                        );
                        (None, Ident::new_unspanned("".to_owned()))
                    }
                };
                Some(Spanned::new(
                    DataTypeKind::HierName(scope, name, dimlist),
                    expr.span,
                ))
            }
            _ => None,
        }
    }

    /// We made a observation that every identifier (with unpacked dimension) looks like an data
    /// type. This function tries to convert a data_type to an identifier (and a dimension list).
    fn conv_type_to_id(&mut self, ty: DataType) -> Option<(Ident, Vec<Dim>)> {
        match ty.value {
            // TODO: what about dimension
            DataTypeKind::HierName(None, id, dim) => Some((id, dim)),
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
            self.diag.report_span(
                Severity::Error,
                "this looks like a data type but it is not declared",
                ident.span,
            );
            ident = id;
            dim = self.parse_list(Self::parse_dim_opt);
        }

        let init = match self.consume_if(TokenKind::Assign) {
            None => None,
            Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt))),
        };
        Some(DeclAssign {
            name: ident,
            dim,
            init,
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
                TokenKind::Eof => return DimKind::Unsized,
                TokenKind::BinaryOp(BinaryOp::Mul) => {
                    if let TokenKind::Eof = **this.peek_n(1) {
                        this.consume();
                        return DimKind::AssocWild;
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
                TokenKind::PlusColon => {
                    this.consume();
                    DimKind::PlusRange(Box::new(expr), Box::new(this.parse_expr()))
                }
                TokenKind::MinusColon => {
                    this.consume();
                    DimKind::MinusRange(Box::new(expr), Box::new(this.parse_expr()))
                }
                _ => DimKind::Value(Box::new(expr)),
            }
        })
    }

    /// Check if a dimension is a legal unpacked dimension
    fn check_unpacked_dim(&mut self, dim: &Dim) -> bool {
        match **dim {
            DimKind::AssocWild
            | DimKind::PlusRange(..)
            | DimKind::MinusRange(..)
            | DimKind::Unsized => {
                self.diag.report_span(
                    Severity::Error,
                    "this type of range is not allowed in unpacked dimension context",
                    dim.span,
                );
                false
            }
            _ => true,
        }
    }

    /// Check if a dimension is a legal packed dimension
    fn check_packed_dim(&mut self, dim: &Dim) -> bool {
        match **dim {
            DimKind::AssocWild
            | DimKind::PlusRange(..)
            | DimKind::MinusRange(..)
            | DimKind::Value(_) => {
                self.diag.report_span(
                    Severity::Error,
                    "this type of range is not allowed in packed dimension context",
                    dim.span,
                );
                false
            }
            _ => true,
        }
    }

    //
    // A.2.6 Function declarations
    //

    fn parse_func_decl(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        self.consume();
        let lifetime = self.parse_lifetime();
        let ty = self.parse_data_type();
        // TODO: [ interface_identifier . | class_scope ]
        let name = self.expect_id();
        // TODO: tf_port is different from module port
        let ports = self.parse_port_list();
        self.expect(TokenKind::Semicolon);
        let stmts = self.parse_list(Self::parse_stmt_opt);
        self.expect(TokenKind::Keyword(Keyword::Endfunction));
        self.parse_end_annotation(Some(&name));
        Item::FuncDecl(Box::new(FuncDecl {
            attr,
            lifetime,
            ty,
            name,
            ports: ports.unwrap_or_else(|| Vec::new()),
            stmts,
        }))
    }

    //
    // A.2.7 Task declarations
    //

    fn parse_task_decl(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        self.consume();
        let lifetime = self.parse_lifetime();
        // TODO: [ interface_identifier . | class_scope ]
        let name = self.expect_id();
        // TODO: tf_port is different from module port
        let ports = self.parse_port_list();
        self.expect(TokenKind::Semicolon);
        let stmts = self.parse_list(Self::parse_stmt_opt);
        self.expect(TokenKind::Keyword(Keyword::Endtask));
        self.parse_end_annotation(Some(&name));
        Item::TaskDecl(Box::new(TaskDecl {
            attr,
            lifetime,
            name,
            ports: ports.unwrap_or_else(|| Vec::new()),
            stmts,
        }))
    }

    //
    // A.2.9 Interface declarations
    //

    fn parse_modport_decl(&mut self, attr: Option<Box<AttrInst>>) -> Item {
        self.consume();
        let vec = self.parse_comma_list(false, false, |this| {
            let id = this.consume_if_id()?;
            let list = this.parse_delim(Delim::Paren, |this| {
                this.parse_comma_list(true, false, |this| {
                    // Parse optional prefixing attribute first.
                    let attr = this.parse_attr_inst_opt();
                    match **this.peek() {
                        TokenKind::Keyword(Keyword::Clocking) => {
                            // modport_clocking_declaration
                            this.consume();
                            let id = this.expect_id();
                            Some(ModportPortDecl::Clocking(attr, Box::new(id)))
                        }
                        TokenKind::Keyword(Keyword::Import)
                        | TokenKind::Keyword(Keyword::Export) => {
                            // modport_tf_ports_declaration
                            this.unimplemented()
                        }
                        TokenKind::PortDir(dir) => {
                            this.consume();
                            let mut vec = Vec::new();
                            loop {
                                if this.check(TokenKind::Dot) {
                                    let id = this.expect_id();
                                    let expr = this.parse_delim(Delim::Paren, Self::parse_expr);
                                    vec.push(ModportSimplePort::Explicit(id, Box::new(expr)))
                                } else {
                                    let id = this.expect_id();
                                    vec.push(ModportSimplePort::Named(id))
                                }
                                if let TokenKind::Comma = **this.peek() {
                                    match **this.peek_n(1) {
                                        // Consume comma and continue
                                        TokenKind::Dot | TokenKind::Id(_) => {
                                            this.consume();
                                            continue;
                                        }
                                        _ => (),
                                    }
                                }
                                break;
                            }
                            Some(ModportPortDecl::Simple(attr, dir, vec))
                        }
                        _ => None,
                    }
                })
            });
            Some((id, list))
        });
        self.expect(TokenKind::Semicolon);
        Item::ModportDecl(attr, vec)
    }

    //
    // A.4.1.1 Module instantiation
    //

    /// Try to tell which item does an identifier begin by looking ahead.
    fn disambiguate_item(&mut self) -> ItemDAB {
        let mut peek = 1;
        let mut has_hash = false;
        // This is an interface port
        if let TokenKind::Dot = **self.peek_n(1) {
            return ItemDAB::IntfPort;
        }
        // Skip over parameter assignment if any
        if let TokenKind::Hash = **self.peek_n(1) {
            has_hash = true;
            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(2) {
                peek = 3;
            } else {
                // Hash but no parenthesis, then the hash should be delay control.
                // This is therefore a net_declaration
                return ItemDAB::NetDecl;
            }
        }
        // For net declaration and instantiation, the next one must be an identifier
        if let TokenKind::Id(_) = **self.peek_n(peek) {
            peek += 1
        } else {
            return ItemDAB::DataDecl;
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
            if has_hash {
                ItemDAB::NetDecl
            } else {
                ItemDAB::DataDecl
            }
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
            Some(self.parse_unwrap(|this| this.parse_args_opt(true)))
        } else {
            None
        };

        let list = self.parse_comma_list(false, false, |this| {
            let name = match this.consume_if_id() {
                None => return None,
                Some(v) => v,
            };

            let mut dim = this.parse_list(Self::parse_dim_opt);
            this.check_list(&mut dim, Self::check_unpacked_dim);
            let ports = this.parse_port_conn();
            Some(HierInst { name, dim, ports })
        });

        self.expect(TokenKind::Semicolon);

        Item::HierInstantiation(Box::new(HierInstantiation {
            attr,
            param,
            name: mod_name,
            inst: list,
        }))
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
    fn parse_port_conn(&mut self) -> PortConn {
        self.parse_delim(Delim::Paren, |this| {
            if let TokenKind::Eof = **this.peek() {
                return PortConn::Ordered(Vec::new());
            }

            // One list for each type of argument.
            let mut ordered = Vec::new();
            let mut named = Vec::new();
            let mut has_wildcard = false;

            this.parse_comma_list_unit(true, false, |this| {
                let attr = this.parse_attr_inst_opt();
                if let Some(v) = this.consume_if(TokenKind::WildPattern) {
                    if has_wildcard {
                        this.diag.report_span(
                            Severity::Error,
                            ".* can only appear once in an argument list",
                            v.span,
                        );
                    }
                    named.push((attr, NamedPortConn::Wildcard));
                    has_wildcard = true;
                    true
                } else if let Some(v) = this.consume_if(TokenKind::Dot) {
                    let name = this.expect_id();
                    let expr = this.parse_if_delim_spanned(Delim::Paren, Self::parse_expr_opt);
                    if !ordered.is_empty() {
                        this.diag.report_span(
                            Severity::Error,
                            "mixture of ordered and named argument is not allowed",
                            v.span.merge(match expr {
                                None => name.span,
                                Some(ref expr) => expr.span,
                            }),
                        );
                    }
                    named.push((
                        attr,
                        match expr {
                            None => NamedPortConn::Implicit(name),
                            Some(expr) => NamedPortConn::Explicit(name, expr.value.map(Box::new)),
                        },
                    ));
                    true
                } else {
                    let expr = this.parse_expr_opt().map(Box::new);
                    if !named.is_empty() {
                        if let Some(expr) = &expr {
                            this.diag.report_span(
                                Severity::Error,
                                "ordered argument cannot appear after named argument",
                                expr.span,
                            );
                        } else {
                            // Return None so error message will be about trailing comma.
                            return false;
                        }
                    }
                    ordered.push((attr, expr));
                    true
                }
            });

            if !named.is_empty() {
                PortConn::Named(named)
            } else {
                PortConn::Ordered(ordered)
            }
        })
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
    fn parse_args_opt(&mut self, is_param: bool) -> Option<Args> {
        self.parse_if_delim(Delim::Paren, |this| {
            // One list for each type of argument.
            let mut ordered = Vec::new();
            let mut named = Vec::new();

            this.parse_comma_list_unit(true, false, |this| {
                if let Some(v) = this.consume_if(TokenKind::Dot) {
                    let name = this.expect_id();
                    let expr = this.parse_delim_spanned(Delim::Paren, Self::parse_expr_opt);
                    if !ordered.is_empty() && is_param {
                        this.diag.report_span(
                            Severity::Error,
                            "mixture of ordered and named argument is not allowed",
                            v.span.merge(expr.span),
                        );
                    }
                    named.push((Box::new(name), expr.value.map(Box::new)));
                    true
                } else {
                    let expr = this.parse_expr_opt().map(Box::new);
                    if !named.is_empty() {
                        if let Some(expr) = &expr {
                            this.diag.report_span(
                                Severity::Error,
                                "ordered argument cannot appear after named argument",
                                expr.span,
                            );
                        } else {
                            // Return None so error message will be about trailing comma.
                            return false;
                        }
                    }
                    ordered.push(expr);
                    true
                }
            });

            Args { ordered, named }
        })
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
        let (genvar, id, init, cond, update) = self.parse_delim(Delim::Paren, |this| {
            let genvar = this.check(TokenKind::Keyword(Keyword::Genvar));
            let id = this.expect_id();
            this.expect(TokenKind::Assign);
            let init = this.parse_expr();
            this.expect(TokenKind::Semicolon);
            let cond = this.parse_expr();
            this.expect(TokenKind::Semicolon);
            let update = this.parse_assign_expr();
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

        let mut if_block = Vec::new();

        loop {
            let cond = self.parse_delim(Delim::Paren, Self::parse_expr);
            let true_block = self.parse_gen_block();
            if_block.push((cond, true_block));

            let else_block = if self.check(TokenKind::Keyword(Keyword::Else)) {
                // This is a else-if. We need to treat it differently
                if self.check(TokenKind::Keyword(Keyword::If)) {
                    continue;
                }
                // Treat different if next one is also a if
                Some(Box::new(self.parse_gen_block()))
            } else {
                None
            };

            // There are no more else-if's, return now.
            return Item::IfGen(Box::new(IfGen {
                attr,
                if_block,
                else_block,
            }));
        }
    }

    fn parse_gen_block(&mut self) -> GenBlock {
        // A generate-block may begin with a label. It is treated as same as label after begin.
        let label = if let TokenKind::Id(_) = **self.peek() {
            if let TokenKind::Colon = **self.peek_n(1) {
                // This is actuall
                if let TokenKind::Keyword(Keyword::Begin) = **self.peek_n(2) {
                    let label = self.expect_id();
                    self.consume();
                    Some(label)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };

        let begin = match self.consume_if(TokenKind::Keyword(Keyword::Begin)) {
            None => {
                return GenBlock {
                    name: None,
                    items: vec![self.parse_item()],
                }
            }
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
                self.diag.report_span(
                    Severity::Error,
                    "block identifiers before and after 'begin' are not identical",
                    n.span,
                );
            } else {
                self.diag.report_span(
                    Severity::Warning,
                    "duplicate block identifiers before and after 'begin'",
                    n.span,
                );
            }
        } else if let (Some(l), None) = (&label, &name) {
            self.diag.report(
                Diagnostic::new(
                    Severity::Warning,
                    "it is suggested to place block identifier after 'begin'",
                    l.span.merge(begin.span),
                )
                .fix_primary(format!("begin: {}", l)),
            );
        }

        let name = name.or(label);
        let items = self.parse_list(Self::parse_item_opt);

        self.expect(TokenKind::Keyword(Keyword::End));
        self.parse_end_annotation(name.as_ref());

        GenBlock {
            name: name.map(Box::new),
            items,
        }
    }

    //
    // A.6.1 Continuous assignment and net alias statements
    //
    fn parse_continuous_assign(&mut self) -> Item {
        self.consume();
        // IMP: Parse drive_strength
        // IMP: Parse delay control
        let assignments = self.parse_comma_list(false, false, Self::parse_assign_expr_opt);
        self.expect(TokenKind::Semicolon);
        Item::ContinuousAssign(assignments)
    }

    //
    // A.6.2 Procedural blocks and assignments
    //

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
                    self.diag.report_span(
                        Severity::Error,
                        "block identifiers before and after 'begin' are not identical",
                        id.span,
                    );
                } else {
                    self.diag.report_span(
                        Severity::Warning,
                        "duplicate block identifiers before and after 'begin'",
                        id.span,
                    );
                }
            }
            *label = Some(id);
        } else if let Some(v) = label {
            self.diag.report(
                Diagnostic::new(
                    Severity::Warning,
                    "it is suggested to place block identifier after 'begin'",
                    v.span.merge(begin.span),
                )
                .fix_primary(format!("begin: {}", v)),
            );
        }

        let items = self.parse_list(Self::parse_stmt_opt);

        self.expect(TokenKind::Keyword(Keyword::End));
        self.parse_end_annotation(label.as_ref());
        StmtKind::SeqBlock(items)
    }

    //
    // A.6.4 Statements
    //

    /// Parse a block item declaration, statement, or null statement.
    /// According to the spec
    /// ```bnf
    /// block_item_declaration ::=
    ///   { attribute_instance } data_declaration
    /// | { attribute_instance } local_parameter_declaration ;
    /// | { attribute_instance } parameter_declaration ;
    /// | { attribute_instance } overload_declaration
    /// | { attribute_instance } let_declaration
    /// statement_or_null ::=
    ///  statement | { attribute_instance } ;
    /// statement ::=
    ///  [ block_identifier : ] { attribute_instance } statement_item
    /// statement_item ::=
    ///   blocking_assignment ;
    /// | nonblocking_assignment ;
    /// | case_statement
    /// | conditional_statement
    /// | inc_or_dec_expression ;
    /// | subroutine_call_statement
    /// | disable_statement
    /// | event_trigger
    /// | loop_statement
    /// | jump_statement
    /// | par_block
    /// | procedural_timing_control_statement
    /// | seq_block
    /// | wait_statement
    /// | procedural_assertion_statement
    /// | clocking_drive ;
    /// | randsequence_statement
    /// | randcase_statement
    /// | expect_property_statement
    /// ```
    fn parse_stmt_opt(&mut self) -> Option<Stmt> {
        // These are common to all statements:
        // an optional identifier and an attribute.
        let mut label = if let TokenKind::Id(_) = **self.peek() {
            if let TokenKind::Colon = **self.peek_n(1) {
                let id = self.expect_id();
                self.consume();
                Some(id)
            } else {
                None
            }
        } else {
            None
        };
        let attr = self.parse_attr_inst_opt();

        let kind = match **self.peek() {
            TokenKind::Keyword(Keyword::End)
            | TokenKind::Keyword(Keyword::Endfunction)
            | TokenKind::Keyword(Keyword::Endtask)
            | TokenKind::Keyword(Keyword::Else) => return None,
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
                    TokenKind::CaseKw(_) => self.parse_case_stmt(Some(uniq)),
                    _ => {
                        self.diag.report_span(
                            Severity::Error,
                            "expected if or case statement after unique, unique0 or priority",
                            prio.span,
                        );
                        // Error recovery
                        StmtKind::Empty
                    }
                }
            }
            // conditional_statement
            TokenKind::Keyword(Keyword::If) => self.parse_if_stmt(None),
            TokenKind::CaseKw(_) => self.parse_case_stmt(None),
            // disable_statement
            TokenKind::Keyword(Keyword::Disable) => self.unimplemented(),
            // event_trigger
            TokenKind::NonblockTrigger | TokenKind::BinaryOp(BinaryOp::Imply) => {
                self.unimplemented()
            }
            // loop_statement
            TokenKind::Keyword(Keyword::Forever)
            | TokenKind::Keyword(Keyword::Repeat)
            | TokenKind::Keyword(Keyword::While) => self.unimplemented(),
            TokenKind::Keyword(Keyword::For) => self.parse_for(),
            TokenKind::Keyword(Keyword::Do) | TokenKind::Keyword(Keyword::Foreach) => {
                self.unimplemented()
            }
            // jump_statement
            TokenKind::Keyword(Keyword::Return)
            | TokenKind::Keyword(Keyword::Break)
            | TokenKind::Keyword(Keyword::Continue) => self.unimplemented(),
            // par_block
            TokenKind::Keyword(Keyword::Fork) => self.unimplemented(),
            // procedural_timing_control_statement
            TokenKind::Hash | TokenKind::CycleDelay | TokenKind::AtStar | TokenKind::At => {
                self.parse_timing_ctrl_stmt()
            }
            // seq_block
            TokenKind::Keyword(Keyword::Begin) => self.parse_seq_block(&mut label),
            // wait_statement
            TokenKind::Keyword(Keyword::Wait) | TokenKind::Keyword(Keyword::WaitOrder) => {
                self.unimplemented()
            }
            // simple_immediate_assertion_statement, deferred_immediate_assertion_statement
            // or assert_property_statement
            TokenKind::Keyword(Keyword::Assert) => {
                let kw = self.consume();
                match **self.peek() {
                    // assert_property_statement
                    TokenKind::Keyword(Keyword::Property) |
                    // deferred_immediate_assertion_statement
                    TokenKind::Hash => self.unimplemented(),
                    // simple_immediate_assertion_statement
                    _ => {
                        let expr = Box::new(self.parse_delim(Delim::Paren, Self::parse_expr));
                        let success = self.parse_stmt_opt().map(Box::new);
                        let failure = if self.check(TokenKind::Keyword(Keyword::Else)) {
                            Some(Box::new(self.parse_stmt()))
                        } else {
                            None
                        };
                        if success.is_none() && failure.is_none() {
                            self.diag.report_error(
                                "assertion statement must be followed by action block",
                                kw.span
                            );
                        }
                        StmtKind::Assert {
                            kind: (),
                            expr,
                            success,
                            failure,
                        }
                    }
                }
            }
            TokenKind::Keyword(Keyword::Assume)
            | TokenKind::Keyword(Keyword::Cover)
            | TokenKind::Keyword(Keyword::Restrict) => self.unimplemented(),
            // randsequence_statement
            TokenKind::Keyword(Keyword::Randsequence) => self.unimplemented(),
            // randcase_statement
            TokenKind::Keyword(Keyword::Randcase) => self.unimplemented(),
            // expect_property_statement
            TokenKind::Keyword(Keyword::Expect) => self.unimplemented(),
            // block_item_declaration -> data_declaration
            // data_declaration. Either begin with const/var or explicit data type.
            TokenKind::Keyword(Keyword::Const)
            | TokenKind::Keyword(Keyword::Var)
            | TokenKind::Keyword(Keyword::Automatic)
            | TokenKind::Keyword(Keyword::Static)
            | TokenKind::IntAtomTy(_)
            | TokenKind::IntVecTy(_)
            | TokenKind::Keyword(Keyword::Reg)
            | TokenKind::RealTy(_)
            | TokenKind::Keyword(Keyword::Struct)
            | TokenKind::Keyword(Keyword::Union)
            | TokenKind::Keyword(Keyword::Enum)
            | TokenKind::Keyword(Keyword::String)
            | TokenKind::Keyword(Keyword::Chandle)
            | TokenKind::Keyword(Keyword::Virtual)
            | TokenKind::Keyword(Keyword::Event)
            | TokenKind::Keyword(Keyword::Type)
            | TokenKind::Keyword(Keyword::Void) => {
                StmtKind::DataDecl(Box::new(self.parse_data_decl(None)))
            }
            _ => {
                let expr = self.parse_unwrap(Self::parse_assign_expr_opt);
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
            TokenKind::Hash => self.unimplemented(),
            TokenKind::CycleDelay => self.unimplemented(),
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
                    TokenKind::DelimGroup(Delim::Paren, _) => TimingCtrl::ExprEventCtrl(Box::new(
                        self.parse_delim(Delim::Paren, Self::parse_event_expr),
                    )),
                    _ => {
                        let scope = self.parse_scope();
                        let id = self.parse_unwrap(|this| this.parse_hier_id(scope));
                        TimingCtrl::NameEventCtrl(id.value)
                    }
                }
            }
            _ => unreachable!(),
        }
    }

    fn parse_event_expr_item(&mut self) -> EventExpr {
        if let Some(v) = self.parse_if_delim(Delim::Paren, Self::parse_event_expr) {
            return EventExpr::Paren(Box::new(v));
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
        EventExpr::Item(Box::new(EventExprItem { edge, expr, iff }))
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

    fn parse_case_stmt(&mut self, uniq: Option<UniqPrio>) -> StmtKind {
        let kw = if let TokenKind::CaseKw(kw) = *self.consume() {
            kw
        } else {
            unreachable!()
        };
        let expr = Box::new(self.parse_delim(Delim::Paren, Self::parse_expr));
        match **self.peek() {
            TokenKind::Keyword(Keyword::Matches) | TokenKind::Keyword(Keyword::Inside) => {
                self.unimplemented()
            }
            _ => (),
        }
        let items = self.parse_list(|this| {
            let arm = match **this.peek() {
                TokenKind::Keyword(Keyword::Endcase) => return None,
                TokenKind::Keyword(Keyword::Default) => {
                    // Default match arm
                    this.consume();
                    Vec::new()
                }
                _ => {
                    // Expression match arms
                    this.parse_comma_list(false, false, Self::parse_expr_opt)
                }
            };
            this.expect(TokenKind::Colon);
            let stmt = this.parse_stmt();
            Some((arm, stmt))
        });
        self.expect(TokenKind::Keyword(Keyword::Endcase));
        StmtKind::Case {
            uniq,
            kw,
            expr,
            items,
        }
    }

    //
    // A.6.8 Looping statements
    //

    // ```bnf
    /// for_initialization ::=
    ///   list_of_variable_assignments | for_variable_declaration { , for_variable_declaration }
    /// for_variable_declaration ::=
    ///   [ var ] data_type variable_identifier = expression { , variable_identifier = expression }
    /// for_step ::=
    ///   for_step_assignment { , for_step_assignment }
    /// for_step_assignment ::=
    ///   operator_assignment | inc_or_dec_expression | function_subroutine_call
    // ```
    fn parse_for(&mut self) -> StmtKind {
        self.consume();
        let (ty, init, cond, update) = self.parse_delim(Delim::Paren, |this| {
            // First parse initialisation list.
            let (ty, init) = if let TokenKind::Semicolon = **this.peek() {
                (None, Vec::new())
            } else {
                // Consume "var" keyword if any. This keyword serves no special purpose and thus is
                // not reflected in AST.
                let var_kw = this.check(TokenKind::Keyword(Keyword::Var));
                // First try to parse as an assignment
                let assign = this.parse_assign_expr();
                let (ty, assign) = match assign.value {
                    ExprKind::Assign(..) => {
                        if var_kw {
                            this.diag.report_error(
                                "data type must follow 'var' inside for initialization",
                                assign.span
                            );
                        }
                        (None, assign)
                    }
                    _ => {
                        // If we this is not an assignment, it must be a data type. Convert it and
                        // parse a new assignment.
                        let span = assign.span;
                        let ty = match this.conv_expr_to_type(assign) {
                            None => {
                                this.diag.report_error("expected data type", span);
                                // Error recovery
                                None
                            },
                            Some(v) => Some(Box::new(v)),
                        };
                        let assign = this.parse_assign_expr();
                        (ty, assign)
                    }
                };
                let mut list = vec![assign];
                while this.check(TokenKind::Comma) {
                    list.push(this.parse_assign_expr());
                }

                // Now check expressions to make sure they are actually all proper assignment expressions
                this.check_list(&mut list, |this, assign| {
                    match assign.value {
                        ExprKind::Assign(ref lhs, _) => {
                            if ty.is_some() {
                                // When type is specified this must be a simple name
                                match lhs.value {
                                    ExprKind::HierName(HierId::Name(None, _)) => (),
                                    _ => {
                                        this.diag.report_error(
                                            "expecting loop variable name inside for initialization",
                                            lhs.span
                                        );
                                        return false
                                    }
                                }
                            }
                        }
                        _ => {
                            this.diag.report_error(
                                "expected assignment expression inside for initialization",
                                assign.span
                            );
                            return false
                        }
                    }
                    return true
                });

                (ty, list)
            };

            this.expect(TokenKind::Semicolon);
            let cond = this.parse_expr_opt().map(Box::new);
            this.expect(TokenKind::Semicolon);
            let update = this.parse_comma_list(true, false, Self::parse_assign_expr_opt);
            (ty, init, cond, update)
        });
        let body = Box::new(self.parse_stmt());
        StmtKind::For {
            ty,
            init,
            cond,
            update,
            body,
        }
    }

    //
    // A.6.7.1 Patterns
    //

    fn parse_assign_pattern(&mut self) -> AssignPattern {
        self.parse_delim(Delim::TickBrace, |this| {
            let expr = match this.parse_expr_opt() {
                None => {
                    let span = this.peek().span;
                    this.diag.report_span(
                        Severity::Error,
                        "assignment pattern cannot be empty",
                        span,
                    );
                    // Error recovery: return a empty list
                    return AssignPattern::Simple(Vec::new());
                }
                Some(v) => v,
            };
            match **this.peek() {
                TokenKind::Comma => {
                    // Simple pattern
                    let mut list = vec![expr];
                    while this.check(TokenKind::Comma) {
                        list.push(this.parse_expr());
                    }
                    AssignPattern::Simple(list)
                }
                TokenKind::Colon => {
                    // Keyed pattern
                    this.consume();
                    let val = this.parse_expr();
                    let mut list = vec![(expr, val)];
                    while this.check(TokenKind::Comma) {
                        let key = this.parse_expr();
                        this.expect(TokenKind::Colon);
                        let expr = this.parse_expr();
                        list.push((key, expr));
                    }
                    AssignPattern::Keyed(list)
                }
                TokenKind::DelimGroup(Delim::Paren, _) => {
                    let repeated = this.parse_expr();
                    let list = match repeated.value {
                        ExprKind::Concat(list, None) => list,
                        _ => {
                            this.diag.report_span(
                                Severity::Error,
                                "expected a simple list of expressions",
                                repeated.span,
                            );
                            vec![repeated]
                        }
                    };
                    AssignPattern::Mult(Box::new(expr), list)
                }
                // Otherwise just return a pattern with one element.
                // If there are extra tokens, parse_delim will catch them for us.
                _ => AssignPattern::Simple(vec![expr]),
            }
        })
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
        let args = self.parse_args_opt(false);
        SysTfCall { task, args }
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
    fn parse_assign_expr_opt(&mut self) -> Option<Expr> {
        self.leq_as_assign = true;
        let expr = self.parse_expr_opt();
        self.leq_as_assign = false;
        let expr = match expr {
            None => return None,
            Some(v) => v,
        };

        match **self.peek() {
            TokenKind::Assign => {
                self.consume();
                let rhs = self.parse_expr();
                let span = expr.span.merge(rhs.span);
                Some(Spanned::new(
                    ExprKind::Assign(Box::new(expr), Box::new(rhs)),
                    span,
                ))
            }
            TokenKind::BinaryOp(BinaryOp::Leq) => {
                self.consume();
                let rhs = self.parse_expr();
                let span = expr.span.merge(rhs.span);
                Some(Spanned::new(
                    ExprKind::NonblockAssign(Box::new(expr), Box::new(rhs)),
                    span,
                ))
            }
            TokenKind::BinaryOpAssign(op) => {
                self.consume();
                let rhs = self.parse_expr();
                let span = expr.span.merge(rhs.span);
                Some(Spanned::new(
                    ExprKind::BinaryAssign(Box::new(expr), op, Box::new(rhs)),
                    span,
                ))
            }
            _ => Some(expr),
        }
    }

    fn parse_assign_expr(&mut self) -> Expr {
        self.parse_unwrap(Self::parse_assign_expr_opt)
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
            TokenKind::BinaryOp(BinaryOp::Imply) | TokenKind::BinaryOp(BinaryOp::Equiv) => {
                let span = self.peek().span;
                self.diag.report_fatal("-> and <-> not yet supported", span);
            }
            _ => Some(lhs),
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_unwrap(Self::parse_expr_opt)
    }

    fn parse_cond_expr_opt(&mut self) -> Option<Expr> {
        let expr = self.parse_cond_pred_opt()?;
        if self.check(TokenKind::Question) {
            let attr = self.parse_attr_inst_opt();
            let true_expr = Box::new(self.parse_expr());
            self.expect(TokenKind::Colon);
            let false_expr = Box::new(self.parse_expr());
            let span = expr.span.merge(false_expr.span);
            Some(Spanned::new(
                ExprKind::Cond(Box::new(expr), attr, true_expr, false_expr),
                span,
            ))
        } else {
            Some(expr)
        }
    }

    fn parse_cond_pred_opt(&mut self) -> Option<Expr> {
        let expr = self.parse_cond_pattern_opt()?;
        if self.check(TokenKind::TripleAnd) {
            let span = self.peek().span;
            self.diag
                .report_fatal("expression_or_cond_pattern not yet supported", span);
        } else {
            Some(expr)
        }
    }

    fn parse_cond_pattern_opt(&mut self) -> Option<Expr> {
        let expr = match **self.peek() {
            // tagged_union_expression
            TokenKind::Keyword(Keyword::Tagged) => {
                let span = self.peek().span;
                self.diag
                    .report_fatal("tagged_union_expression not yet supported", span);
            }
            _ => self.parse_bin_expr(0)?,
        };
        if self.check(TokenKind::Keyword(Keyword::Matches)) {
            let span = self.peek().span;
            self.diag
                .report_fatal("cond_pattern not yet supported", span);
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
            let leq_as_assign = self.leq_as_assign;
            let (op, new_prec) = match **self.peek() {
                TokenKind::BinaryOp(BinaryOp::Leq) if leq_as_assign => break,
                TokenKind::BinaryOp(op) => {
                    let new_prec = Self::get_bin_op_prec(op);
                    // Can only proceed if precedence is higher
                    if new_prec <= prec {
                        break;
                    }
                    (op, new_prec)
                }
                TokenKind::LShl => {
                    let new_prec = Self::get_bin_op_prec(BinaryOp::Shl);
                    // Can only proceed if precedence is higher
                    if new_prec <= prec {
                        break;
                    }
                    (BinaryOp::Shl, new_prec)
                }
                TokenKind::Keyword(Keyword::Inside) | TokenKind::Keyword(Keyword::Dist)
                    if 7 > prec =>
                {
                    // 7 is the precedence of comparison operator.
                    let span = self.peek().span;
                    self.diag
                        .report_fatal("inside & dist not yet supported", span);
                }
                _ => break,
            };

            self.consume();
            let attr = self.parse_attr_inst_opt();
            let rhs = self.parse_unwrap(|this| this.parse_bin_expr(new_prec));
            let span = expr.span.merge(rhs.span);
            expr = Spanned::new(
                ExprKind::Binary(Box::new(expr), op, attr, Box::new(rhs)),
                span,
            );
        }

        Some(expr)
    }

    fn parse_unary_expr(&mut self) -> Option<Expr> {
        let op = match **self.peek() {
            // inc_or_dec_operator { attribute_instance } variable_lvalue
            // unary_operator { attribute_instance } primary
            TokenKind::IncDec(incdec) => {
                let span = self.consume().span;
                let attr = self.parse_attr_inst_opt();
                let expr = self.parse_unwrap(Self::parse_primary);
                let span = span.merge(expr.span);
                return Some(Spanned::new(
                    ExprKind::PrefixIncDec(incdec, attr, Box::new(expr)),
                    span,
                ));
            }
            TokenKind::UnaryOp(op) => op,
            TokenKind::BinaryOp(BinaryOp::Add) => UnaryOp::Add,
            TokenKind::BinaryOp(BinaryOp::Sub) => UnaryOp::Sub,
            TokenKind::BinaryOp(BinaryOp::And) => UnaryOp::And,
            TokenKind::BinaryOp(BinaryOp::Or) => UnaryOp::Or,
            TokenKind::BinaryOp(BinaryOp::Xor) => UnaryOp::Xor,
            TokenKind::BinaryOp(BinaryOp::Xnor) => UnaryOp::Xnor,
            _ => {
                let expr = self.parse_primary()?;
                return match **self.peek() {
                    // Inc/dec with attributes
                    TokenKind::DelimGroup(Delim::Attr, _) => match **self.peek_n(1) {
                        TokenKind::IncDec(incdec) => {
                            let attr = self.parse_attr_inst_opt();
                            let span = expr.span.merge(self.consume().span);
                            Some(Spanned::new(
                                ExprKind::PostfixIncDec(Box::new(expr), attr, incdec),
                                span,
                            ))
                        }
                        _ => Some(expr),
                    },
                    // Inc/dec without attributes
                    TokenKind::IncDec(incdec) => {
                        let span = expr.span.merge(self.consume().span);
                        Some(Spanned::new(
                            ExprKind::PostfixIncDec(Box::new(expr), None, incdec),
                            span,
                        ))
                    }
                    _ => Some(expr),
                };
            }
        };
        let span = self.consume().span;
        let attr = self.parse_attr_inst_opt();
        let expr = self.parse_unwrap(Self::parse_primary);
        let span = span.merge(expr.span);
        Some(Spanned::new(
            ExprKind::Unary(op, attr, Box::new(expr)),
            span,
        ))
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
            return Some(expr);
        }
        let typ = Box::new(self.parse_expr());
        self.expect(TokenKind::Colon);
        let max = Box::new(self.parse_expr());
        let span = expr.span.merge(max.span);
        Some(Spanned::new(
            ExprKind::MinTypMax(Box::new(expr), typ, max),
            span,
        ))
    }

    //
    // A.8.4 Primaries
    //

    /// Parse primary expression (or data_type) with cast.
    fn parse_primary(&mut self) -> Option<Expr> {
        let mut expr = match **self.peek() {
            TokenKind::Keyword(Keyword::Const) => {
                let span = self.consume().span;
                self.expect(TokenKind::Tick);
                let expr = self.parse_delim_spanned(Delim::Paren, |this| {
                    this.parse_unwrap(Self::parse_expr_opt)
                });
                Spanned::new(
                    ExprKind::ConstCast(Box::new(expr.value)),
                    span.merge(expr.span),
                )
            }
            TokenKind::Signing(sign) => {
                if let TokenKind::Tick = **self.peek_n(1) {
                    let span = self.consume().span;
                    self.expect(TokenKind::Tick);
                    let expr = self.parse_delim_spanned(Delim::Paren, |this| {
                        this.parse_unwrap(Self::parse_expr_opt)
                    });
                    Spanned::new(
                        ExprKind::SignCast(sign, Box::new(expr.value)),
                        span.merge(expr.span),
                    )
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
            },
        };
        loop {
            if self.consume_if(TokenKind::Tick).is_none() {
                break;
            }

            let nexpr = self
                .parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt));
            let span = expr.span.merge(nexpr.span);
            expr = Spanned::new(
                ExprKind::TypeCast(Box::new(expr), Box::new(nexpr.value)),
                span,
            )
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
            TokenKind::RealLiteral(_)
            | TokenKind::IntegerLiteral(_)
            | TokenKind::TimeLiteral(_)
            | TokenKind::UnbasedLiteral(_)
            | TokenKind::StringLiteral(_)
            | TokenKind::Dollar
            | TokenKind::Keyword(Keyword::Null) => {
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
                        TokenKind::BinaryOp(BinaryOp::LShr) | TokenKind::LShl => {
                            let span = this.peek().span;
                            this.diag
                                .report_fatal("streaming concat is not yet supported", span);
                        }
                        _ => {
                            let expr = this.parse_expr();
                            if let TokenKind::DelimGroup(Delim::Brace, _) = **this.peek() {
                                // This is a multiple concatenation
                                let concat = this.parse_unwrap(Self::parse_primary_nocast);
                                let select = this.parse_dim_opt().map(Box::new);
                                ExprKind::MultConcat(Box::new(expr), Box::new(concat), select)
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
                                let select = this.parse_dim_opt().map(Box::new);
                                ExprKind::Concat(list, select)
                            }
                        }
                    }
                }))
            }
            // assignment_pattern_expression
            TokenKind::DelimGroup(Delim::TickBrace, _) => {
                let span = self.peek().span;
                let pat = self.parse_assign_pattern();
                Some(Spanned::new(ExprKind::AssignPattern(None, pat), span))
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
            TokenKind::DelimGroup(Delim::Bracket, _)
            | TokenKind::IntAtomTy(_)
            | TokenKind::IntVecTy(_)
            | TokenKind::Keyword(Keyword::Reg)
            | TokenKind::RealTy(_)
            | TokenKind::Keyword(Keyword::Struct)
            | TokenKind::Keyword(Keyword::Union)
            | TokenKind::Keyword(Keyword::Enum)
            | TokenKind::Keyword(Keyword::String)
            | TokenKind::Keyword(Keyword::Chandle)
            | TokenKind::Keyword(Keyword::Virtual)
            | TokenKind::Keyword(Keyword::Event)
            | TokenKind::Keyword(Keyword::Type)
            | TokenKind::Keyword(Keyword::Void) => {
                let ty = Box::new(self.parse_kw_data_type());
                let span = ty.span;
                Some(Spanned::new(ExprKind::Type(ty), span))
            }
            // Otherwise, parse as name
            _ => {
                let begin_span = self.peek().span;
                let scope = self.parse_scope();
                let id = self.parse_hier_id(scope);

                // Not a primary expressison
                if id.is_none() {
                    None
                } else {
                    // TODO: This is a hack. Could do better
                    let span = begin_span.start.span_to(self.peek().span.end);
                    let expr = Spanned::new(ExprKind::HierName(id.unwrap().value), span);

                    match **self.peek() {
                        // If next is '{, then this is actually an assignment pattern
                        TokenKind::DelimGroup(Delim::TickBrace, _) => {
                            let ty = match self.conv_expr_to_type(expr) {
                                None => {
                                    self.diag.report_fatal("expected data type", span);
                                    // TODO: Error recovery
                                }
                                Some(v) => v,
                            };
                            let span = span.merge(self.peek().span);
                            let pat = self.parse_assign_pattern();
                            Some(Spanned::new(
                                ExprKind::AssignPattern(Some(Box::new(ty)), pat),
                                span,
                            ))
                        }
                        // This can be either function call or inc/dec expression, peek ahead
                        TokenKind::DelimGroup(Delim::Attr, _) => {
                            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(1) {
                                let span = expr.span.merge(self.peek().span);
                                let attr = self.parse_attr_inst_opt();
                                let args = self.parse_args_opt(false).map(Box::new);
                                Some(Spanned::new(
                                    ExprKind::FuncCall {
                                        expr: Box::new(expr),
                                        attr,
                                        args,
                                    },
                                    span,
                                ))
                            } else {
                                Some(expr)
                            }
                        }
                        // Function call
                        TokenKind::DelimGroup(Delim::Paren, _) => {
                            let span = expr.span.merge(self.peek().span);
                            let args = self.parse_args_opt(false).map(Box::new);
                            Some(Spanned::new(
                                ExprKind::FuncCall {
                                    expr: Box::new(expr),
                                    attr: None,
                                    args,
                                },
                                span,
                            ))
                        }
                        _ => Some(expr),
                    }
                }
            }
        }
    }

    //
    // A.8.6 Operators
    //

    /// Get precedence of binary operator. Exclude -> and ->>
    fn get_bin_op_prec(op: BinaryOp) -> i32 {
        match op {
            BinaryOp::Power => 11,
            BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 10,
            BinaryOp::Add | BinaryOp::Sub => 9,
            BinaryOp::Shl | BinaryOp::LShr | BinaryOp::AShr => 8,
            BinaryOp::Lt | BinaryOp::Leq | BinaryOp::Gt | BinaryOp::Geq => 7,
            BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::CaseEq
            | BinaryOp::CaseNeq
            | BinaryOp::WildEq
            | BinaryOp::WildNeq => 6,
            BinaryOp::And => 5,
            BinaryOp::Xor | BinaryOp::Xnor => 4,
            BinaryOp::Or => 3,
            BinaryOp::LAnd => 2,
            BinaryOp::LOr => 1,
            _ => unreachable!(),
        }
    }

    //
    // A.9.1 Attributes
    //
    fn parse_attr_inst_opt(&mut self) -> Option<Box<AttrInst>> {
        let attr = self.parse_if_delim_spanned(Delim::Attr, |this| {
            AttrInstStruct(
                this.parse_comma_list(false, false, |this| match this.consume_if_id() {
                    None => None,
                    Some(name) => {
                        let expr = if this.check(TokenKind::Assign) {
                            Some(Box::new(this.parse_expr()))
                        } else {
                            None
                        };
                        Some(AttrSpec { name, expr })
                    }
                }),
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
                        self.diag.report_span(
                            Severity::Error,
                            "local scope can only be the outermost scope",
                            tok.span,
                        );
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::ScopeSep);
                }
                TokenKind::Keyword(Keyword::Unit) => {
                    let tok = self.consume();
                    if let Some(_) = scope {
                        self.diag.report_span(
                            Severity::Error,
                            "$unit scope can only be the outermost scope",
                            tok.span,
                        );
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::ScopeSep);
                }
                TokenKind::Id(_) => {
                    // Lookahead to check if this is actually a scope
                    match **self.peek_n(1) {
                        TokenKind::ScopeSep => (),
                        TokenKind::Hash => {
                            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(2) {
                                if let TokenKind::ScopeSep = **self.peek_n(3) {
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        }
                        _ => break,
                    };
                    let ident = self.expect_id();
                    if self.consume_if(TokenKind::Hash).is_some() {
                        // TODO: Add parameter support
                        self.diag
                            .report_fatal("class parameter scope is not yet supported", ident.span);
                    }
                    self.expect(TokenKind::ScopeSep);
                    scope = Some(Scope::Name(scope.map(Box::new), Box::new(ident)))
                }
                _ => break,
            }
        }
        scope
    }

    /// Parse hierachical identifier
    fn parse_hier_id(&mut self, scope: Option<Scope>) -> Option<Spanned<HierId>> {
        // Parse the leading hierachical name
        let mut id = match **self.peek() {
            TokenKind::Keyword(Keyword::This) => {
                let token = self.consume();
                if let TokenKind::Dot = self.peek().value {
                    if let TokenKind::Keyword(Keyword::Super) = self.peek_n(1).value {
                        self.consume();
                        let token2 = self.consume();
                        Spanned::new(HierId::Super(scope), token.span.merge(token2.span))
                    } else {
                        Spanned::new(HierId::This(scope), token.span)
                    }
                } else {
                    Spanned::new(HierId::This(scope), token.span)
                }
            }
            TokenKind::Keyword(Keyword::Super) => {
                let token = self.consume();
                Spanned::new(HierId::Super(scope), token.span)
            }
            TokenKind::Keyword(Keyword::Root) => {
                let token = self.consume();
                if scope.is_some() {
                    self.diag
                        .report_error("$root cannot follow a scope", token.span);
                }
                Spanned::new(HierId::Root, token.span)
            }
            TokenKind::Id(_) => {
                let span = self.peek().span;
                Spanned::new(HierId::Name(scope, Box::new(self.expect_id())), span)
            }
            _ => {
                // If we've seen the scopes then we must need to see the id
                if scope.is_some() {
                    let span = self.peek().span;
                    self.diag.report_span(
                        Severity::Error,
                        "expected identifiers after scope",
                        span,
                    );
                    // Error recovery
                    return Some(Spanned::new(
                        HierId::Name(scope, Box::new(Ident::new_unspanned("".to_owned()))),
                        Span::none(),
                    ));
                } else {
                    return None;
                }
            }
        };

        loop {
            match **self.peek() {
                TokenKind::DelimGroup(Delim::Bracket, _) => {
                    let sel = self.parse_dim_opt().unwrap();
                    let span = id.span.merge(sel.span);
                    id = Spanned::new(HierId::Select(Box::new(id), Box::new(sel)), span);
                }
                TokenKind::Dot => {
                    self.consume();
                    let subid = self.expect_id();
                    let span = id.span.merge(subid.span);
                    id = Spanned::new(HierId::Member(Box::new(id), Box::new(subid)), span);
                }
                _ => return Some(id),
            }
        }
    }
}
