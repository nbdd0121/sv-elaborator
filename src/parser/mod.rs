pub mod ast;

use self::ast::*;
use super::lexer::{Token, TokenKind, Keyword, Operator, Delim, DelimGroup};
use super::source::{SrcMgr, Diagnostic, DiagMgr, Severity, Span, Spanned};

use std::result;
use std::mem;
use std::rc::Rc;
use std::collections::VecDeque;
use std::borrow::Borrow;

pub struct Parser {
    mgr: Rc<SrcMgr>,
    diag: Rc<DiagMgr>,
    lexer: VecDeque<Token>,
}

//
// Macros for coding more handily
//

macro_rules! scope {
    ($t:expr) => {
        macro_rules! parse {
            ([expr]) => {
                $t.parse_expr_opt()?
            };
            (expr) => {
                $t.parse_unwrap(Self::parse_expr_opt)?
            };
            (box(expr)) => {
                Box::new($t.parse_unwrap(Self::parse_expr_opt)?)
            };
            ([$rule:ident]) => {
                $t.rule()?
            };
            ($rule:ident) => {
                $t.parse_unwrap(Self::$rule)?
            };
            (box($rule:ident)) => {
                Box::new($t.parse_unwrap(Self::$rule)?)
            };
        }
    }
}

type Result<T> = result::Result<T, ()>;

//
// Data types internal to parser
//
#[derive(Clone, Copy, PartialEq, Eq)]
enum ArgOption {
    Param,
    Port,
    Arg,
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
/// Some of them are easy to check:
/// * As module/interfaces/etc can be declared in other files, to disambiguate hierachical
/// instantiation we need to lookahead a check if it is an instantation.
/// * For interface port declaration, we only need to lookahead if it is of form "id.id id"
///
/// But for net-type and data-type, we actually need to have knowledge about identifiers to make
/// correct decision because we cannot tell apart from net-type + implicit data-type vs data-type.
/// In cases where data type can be implicit, we can still perform lookahead to check if an
/// identifier followes, but we can't really do anything for param expression.
impl Parser {
    pub fn new(mgr: Rc<SrcMgr>, diag: Rc<DiagMgr>, lexer: VecDeque<Token>) -> Parser {
        Parser {
            mgr,
            diag,
            lexer: lexer,
        }
    }

    fn consume(&mut self) -> Token {
        match self.lexer.pop_front() {
            Some(v) => v,
            None => Token::eof(),
        }
    }

    fn peek(&mut self) -> &Token {
        match self.lexer.front() {
            Some(v) => v,
            None => Token::eof_ref(),
        }
    }

    fn peek_n(&mut self, n: usize) -> &Token {
        if self.lexer.len() > n {
            &self.lexer[n]
        } else {
            Token::eof_ref()
        }
    }

    fn pushback(&mut self, tok: Token) {
        self.lexer.push_front(tok);
    }

    /// Parse a delimited group of tokens. After calling the callback, the token stream must
    /// be empty.
    fn delim_group<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self, mut stream: VecDeque<Token>, mut f: F
    ) -> Result<T> {
        mem::swap(&mut self.lexer, &mut stream);
        let ret = f(self)?;
        self.expect_eof()?;
        mem::swap(&mut self.lexer, &mut stream);
        Ok(ret)
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

    fn expect_delim(&mut self, expected: Delim) -> Result<Box<DelimGroup>> {
        match self.consume_if_delim(expected) {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, format!("expected open delimiter {:#?}", expected), span.clone())?;
                // Error recovery
                let fake_open = Spanned::new(TokenKind::Unknown, span);
                let fake_close = Spanned::new(TokenKind::Unknown, span);
                Ok(Box::new(DelimGroup {
                    open: fake_open,
                    close: fake_close,
                    tokens: VecDeque::new(),
                }))
            }
            Some(v) => Ok(v),
        }
    }

    fn expect_eof(&mut self) -> Result<()> {
        if let TokenKind::Eof = self.peek().value {} else {
            let span = self.peek().span;
            self.report_span(Severity::Error, "unexpected extra token", span)?;
        }
        Ok(())
    }

    fn expect_id(&mut self) -> Result<Ident> {
        match self.consume_if_id() {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, "expected identifier", span.clone())?;
                // Error recovery
                Ok(Spanned::new("".to_owned(), span))
            }
            Some(v) => Ok(v),
        }
    }

    fn expect<T: Borrow<TokenKind>>(&mut self, token: T) -> Result<Token> {
        let token = token.borrow();
        match self.consume_if(token) {
            None => {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, format!("expected token {:?}", token), span.clone())?;
                // Error recovery
                Ok(Spanned::new(TokenKind::Unknown, span))
            }
            Some(v) => Ok(v),
        }
    }

    fn report_span<M: Into<String>>(&self, severity: Severity, msg: M, span: Span) -> Result<()> {
        self.report_diag(Diagnostic::new(
            severity,
            msg.into(),
            span,
        ))
    }

    fn report_diag(&self, diag: Diagnostic) -> Result<()> {
        self.diag.report(diag)
    }

    //
    // Utility functions
    //

    /// Unwrap `Option` with sensible error message
    fn unwrap<T: AstNode>(&mut self, t: Option<T>) -> Result<T> {
        match t {
            None => {
                let span = self.peek().span;
                match T::recovery(span) {
                    None => {
                        self.report_span(
                            Severity::Fatal,
                            format!("{} support is not completed yet", T::name()),
                            span
                        )?;
                        Err(())
                    }
                    Some(v) => {
                        self.report_span(
                            Severity::Error,
                            format!("expected {}", T::name()),
                            span
                        )?;
                        Ok(v)
                    }
                }
            }
            Some(v) => Ok(v),
        }
    }

    /// Unwrap `Option` with sensible error message
    fn parse_unwrap<T: AstNode, F: FnMut(&mut Self) -> Result<Option<T>>>(
        &mut self, mut f: F
    ) -> Result<T> {
        let result = f(self)?;
        self.unwrap(result)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self, delim: Delim, f: F
    ) -> Result<T> {
        let delim = self.expect_delim(delim)?;
        self.delim_group(delim.tokens, f)
    }

    /// Expect the next token tree to be a delimited group, parse it with given function.
    fn parse_delim_spanned<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self, delim: Delim, f: F
    ) -> Result<Spanned<T>> {
        let span = self.peek().span;
        let delim = self.expect_delim(delim)?;
        Ok(Spanned::new(self.delim_group(delim.tokens, f)?, span))
    }


    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self, delim: Delim, f: F
    ) -> Result<Option<T>> {
        match self.consume_if_delim(delim) {
            None => Ok(None),
            Some(v) => Ok(Some(self.delim_group(v.tokens, f)?))
        }
    }

    /// If the next token tree to be a delimited group, parse it with given function, otherwise
    /// return `None`.
    fn parse_if_delim_spanned<T, F: FnMut(&mut Self) -> Result<T>>(
        &mut self, delim: Delim, f: F
    ) -> Result<Option<Spanned<T>>> {
        match **self.peek() {
            TokenKind::DelimGroup(d, _) if d == delim => (),
            _ => return Ok(None),
        }
        let token = self.consume();
        if let TokenKind::DelimGroup(_, grp) = token.value {
            Ok(Some(Spanned::new(self.delim_group(grp.tokens, f)?, token.span)))
        } else {
            unreachable!();
        }
    }

    /// Parse until `None` is returned, and organize parsed items into a list.
    fn parse_list<T, F: FnMut(&mut Self) -> Result<Option<T>>>(&mut self, mut f: F) -> Result<Vec<T>> {
        let mut vec = Vec::new();
        loop {
            let result = f(self)?;
            match result {
                None => break,
                Some(v) => vec.push(v),
            }
        }
        Ok(vec)
    }

    /// Parse a comma seperated list. We require `F` to return a `Option<T>` as it will make
    /// diagnostics easier by being able to catch trailing comma easily.
    /// * `empty`: If true, empty list is allowed
    /// * `trail`: If true, trailing comma is allowed
    fn parse_comma_list<T, F: FnMut(&mut Self) -> Result<Option<T>>>(
        &mut self, empty: bool, trail: bool, mut f: F
    ) -> Result<Vec<T>> {
        let mut vec = Vec::new();

        // Parse first element
        let result = f(self)?;
        match result {
            None => {
                // If we failed and this is the first element, then we get an empty list
                if !empty {
                    let span = self.peek().span.clone();
                    self.report_span(Severity::Error, "empty list not allowed", span)?;
                }
                return Ok(vec)
            }
            Some(v) => vec.push(v),
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Operator(Operator::Comma)) {
                None => break,
                Some(v) => v,
            };
            let result = f(self)?;
            match result {
                None => {
                    if !trail {
                        // TODO: We could place a FixItHint here.
                        self.report_span(
                            Severity::Error,
                            "trailing comma is not allowed; consider removing it",
                            comma.span
                        )?;
                    }
                    break;
                }
                Some(v) => vec.push(v),
            }
        }

        Ok(vec)
    }

    /// Parse a comma seperated list, but the list will be built externally. Similarly to above
    /// expects boolean instead of Option<T>.
    /// * `empty`: If true, empty list is allowed
    /// * `trail`: If true, trailing comma is allowed
    fn parse_comma_list_unit<F: FnMut(&mut Self) -> Result<bool>>(
        &mut self, mut f: F, empty: bool, trail: bool
    ) -> Result<()> {
        // Parse first element
        if !f(self)? {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span.clone();
                self.report_span(Severity::Error, "empty list not allowed", span)?;
            }
            return Ok(())
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Operator(Operator::Comma)) {
                None => break,
                Some(v) => v,
            };
            if !f(self)? {
                if !trail {
                    // TODO: We could place a FixItHint here.
                    self.report_span(
                        Severity::Error,
                        "trailing comma is not allowed; consider removing it",
                        comma.span
                    )?;
                }
                break;
            }
        }

        Ok(())
    }

    /// Parse a seperated list, but do not attempt to build a vector.
    fn parse_sep_list_unit<F: FnMut(&mut Self) -> Result<bool>>(
        &mut self, sep: Operator, empty: bool, trail: bool, mut f: F
    ) -> Result<()> {
        // Parse first element
        if !f(self)? {
            // If we failed and this is the first element, then we get an empty list
            if !empty {
                let span = self.peek().span;
                self.report_span(Severity::Error, "empty list not allowed", span)?;
            }
            return Ok(())
        }

        loop {
            // Consume comma if there is some, break otherwise
            let comma = match self.consume_if(TokenKind::Operator(sep)) {
                None => break,
                Some(v) => v,
            };
            if !f(self)? {
                if !trail {
                    // TODO: We could place a FixItHint here.
                    self.report_span(
                        Severity::Error,
                        format!("trailing {:#?} is not allowed; consider removing it", sep),
                        comma.span
                    )?;
                }
                break;
            }
        }

        Ok(())
    }

    /// Check if the list contains invalid elements, and remove them.
    fn check_list<T, F: FnMut(&mut Self, &T) -> Result<bool>>(
        &mut self, mut f: F, list: &mut Vec<T>
    ) -> Result<()> {
        let mut okay = true;
        list.retain(|x| {
            if !okay { return true; }
            match f(self, x) {
                Ok(v) => v,
                Err(_) => {
                    okay = false;
                    true
                }
            }
        });
        if okay { Ok(()) } else { Err(()) }
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
    fn parse_item_opt(&mut self) -> Result<Option<Item>> {
        let mut attr = self.parse_attr_inst_opt()?;
        match self.peek().value {
            TokenKind::Eof |
            TokenKind::Keyword(Keyword::Endmodule) |
            TokenKind::Keyword(Keyword::Endgenerate) |
            TokenKind::Keyword(Keyword::End) => Ok(None),
            // Externs are parsed together (even though they're not currently supported yet)
            TokenKind::Keyword(Keyword::Extern) => {
                let clone = self.peek().span.clone();
                self.report_span(Severity::Fatal, "extern is not supported", clone)?;
                unreachable!()
            }
            // module_declaration
            TokenKind::Keyword(Keyword::Module) => Ok(Some(self.parse_module()?)),
            // continuous_assign
            TokenKind::Keyword(Keyword::Assign) => Ok(Some(self.parse_continuous_assign()?)),
            // generate_region
            TokenKind::Keyword(Keyword::Generate) => {
                let kw = self.consume();
                self.report_span(Severity::Warning, "there is no need for generate region", kw.span)?;
                let list = self.parse_list(Self::parse_item_opt)?;
                self.expect(TokenKind::Keyword(Keyword::Endgenerate))?;
                Ok(Some(Item::GenRegion(list)))
            }
            // loop_generate_construct
            TokenKind::Keyword(Keyword::For) => Ok(Some(self.parse_loop_gen(attr)?)),
            // if_generate_construct
            TokenKind::Keyword(Keyword::If) => Ok(Some(self.parse_if_gen(attr)?)),
            // case_generate_construct
            TokenKind::Keyword(Keyword::Case) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "case_generate_construct is not supported", span)?;
                unreachable!()
            }
            // elaboration_system_task
            TokenKind::SystemTask(_) => {
                let tf = self.parse_sys_tf_call()?;
                self.expect(TokenKind::Operator(Operator::Semicolon))?;
                Ok(Some(Item::SysTfCall(Box::new(tf))))
            }
            TokenKind::Id(_) => {
                if let Some(v) = self.parse_instantiation(&mut attr)? {
                    Ok(Some(v))
                } else {
                    let clone = self.peek().span.clone();
                    self.report_span(Severity::Fatal, "not implemented", clone)?;
                    unreachable!()
                }
            }
            _ => {
                let clone = self.peek().span.clone();
                self.report_span(Severity::Fatal, "not implemented", clone)?;
                unreachable!()
            }
        }
    }

    fn parse_item(&mut self) -> Result<Item> {
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
    pub fn parse_source(&mut self) -> Result<Vec<Item>> {
        let list = self.parse_list(Self::parse_item_opt)?;
        Ok(list)
    }

    /// Parse a module. We processed attributes in parse_item_opt, and externs will not be processed
    /// here.
    ///
    /// Acccording to spec:
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
    /// We will need to check if items can legally appear in here.
    fn parse_module(&mut self) -> Result<Item> {
        self.expect(TokenKind::Keyword(Keyword::Module))?;
        let lifetime = self.parse_lifetime();
        let name = self.expect_id()?;
        // TODO Package import declaration
        let param = self.parse_param_port_list()?;
        let port = self.parse_port_list()?;
        self.expect(TokenKind::Operator(Operator::Semicolon))?;
        let items = self.parse_list(Self::parse_item_opt)?;
        self.expect(TokenKind::Keyword(Keyword::Endmodule))?;

        if self.consume_if(TokenKind::Operator(Operator::Colon)).is_some() {
            let id = self.expect_id()?;
            if *id != *name {
                self.report_span(
                    Severity::Error,
                    format!("identifer annotation at end does match declaration, should be '{}'", name),
                    id.span
                )?;
            }
        }

        Ok(Item::ModuleDecl(Box::new(ModuleDecl {
            lifetime,
            name,
            param,
            port: port.unwrap_or_else(|| Vec::new()),
            items: items,
        })))
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
    fn parse_param_port_list(&mut self) -> Result<Option<Vec<ParamDecl>>> {
        if self.consume_if(TokenKind::Operator(Operator::Hash)).is_none() {
            return Ok(None)
        }

        self.parse_delim(Delim::Paren, |this| {
            let mut vec = Vec::new();

            // Default to parameter and un-typed
            let mut param_decl = ParamDecl {
                kw: Keyword::Parameter,
                ty: None,
                list: Vec::new()
            };

            this.parse_comma_list_unit(|this| {
                // If a new keyword is seen update it.
                match **this.peek() {
                    TokenKind::Eof => return Ok(false),
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
                if let Some(v) = this.parse_data_type(true)? {
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

                let assign = this.parse_decl_assign()?;
                param_decl.list.push(assign);

                Ok(true)
            }, true, false)?;

            if !param_decl.list.is_empty() {
                vec.push(param_decl);
            }
            Ok(Some(vec))
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
    fn parse_port_list(&mut self) -> Result<Option<Vec<PortDecl>>> {
        self.parse_if_delim(Delim::Paren, |this| {
            if let Some(v) = this.consume_if(TokenKind::Operator(Operator::WildPattern)) {
                this.report_span(Severity::Fatal, "(.*) port declaration is not supported", v.span)?;
                unreachable!();
            }

            // If there are no ports, it doesn't matter about which style we're using.
            if this.consume_if_eof().is_some() {
                return Ok(Vec::new())
            }

            let mut ansi = true;
            let mut prev = None;
            let mut vec = Vec::new();

            this.parse_comma_list_unit(|this| {
                if this.consume_if_eof().is_some() {
                    return Ok(false)
                }

                let dirsp = this.peek().span.clone();
                let dir = this.parse_port_dir();

                // Could only appear in non-ANSI declaration
                if prev.is_none() {
                    match this.peek().value {
                        TokenKind::DelimGroup(Delim::Brace, _) => {
                            ansi = false;
                            return Ok(false)
                        }
                        _ => (),
                    }
                }

                // Explicit port declaration
                if let Some(_) = this.consume_if(TokenKind::Operator(Operator::Dot)) {
                    let name = Box::new(this.expect_id()?);
                    let expr = Box::new(this.parse_unwrap(|this| {
                        this.parse_delim(Delim::Paren, Self::parse_expr_opt)
                    })?);

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
                    return Ok(true)
                }

                // Parse net-type
                let net = if this.consume_if(TokenKind::Keyword(Keyword::Var)).is_some() {
                    Some(NetPortType::Variable)
                } else {
                    // TODO parse net-type
                    None
                };

                // Parse data-type
                let dtype = this.parse_data_type(true)?;

                // If both none, then there is a chance that this is an interface port
                if net.is_none() && dtype.is_none() {
                    let is_intf = if let TokenKind::Keyword(Keyword::Interface) = **this.peek() {
                        // Okay, this is indeed an interface port
                        this.consume();
                        if this.consume_if(TokenKind::Operator(Operator::Dot)).is_some() {
                            let modport = this.expect_id()?;
                            Some((None, Some(Box::new(modport))))
                        } else {
                            Some((None, None))
                        }
                    } else if let TokenKind::Id(_) = **this.peek() {
                        // If we see the dot, then this is definitely is a interface
                        if let TokenKind::Operator(Operator::Dot) = **this.peek_n(1) {
                            let intf = this.expect_id()?;
                            this.consume();
                            let modport = this.expect_id()?;
                            Some((Some(Box::new(intf)), Some(Box::new(modport))))
                        } else if let TokenKind::Id(_) = **this.peek_n(1) {
                            // This is of form "id id", and we already ruled out possibility that this
                            // is a data port. So it must be interface port
                            let intf = this.expect_id()?;
                            Some((Some(Box::new(intf)), None))
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
                            )?;
                        }
                        let decl = PortDecl::Interface(a, b, vec![this.parse_decl_assign()?]);
                        if let Some(v) = mem::replace(&mut prev, Some(decl)) {
                            vec.push(v);
                        }
                        return Ok(true);
                    }

                    if dir.is_none() {
                        // If nothing is declared for first port, it is non-ANSI
                        if prev.is_none() {
                            ansi = false;
                            return Ok(false);
                        }
                    }
                }

                let assign = this.parse_decl_assign()?;

                // Nothing specified, inherit everything
                if dir.is_none() && net.is_none() && dtype.is_none() {
                    match prev.as_mut().unwrap() {
                        PortDecl::Data(_, _, _, ref mut l) |
                        PortDecl::Interface(_, _, ref mut l) => {
                            l.push(assign);
                            return Ok(true);
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

                return Ok(true)
            }, true, false)?;

            if !ansi {
                let span = this.peek().span.clone();
                this.report_span(Severity::Fatal, "non-ANSI port declaration is not yet supported", span)?;
                unreachable!();
            }

            if let Some(v) = prev {
                vec.push(v);
            }
            Ok(vec)
        })
    }

    /// Parse a port direction
    /// ```bnf
    /// port_direction ::=
    ///   input | output | inout | ref
    /// ```
    fn parse_port_dir(&mut self) -> Option<PortDir> {
        match self.peek().value {
            TokenKind::Keyword(Keyword::Input) => {
                self.consume();
                Some(PortDir::Input)
            }
            TokenKind::Keyword(Keyword::Output) => {
                self.consume();
                Some(PortDir::Output)
            }
            TokenKind::Keyword(Keyword::Inout) => {
                self.consume();
                Some(PortDir::Inout)
            }
            TokenKind::Keyword(Keyword::Ref) => {
                self.consume();
                Some(PortDir::Ref)
            }
            _ => None,
        }
    }

    //
    // A.2.1.3 Type declarations
    //

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

    // If this keyword can begin a data_type definition
    fn is_keyword_typename(kw: Keyword) -> bool {
        match kw {
            Keyword::Bit |
            Keyword::Logic |
            Keyword::Reg |
            Keyword::Byte |
            Keyword::Shortint |
            Keyword::Int |
            Keyword::Longint |
            Keyword::Integer |
            Keyword::Time |
            Keyword::Shortreal |
            Keyword::Real |
            Keyword::Realtime |
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

    /// Parse a data_type (or data_type_and_implicit). Note that for implicit, if there's no
    /// dimension & signing `None` will be returned. Note that we also treat `type` as data-type
    /// for simplicity.
    ///
    /// ```bnf
    /// data_type ::=
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
    /// | [ class_scope | package_scope ] type_identifier { packed_dimension }
    /// | class_type
    /// | event
    /// | ps_covergroup_identifier
    /// | type_reference
    /// ```
    fn parse_data_type(&mut self, implicit: bool) -> Result<Option<DataType>> {
        let toksp = self.consume();
        match toksp.value {
            TokenKind::Keyword(kw) => match kw {
                Keyword::Bit | Keyword::Logic | Keyword::Reg => {
                    let sign = self.parse_signing();
                    let dim = self.parse_list(Self::parse_pack_dim)?;
                    Ok(Some(Spanned::new(DataTypeKind::IntVec(kw, sign, dim), toksp.span.clone())))
                }
                Keyword::Signed | Keyword::Unsigned => {
                    let sp = toksp.span.clone();
                    self.pushback(toksp);
                    if implicit {
                        let sign = self.parse_signing();
                        let dim = self.parse_list(Self::parse_pack_dim)?;
                        Ok(Some(Spanned::new(DataTypeKind::Implicit(sign, dim), sp)))
                    } else {
                        Ok(None)
                    }
                }
                Keyword::Type => {
                    let token = self.consume();
                    // TODO: Might be parenthesis
                    Ok(Some(Spanned::new(DataTypeKind::Type, token.span)))
                }
                _ => {
                    self.pushback(toksp);
                    Ok(None)
                }
            }
            TokenKind::DelimGroup(Delim::Bracket, _) => {
                let sp = toksp.span.clone();
                self.pushback(toksp);
                if implicit {
                    let dim = self.parse_list(Self::parse_pack_dim)?;
                    Ok(Some(Spanned::new(DataTypeKind::Implicit(Signing::Unsigned, dim), sp)))
                } else {
                    Ok(None)
                }
            }
            _ => {
                self.pushback(toksp);
                Ok(None)
            }
        }
    }

    /// Parse a signing, defaulted to unsigned
    /// ```bnf
    /// signing ::= signed | unsigned
    /// ```
    fn parse_signing(&mut self) -> Signing {
        match self.peek().value {
            TokenKind::Keyword(Keyword::Signed) => {
                self.consume();
                Signing::Signed
            }
            TokenKind::Keyword(Keyword::Unsigned) => {
                self.consume();
                Signing::Unsigned
            }
            _ => Signing::Unsigned,
        }
    }


    //
    // A.2.4 Declaration assignments
    //

    fn parse_decl_assign(&mut self) -> Result<DeclAssign> {
        let mut ident = self.expect_id()?;
        let mut dim = self.parse_list(Self::parse_dim)?;

        // If we see another ID here, it means that the ID we seen previously are probably a
        // type name that isn't declared. Raise a sensible warning here.
        if let Some(id) = self.consume_if_id() {
            self.report_span(
                Severity::Error,
                "this looks like a data type but it is not declared",
                ident.span
            )?;
            ident = id;
            dim = self.parse_list(Self::parse_dim)?;
        }

        self.check_list(Self::check_unpacked_dim, &mut dim)?;
        let init = match self.consume_if(TokenKind::Operator(Operator::Assign)) {
            None => None,
            Some(_) => Some(Box::new(self.parse_unwrap(Self::parse_expr_opt)?)),
        };
        Ok(DeclAssign {
            name: ident,
            dim,
            init
        })
    }

    //
    // A.2.5 Declaration ranges
    //

    /// Parse a dimension. This is called variable_dimension in the spec. We've noted that it is
    /// a superset of all dimensions so we can simply call this function from other dimension
    /// parsing function.
    ///
    /// ```bnf
    /// unpacked_dimension ::=  [ constant_range ] | [ constant_expression ]
    /// packed_dimension ::= [ constant_range ] | unsized_dimension
    /// associative_dimension ::= [ data_type ] | [ * ]
    /// variable_dimension ::=
    ///   unsized_dimension | unpacked_dimension | associative_dimension | queue_dimension
    /// queue_dimension ::= [ $ [ : constant_expression ] ]
    /// unsized_dimension ::= [ ]
    /// ```
    fn parse_dim(&mut self) -> Result<Option<Dim>> {
        self.parse_if_delim_spanned(Delim::Bracket, |this| {
            scope!(this);
            Ok(match this.peek().value {
                TokenKind::Eof => {
                    DimKind::Unsized
                }
                TokenKind::Operator(Operator::Dollar) => {
                    this.consume();
                    let limit = match this.consume_if(TokenKind::Operator(Operator::Colon)) {
                        None => None,
                        Some(_) => {
                            Some(Box::new(parse!(expr)))
                        }
                    };
                    DimKind::Queue(limit)
                }
                TokenKind::Operator(Operator::Mul) => {
                    this.consume();
                    DimKind::AssocWild
                }
                _ => {
                    let expr = this.parse_unwrap(Self::parse_expr_opt)?;
                    if this.consume_if(TokenKind::Operator(Operator::Colon)).is_some() {
                        let end = Box::new(parse!(expr));
                        DimKind::Range(Box::new(expr), end)
                    } else {
                        DimKind::Value(Box::new(expr))
                    }
                }
            })
        })
    }

    /// Check if a dimension is a legal unpacked dimension
    fn check_unpacked_dim(&mut self, dim: &Dim) -> Result<bool> {
        match **dim {
            DimKind::AssocWild |
            DimKind::Queue(_) |
            DimKind::Unsized => {
                self.report_span(
                    Severity::Error,
                    "this type of range is not allowed in unpacked dimension context",
                    dim.span
                )?;
                Ok(false)
            }
            _ => Ok(true)
        }
    }

    /// Parse a packed dimension
    fn parse_pack_dim(&mut self) -> Result<Option<Dim>> {
        let ret = match self.parse_dim()? {
            None => return Ok(None),
            Some(v) => v,
        };
        match *ret {
            DimKind::AssocWild |
            DimKind::Queue(_) |
            DimKind::Value(_) => {
                self.report_span(
                    Severity::Error,
                    "this type of range is not allowed in packed dimension context",
                    ret.span
                )?;
                Ok(None)
            }
            _ => Ok(Some(ret))
        }
    }
    
    //
    // A.4.1.1 Module instantiation
    //

    /// Parse an instantiation. This can be any type of hierachical instantiaton and we cannot
    /// judge from only syntax. Returns `None` if this is actually not an instantiation.
    ///
    /// ```bnf
    /// instantiation ::=
    ///   identifier [ parameter_value_assignment ] hierarchical_instance
    ///   { , hierarchical_instance } ;
    /// ```
    fn parse_instantiation(&mut self, attr: &mut Option<Box<AttrInst>>) -> Result<Option<Item>> {
        let mut peek = 1;
        // Skip over parameter assignment if any
        if let TokenKind::Operator(Operator::Hash) = **self.peek_n(1) {
            if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(2) {
                peek = 3;
            } else {
                // Not an instantiation
                return Ok(None)
            }
        }
        // The next one must be an identifier
        if let TokenKind::Id(_) = **self.peek_n(peek) {
            peek += 1
        } else {
            return Ok(None)
        }
        // We must skip over dimension list if any
        while let TokenKind::DelimGroup(Delim::Bracket, _) = **self.peek_n(peek) {
            peek += 1
        }
        // Now we expect a opening paranthesis
        if let TokenKind::DelimGroup(Delim::Paren, _) = **self.peek_n(peek) {
            // Bingo!
        } else {
            return Ok(None)
        }
        
        let attr = mem::replace(attr, None);
        let mod_name = self.expect_id()?;

        // Parse parameter value assignment
        let param = if self.check(TokenKind::Operator(Operator::Hash)) {
            Some(self.parse_args(ArgOption::Param)?)
        } else {
            None
        };

        let list = self.parse_comma_list(false, false, |this| {
            let name = match this.consume_if_id() {
                None => return Ok(None),
                Some(v) => v,
            };

            let mut dim = this.parse_list(Self::parse_dim)?;
            this.check_list(Self::check_unpacked_dim, &mut dim)?;
            let ports = this.parse_args(ArgOption::Port)?;
            Ok(Some(HierInst {
                name,
                dim,
                ports,
            }))
        })?;

        self.expect(TokenKind::Operator(Operator::Semicolon))?;

        Ok(Some(Item::HierInstantiation(Box::new(
            HierInstantiation {
                attr,
                param,
                name: mod_name,
                inst: list,
            }
        ))))
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
    fn parse_args_opt(&mut self, option: ArgOption) -> Result<Option<Vec<Arg>>> {
        let mut named_seen = false;
        let mut ordered_seen = false;
        self.parse_if_delim(Delim::Paren, |this| {
            this.parse_comma_list(true, false, |this| {
                let attr = this.parse_attr_inst_opt()?;
                if option != ArgOption::Port && attr.is_some() {
                    this.report_span(
                        Severity::Error,
                        "attribute instances on argument is not allowed",
                        attr.as_ref().unwrap().span
                    )?;
                }
                if let Some(v) = this.consume_if(TokenKind::Operator(Operator::WildPattern)) {
                    if option != ArgOption::Port {
                        this.report_span(
                            Severity::Error,
                            ".* not allowed as argument",
                            v.span
                        )?;
                    }
                    named_seen = true;
                    Ok(Some(Arg::NamedWildcard(attr)))
                } else if let Some(v) = this.consume_if(TokenKind::Operator(Operator::Dot)) {
                    let name = this.expect_id()?;
                    let expr = this.parse_delim_spanned(Delim::Paren, Self::parse_expr_opt)?;
                    if ordered_seen && option != ArgOption::Arg {
                        this.report_span(
                            Severity::Error,
                            "mixture of ordered and named argument is not allowed",
                            v.span.merge(expr.span)
                        )?;
                    }
                    named_seen = true;
                    Ok(Some(Arg::Named(attr, Box::new(name), expr.value.map(Box::new))))
                } else {
                    let expr = this.parse_expr_opt()?.map(Box::new);
                    if named_seen {
                        if let Some(expr) = &expr {
                            this.report_span(
                                Severity::Error,
                                "ordered argument cannot appear after named argument",
                                expr.span
                            )?;
                        } else {
                            // Return None so error message will be about trailing comma.
                            return Ok(None)
                        }
                    }
                    ordered_seen = true;
                    Ok(Some(Arg::Ordered(attr, expr)))
                }
            })
        })
    }

    fn parse_args(&mut self, option: ArgOption) -> Result<Vec<Arg>> {
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
    fn parse_loop_gen(&mut self, attr: Option<Box<AttrInst>>) -> Result<Item> {
        // Eat the for keyword
        self.consume();
        let (genvar, id, init, cond, update) = 
            self.parse_delim(Delim::Paren, |this| {
                let genvar = this.check(TokenKind::Keyword(Keyword::Genvar));
                let id = this.expect_id()?;
                this.expect(TokenKind::Operator(Operator::Assign))?;
                let init = this.parse_expr()?;
                this.expect(TokenKind::Operator(Operator::Semicolon))?;
                let cond = this.parse_expr()?;
                this.expect(TokenKind::Operator(Operator::Semicolon))?;
                let update = this.parse_expr()?;
                Ok((genvar, id, init, cond, update))
            })?;
        let block = self.parse_gen_block()?;
        Ok(Item::LoopGen(Box::new(LoopGen {
            attr,
            genvar,
            id,
            init,
            cond,
            update,
            block,
        })))
    }

    /// Parse a if_generate_construct
    /// ```bnf
    /// if_generate_construct ::=
    ///   if ( constant_expression ) generate_block [ else generate_block ]
    /// ```
    fn parse_if_gen(&mut self, attr: Option<Box<AttrInst>>) -> Result<Item> {
        // Eat the if keyword
        self.consume();
        let cond = self.parse_delim(Delim::Paren, Self::parse_expr)?;
        let true_block = self.parse_gen_block()?;
        let false_block = if self.check(TokenKind::Keyword(Keyword::Else)) {
            Some(Box::new(self.parse_gen_block()?))
        } else {
            None
        };

        Ok(Item::IfGen(Box::new(IfGen {
            attr,
            cond,
            true_block,
            false_block,
        })))
    }

    fn parse_gen_block(&mut self) -> Result<Item> {
        // A generate-block may begin with a label. It is treated as same as label after begin.
        let label = if let TokenKind::Id(_) = **self.peek() {
            if let TokenKind::Operator(Operator::Colon) = **self.peek_n(1) {
                // This is actuall
                if let TokenKind::Keyword(Keyword::Begin) = **self.peek_n(2) {
                    let label = self.expect_id()?;
                    self.consume();
                    Some(label)
                } else { None }
            } else { None }
        } else { None };
        
        let begin = match self.consume_if(TokenKind::Keyword(Keyword::Begin)) {
            None => return self.parse_item(),
            Some(v) => v,
        };

        let name = if self.check(TokenKind::Operator(Operator::Colon)) {
            Some(self.expect_id()?)
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
                )?;
            } else {
                self.report_span(
                    Severity::Warning,
                    "duplicate block identifiers before and after 'begin'",
                    n.span
                )?;
            }
        } else if let (Some(l), None) = (&label, &name) {
            self.report_diag(
                Diagnostic::new(
                    Severity::Warning,
                    "it is suggested to place block identifier after 'begin'",
                    l.span.merge(begin.span)
                ).fix_primary(format!("begin: {}", l))
            )?;
        }

        let name = name.or(label);
        let items = self.parse_list(Self::parse_item_opt)?;
        
        self.expect(TokenKind::Keyword(Keyword::End))?;

        if self.check(TokenKind::Operator(Operator::Colon)) {
            let id = self.expect_id()?;
            match &name {
                None => self.report_span(
                    Severity::Error,
                    "identifer annotation at end does match declaration, should be empty",
                    id.span
                )?,
                Some(v) => if **v != *id {
                    self.report_span(
                        Severity::Error,
                        format!("identifer annotation at end does match declaration, should be '{}'", v),
                        id.span
                    )?
                }
            }
        }

        Ok(Item::GenBlock(Box::new(GenBlock {
            name: name.map(Box::new),
            items,
        })))
    }

    //
    // A.6.1 Continuous assignment and net alias statements
    //
    fn parse_continuous_assign(&mut self) -> Result<Item> {
        self.consume();
        // IMP: Parse drive_strength
        // IMP: Parse delay control
        let assignments = self.parse_comma_list(false, false, Self::parse_assign_expr)?;
        self.expect(TokenKind::Operator(Operator::Semicolon))?;
        Ok(Item::ContinuousAssign(assignments))
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

    //
    // A.8.2 Subroutine calls
    //

    fn parse_sys_tf_call(&mut self) -> Result<SysTfCall> {
        let task = {
            let token = self.consume();
            match token.value {
                TokenKind::SystemTask(name) => Spanned::new(name, token.span),
                _ => unreachable!(),
            }
        };
        let args = self.parse_args_opt(ArgOption::Arg)?;
        Ok(SysTfCall {
            task,
            args,
        })
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
    fn parse_assign_expr(&mut self) -> Result<Option<Expr>> {
        scope!(self);

        let expr = match parse!([expr]) {
            None => return Ok(None),
            Some(v) => v,
        };

        match **self.peek() {
            TokenKind::Operator(op) if Self::is_assign_op(op) => {
                self.consume();
                let rhs = parse!(expr);
                let span = expr.span.merge(rhs.span);
                Ok(Some(Spanned::new(ExprKind::Assign(Box::new(expr), op, Box::new(rhs)), span)))
            }
            _ => Ok(Some(expr))
        }
    }

    /// Parse an expression (or data_type)
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
    ///   binary_expression
    /// | conditional_expression
    /// | inside_expression
    /// | tagged_union_expression
    /// binary_expression ::=
    ///   unary_expression
    /// | binary_expression binary_operator { attribute_instance } expression
    /// unary_expression ::=
    ///   primary
    /// | unary_operator { attribute_instance } primary
    /// | inc_or_dec_expression
    /// ```
    /// TODO: conditional & inside are not yet completed
    fn parse_expr_opt(&mut self) -> Result<Option<Expr>> {
        match **self.peek() {
            // tagged_union_expression
            TokenKind::Keyword(Keyword::Tagged) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "tagged_union_expression not yet supported", span)?;
                unreachable!();
            }
            _ => {
                self.parse_bin_expr(0)
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_unwrap(Self::parse_expr_opt)
    }

    /// Parse binary expression using precedence climing method which saves stack space.
    /// TODO: Handle <= properly
    fn parse_bin_expr(&mut self, prec: i32) -> Result<Option<Expr>> {
        let mut expr = match self.parse_unary_expr()? {
            None => return Ok(None),
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
                _ => break,
            };

            self.consume();

            if self.consume_if_delim(Delim::Attr).is_some() {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "attributes not yet supported", span)?;
            }

            let rhs = self.parse_unwrap(|this| this.parse_bin_expr(new_prec))?;
            let span = expr.span.merge(rhs.span);
            expr = Spanned::new(ExprKind::Binary(Box::new(expr), op, Box::new(rhs)), span);
        }

        Ok(Some(expr))
    }

    fn parse_unary_expr(&mut self) -> Result<Option<Expr>> {
        match **self.peek() {
            // inc_or_dec_operator { attribute_instance } variable_lvalue
            // unary_operator { attribute_instance } primary
            TokenKind::Operator(op) if Self::is_prefix_operator(op) => {
                let span = self.consume().span;
                if self.consume_if_delim(Delim::Attr).is_some() {
                    let span = self.peek().span;
                    self.report_span(Severity::Fatal, "attributes not yet supported", span)?;
                }
                let expr = self.parse_unwrap(Self::parse_primary)?;
                let span = span.merge(expr.span);
                Ok(Some(Spanned::new(ExprKind::Unary(op, Box::new(expr)), span)))
            }
            _ => {
                self.parse_primary()
            }
        }
    }


    /// Parse mintypmax expression
    ///
    /// ```bnf
    /// mintypmax_expression ::=
    ///   expression | expression : expression : expression
    /// ```
    fn parse_mintypmax_expr(&mut self) -> Result<Option<Expr>> {
        let expr = match self.parse_expr_opt()? {
            None => return Ok(None),
            Some(v) => v,
        };

        if !self.check(TokenKind::Operator(Operator::Colon)) {
            return Ok(Some(expr))
        }

        scope!(self);
        let typ = parse!(box(expr));
        self.expect(TokenKind::Operator(Operator::Colon))?;
        let max = parse!(box(expr));
        let span = expr.span.merge(max.span);
        Ok(Some(Spanned::new(ExprKind::MinTypMax(Box::new(expr), typ, max), span)))
    }

    /// Combined parser of bit_select (single) and part_select_range.
    ///
    /// According to spec
    /// ```bnf
    /// bit_select ::=
    ///   { [ expression ] } <- we parse [ expression ] here instead.
    /// part_select_range ::=
    ///   constant_range | indexed_range
    /// indexed_range ::=
    ///   expression +: constant_expression | expression -: constant_expression
    /// ```
    fn parse_single_select(&mut self) -> Result<Select> {
        self.parse_delim(Delim::Bracket, |this| {
            scope!(this);
            let expr = parse!(expr);
            match **this.peek() {
                TokenKind::Operator(Operator::Colon) => {
                    this.consume();
                    Ok(Select::Range(Box::new(expr), Box::new(parse!(expr))))
                }
                TokenKind::Operator(Operator::PlusColon) => {
                    this.consume();
                    Ok(Select::PlusRange(Box::new(expr), Box::new(parse!(expr))))
                }
                TokenKind::Operator(Operator::MinusColon) => {
                    this.consume();
                    Ok(Select::MinusRange(Box::new(expr), Box::new(parse!(expr))))
                }
                _ => Ok(Select::Value(Box::new(expr))),
            }
        })
    }

    //
    // A.8.4 Primaries (or data_type)
    //

    /// Parse primary expression with cast.
    fn parse_primary(&mut self) -> Result<Option<Expr>> {
        let mut expr = match **self.peek() {
            TokenKind::Keyword(Keyword::Const) => {
                let span = self.consume().span;
                self.expect(TokenKind::Operator(Operator::Tick))?;
                let expr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt))?;
                Spanned::new(ExprKind::ConstCast(Box::new(expr.value)), span.merge(expr.span))
            }
            TokenKind::Keyword(Keyword::Signed) | 
            TokenKind::Keyword(Keyword::Unsigned) => {
                let span = self.peek().span;
                let sign = self.parse_signing();
                self.expect(TokenKind::Operator(Operator::Tick))?;
                let expr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt))?;
                Spanned::new(ExprKind::SignCast(sign, Box::new(expr.value)), span.merge(expr.span))
            }
            _ => match self.parse_primary_nocast()? {
                None => return Ok(None),
                Some(v) => v,
            }
        };
        loop {
            if self.consume_if(TokenKind::Operator(Operator::Tick)).is_none() {
                break
            }

            let nexpr = self.parse_delim_spanned(Delim::Paren, |this| this.parse_unwrap(Self::parse_expr_opt))?;
            let span = expr.span.merge(nexpr.span);
            expr = Spanned::new(ExprKind::TypeCast(Box::new(expr), Box::new(nexpr.value)), span)
        }
        Ok(Some(expr))
    }

    /// Parse primary expression, except for cast. Cast is special as it can take form
    /// `primary '(expr)` which introduces left recursion.
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
    /// ```
    fn parse_primary_nocast(&mut self) -> Result<Option<Expr>> {
        match **self.peek() {
            // Case where this isn't an expression
            TokenKind::Eof => Ok(None),
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
                Ok(Some(Spanned::new(ExprKind::Literal(tok), sp)))
            }
            // empty_queue
            // concatenation [ [ range_expression ] ]
            // multiple_concatenation [ [ range_expression ] ]
            // streaming_concatenation
            TokenKind::DelimGroup(Delim::Brace, _) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "concat is not finished yet", span)?;
                unreachable!();
            }
            // assignment_pattern_expression
            TokenKind::DelimGroup(Delim::TickBrace, _) => {
                let span = self.peek().span;
                self.report_span(Severity::Fatal, "assign pattern is not finished yet", span)?;
                unreachable!();
            }
            // ( mintypmax_expression )
            TokenKind::DelimGroup(Delim::Paren, _) => {
                Ok(Some(self.parse_delim_spanned(Delim::Paren, |this| {
                    scope!(this);
                    Ok(ExprKind::Paren(parse!(box(parse_mintypmax_expr))))
                })?))
            }
            // system_tf_call
            TokenKind::SystemTask(_) => {
                let tf = self.parse_sys_tf_call()?;
                // IMP: better span
                let span = tf.task.span.start.span_to(self.peek().span.start);
                Ok(Some(Spanned::new(ExprKind::SysTfCall(Box::new(tf)), span)))
            }
            // The left-over possibilities are:
            // [ class_qualifier | package_scope ] hierarchical_identifier select
            // function_subroutine_call
            // let_expression
            // cast
            // sequence_method_call
            // this
            // We cannot really distinguish between them directly. But we noted they all begin
            // with a hierachical name (or keyword typename). So we parse it first, and then try
            // to parse the rest as postfix operation.
            // Keyword type names, parse as data type
            TokenKind::Keyword(kw) if Self::is_keyword_typename(kw) => {
                let ty = Box::new(self.parse_data_type(false)?.unwrap());
                let span = ty.span;
                Ok(Some(Spanned::new(ExprKind::Type(ty), span)))
            }
            // Otherwise, parse as name
            _ => {
                let begin_span = self.peek().span;
                let scope = self.parse_scope()?;
                let mut id = self.parse_hier_id()?;

                // Not a primary expressison
                if scope.is_none() && id.is_none() {
                    Ok(None)
                } else {
                    // If we've seen the scopes then we must need to see the id
                    if scope.is_some() && id.is_none() {
                        let span = self.peek().span;
                        self.report_span(Severity::Error, "expected identifiers after scope", span)?;
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
                            self.report_span(Severity::Fatal, "assign pattern is not finished yet", span)?;
                            unreachable!();
                        }
                        // This can be either function call or inc/dec expression
                        TokenKind::DelimGroup(Delim::Attr, _) => {
                            let span = self.peek().span;
                            self.report_span(Severity::Fatal, "inc/dec or function call not finished yet", span)?;
                            unreachable!();
                        }
                        // Function call
                        TokenKind::DelimGroup(Delim::Paren, _) => {
                            let span = self.peek().span;
                            self.report_span(Severity::Fatal, "function call not finished yet", span)?;
                            unreachable!();
                        }
                        // Inc/Dec
                        TokenKind::Operator(e @ Operator::Inc) |
                        TokenKind::Operator(e @ Operator::Dec) => {
                            let span = span.merge(self.consume().span);
                            Ok(Some(Spanned::new(ExprKind::PostfixIncDec(Box::new(expr), e), span)))
                        }
                        // Bit select
                        TokenKind::DelimGroup(Delim::Bracket, _) => Ok(Some(self.parse_select(expr)?)),
                        _ => Ok(Some(expr))
                    }
                }
            }
        }
    }

    /// Parse select expression
    /// select ::=
    ///   [ { . member_identifier bit_select } . member_identifier ] bit_select
    /// | [ [ part_select_range ] ]
    fn parse_select(&mut self, mut expr: Expr) -> Result<Expr> {
        loop {
            match **self.peek() {
                // Bit select
                TokenKind::DelimGroup(Delim::Bracket, _) => {
                    let sel = self.parse_single_select()?;
                    // TODO better end span
                    let span = expr.span.end.span_to(self.peek().span.start);
                    expr = Spanned::new(ExprKind::Select(Box::new(expr), sel), span);
                }
                TokenKind::Operator(Operator::Dot) => {
                    self.consume();
                    let id = self.expect_id()?;
                    let span = expr.span.merge(id.span);
                    expr = Spanned::new(ExprKind::Member(Box::new(expr), id), span);
                }
                _ => return Ok(expr)
            }
        }
    }

    //
    // A.8.5 Expression left-side values
    //
    fn parse_lvalue(&mut self) -> Result<()> {
        // TODO
        self.expect_id()?;
        Ok(())
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
    fn parse_attr_inst_opt(&mut self) -> Result<Option<Box<AttrInst>>> {
        let attr = self.parse_if_delim_spanned(Delim::Attr, |this| {
            Ok(AttrInstStruct(
                this.parse_comma_list(false, false, |this| {
                    scope!(this);
                    match this.consume_if_id() {
                        None => Ok(None),
                        Some(name) => {
                            let expr = if this.check(TokenKind::Operator(Operator::Assign)) {
                                Some(parse!(box(expr)))
                            } else {
                                None
                            };
                            Ok(Some(AttrSpec {
                                name,
                                expr
                            }))
                        }
                    }
                })?
            ))
        })?;
        Ok(attr.map(Box::new))
    }

    //
    // A.9.3 Identifiers
    //

    /// Parse scope. This is more generous than any scoped names in SystemVerilog spec.
    /// ```bnf
    /// [ local :: | $unit :: ] [ identifier [ parameter_value_assignment ] :: ]
    /// ``` 
    fn parse_scope(&mut self) -> Result<Option<Scope>> {
        let mut scope = None;
        loop {
            match **self.peek() {
                TokenKind::Keyword(Keyword::Local) => {
                    let tok = self.consume();
                    if let Some(_) = scope {
                        self.report_span(Severity::Error, "local scope can only be the outermost scope", tok.span)?;
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep))?;
                }
                TokenKind::Keyword(Keyword::Unit) => {
                    let tok = self.consume();
                    if let Some(_) = scope {
                        self.report_span(Severity::Error, "$unit scope can only be the outermost scope", tok.span)?;
                    } else {
                        scope = Some(Scope::Local)
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep))?;
                }
                TokenKind::Id(_) => {
                    // Lookahead to check if this is actually a scope
                    match **self.peek_n(1) {
                        TokenKind::Operator(Operator::ScopeSep) => (),
                        TokenKind::Operator(Operator::Hash) => {
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
                    let ident = self.expect_id()?;
                    if self.consume_if(TokenKind::Operator(Operator::Hash)).is_some() {
                        // TODO: Add parameter support
                        self.report_span(Severity::Fatal, "class parameter scope is not yet supported", ident.span)?;
                        unreachable!();
                    }
                    self.expect(TokenKind::Operator(Operator::ScopeSep))?;
                    scope = Some(Scope::Name(scope.map(Box::new), Box::new(ident)))
                }
                _ => break,
            }
        }
        Ok(scope)
    }

    /// Parse hierachical identifier
    fn parse_hier_id(&mut self) -> Result<Option<HierId>> {
        let mut id = None;
        self.parse_sep_list_unit(Operator::Dot, true, false, |this| {
            match **this.peek() {
                TokenKind::Keyword(Keyword::This) => {
                    let tok = this.consume();
                    if let Some(_) = id {
                        this.report_span(Severity::Error, "this can only be the outermost identifier", tok.span)?;
                    } else {
                        id = Some(HierId::This)
                    }
                }
                TokenKind::Keyword(Keyword::Super) => {
                    let tok = this.consume();
                    match id {
                        None | Some(HierId::This) => id = Some(HierId::Super),
                        Some(_) => {
                            this.report_span(Severity::Error, "super can only be the outermost identifier", tok.span)?;
                        }
                    }
                }
                TokenKind::Keyword(Keyword::Root) => {
                    let tok = this.consume();
                    if let Some(_) = id {
                        this.report_span(Severity::Error, "$root can only be the outermost identifier", tok.span)?;
                    } else {
                        id = Some(HierId::Root)
                    }
                }
                TokenKind::Id(_) => {
                    id = Some(HierId::Name(
                        // Hack to move id out temporarily
                        mem::replace(&mut id, None).map(Box::new),
                        Box::new(this.expect_id()?)
                    ))
                }
                _ => return Ok(false)
            }
            Ok(true)
        })?;
        Ok(id)
    }
}