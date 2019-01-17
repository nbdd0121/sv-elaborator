//! This module represent elaborated and grounded types. Everything here contains no symbolic
//! references to other types.

use std::cmp;
use std::rc::Rc;
use std::cell::RefCell;

use number::LogicVec;
use syntax::ast::{Ident, RealTy};

/// A packed integral data type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntTy {
    /// A single bit. First bool represents whether the bit is two-state, and second bool
    /// represent whether the bit is signed.
    Logic(bool, bool),
    /// An packed array.
    Array(Box<IntTy>, i32, i32),
    /// An packed structure.
    Struct(Rc<Struct>),
    // TODO: Include unions here?
    /// An enumeration.
    Enum(Rc<Enum>),
    /// A simple vector. This is an optimsation over an packed array of a single bit due to the
    /// ubquitiy of simple vector types in SystemVerilog.
    SimpleVec(usize, bool, bool),
}

impl IntTy {

    /// Create a vector of given type.
    pub fn vec(self, upper: i32, lower: i32) -> Self {
        if lower == 0 && upper >= 0 {
            if let IntTy::Logic(two_state, sign) = self {
                return IntTy::SimpleVec((upper + 1) as usize, two_state, sign)
            }
        }
        IntTy::Array(Box::new(self), upper, lower)
    }

    pub fn width(&self) -> usize {
        match self {
            IntTy::Logic(..) => 1,
            IntTy::Array(element, upper, lower) => {
                let w = (cmp::max(upper, lower) - cmp::min(upper, lower) + 1) as usize;
                element.width() * w
            }
            IntTy::Struct(struc) => struc.width,
            IntTy::Enum(enu) => enu.base.width(),
            IntTy::SimpleVec(size, ..) => *size,
        }
    }

    pub fn two_state(&self) -> bool {
        match self {
            IntTy::Logic(two_state, _) => *two_state,
            IntTy::Array(element, ..) => element.two_state(),
            IntTy::Struct(struc) => struc.two_state,
            IntTy::Enum(enu) => enu.base.two_state(),
            IntTy::SimpleVec(_, two_state, _) => *two_state,
        }
    }

    pub fn sign(&self) -> bool {
        match self {
            IntTy::Logic(_, sign) => *sign,
            IntTy::Array(element, ..) => element.sign(),
            IntTy::Struct(struc) => struc.sign,
            IntTy::Enum(enu) => enu.base.sign(),
            IntTy::SimpleVec(.., sign) => *sign,
        }
    }
}

/// A packed structure.
#[derive(Debug)]
pub struct Struct {
    pub width: usize,
    pub two_state: bool,
    pub sign: bool,
    /// Members of this packed structure. Per spec this must only be an integral type.
    pub members: Vec<(IntTy, Ident, Option<Box<LogicVec>>)>,
}

impl Struct {
    pub fn new(sign: bool, members: Vec<(IntTy, Ident, Option<Box<LogicVec>>)>) -> Self {
        let mut width = 0;
        let mut two_state = true;
        for member in &members {
            width += member.0.width();
            two_state &= member.0.two_state();
        }
        Struct {
            width,
            two_state,
            sign,
            members,
        }
    }
}

/// Each Struct is distinct per spec, so only compare pointers.
impl cmp::PartialEq for Struct {
    fn eq(&self, rhs: &Self) -> bool {
        self as *const Self == rhs as *const Self
    }
}

impl cmp::Eq for Struct {}

impl std::hash::Hash for Struct {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize)
    }
}

/// An enumeration after elaboration
#[derive(Debug)]
pub struct Enum {
    pub base: IntTy,
    /// This has to be a RefCell because elements of enum can refer to another elements.
    pub elements: RefCell<Vec<(Ident, LogicVec)>>,
}

impl cmp::PartialEq for Enum {
    fn eq(&self, rhs: &Self) -> bool {
        self as *const Self == rhs as *const Self
    }
}

impl cmp::Eq for Enum {}

impl std::hash::Hash for Enum {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self as *const _ as usize)
    }
}

/// A concrete SystemVerilog data type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    Type,
    Int(IntTy),
    /// This represent a fixed size string, which can be cast into either integral type or string
    /// type
    FixStr(usize),
    Real(RealTy),
    String,
    Chandle,
    Event,
    Void,
    Array(Box<Ty>, i32, i32),
    // TODO: Also include unpacked struct, union, array here?
}
