use num::{BigUint, BigInt, bigint::Sign, One, Zero, ToPrimitive};
use std::fmt;
use std::ops::*;

/// Represntation of number with fixed width.
#[derive(Clone, PartialEq)]
pub struct Int {
    width: usize,
    value: BigUint,
}

impl Int {

    /// Create a Int from BigUint
    pub fn new(width: usize, mut value: BigUint) -> Int {
        if width < value.bits() {
            let mut val = BigUint::one();
            val <<= width;
            val -= 1 as u8;
            value &= val;
        }

        Int {
            width,
            value,
        }
    }

    /// Get a zero of a certain length
    pub fn zero(width: usize) -> Int {
        Int {
            width,
            value: BigUint::zero(),
        }
    }

    /// Get a value with all one.
    pub fn all_one(width: usize) -> Int {
        let mut value = BigUint::one();
        value <<= width;
        value -= 1 as u8;
        Int {
            width,
            value,
        }
    }

    /// Get a value with all one or all zero
    pub fn fill(width: usize, bit: bool) -> Int {
        if bit {
            Self::all_one(width)
        } else {
            Self::zero(width)
        }
    }

    /// Get a bit
    pub fn bit_at(&self, index: usize) -> bool {
        if index >= self.width {
            panic!("out of bound");
        }
        ((&self.value >> index).to_u32().unwrap() & 1) != 0
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    /// Convert this number to BigUInt, treat as unsigned
    pub fn to_biguint(self) -> BigUint {
        self.value
    }

    /// Convert this number to BigInt, treat as signed
    pub fn to_bigint(self) -> BigInt {
        if self.bit_at(self.width - 1) {
            BigInt::from_biguint(Sign::Minus, (-self).to_biguint())
        } else {
            BigInt::from_biguint(Sign::Plus, self.to_biguint())
        }
    }

    /// Truncate to a smaller Int
    fn truncate(&mut self, width: usize) {
        let mut val = BigUint::one();
        val <<= width;
        val -= 1 as u8;
        self.value &= &val;
        self.width = width;
    }

    /// Perform zero-extension or truncation
    pub fn zero_extend_or_trunc(&mut self, width: usize) {
        if width == self.width { return }
        if width < self.width { return self.truncate(width) }
        self.width = width;
    }

    /// Perform one-extension or truncation
    pub fn one_extend_or_trunc(&mut self, width: usize) {
        if width == self.width { return }
        if width < self.width { return self.truncate(width) }

        // Calculate the mask for extension
        let mut extend_mask = BigUint::one();
        extend_mask <<= width - self.width;
        extend_mask -= 1 as u8;
        extend_mask <<= self.width;

        self.value |= &extend_mask;
        self.width = width;
    }

    /// Perform sign extension or truncation
    pub fn sign_extend_or_trunc(&mut self, width: usize) {
        if self.bit_at(self.width - 1) {
            self.one_extend_or_trunc(width)
        } else {
            self.zero_extend_or_trunc(width)
        }
    }

    /// Duplicate self multiple times
    pub fn duplicate(&self, count: usize) -> Self {
        // Currently we assume count is small, and does not use O(logn) algorithm.
        let mut value = BigUint::zero();
        for _ in 0..count {
            value <<= self.width;
            value |= &self.value;
        }
        Self {
            width: self.width * count,
            value,
        }
    }
}

//
// Arithmetic of Int
//

macro_rules! impl_bin_traits {
    ($op: ident, $fn: ident, $ass: ident, $assfn: ident) => {
        impl <'a> $op<&'a Int> for Int {
            type Output = Int;

            fn $fn(mut self, rhs: &Int) -> Self {
                self.$assfn(rhs);
                self
            }
        }
    }
}

impl Neg for Int {
    type Output = Int;

    fn neg(self) -> Self {
        let mut ret = BigUint::one();
        ret <<= self.width;
        ret -= self.value;
        Self {
            width: self.width,
            value: ret,
        }
    }
}

//
// Bitwise operations
//

impl<'a> BitXorAssign<&'a Int> for Int {
    fn bitxor_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value ^= &rhs.value;
    }
}

impl_bin_traits!(BitXor, bitxor, BitXorAssign, bitxor_assign);

impl<'a> BitAndAssign<&'a Int> for Int {
    fn bitand_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value &= &rhs.value;
    }
}

impl_bin_traits!(BitAnd, bitand, BitAndAssign, bitand_assign);

impl Not for Int {
    type Output = Int;

    fn not(self) -> Self {
        // Create a all-one mask
        let mut mask = BigUint::one();
        mask <<= self.width;
        mask -= 1 as u8;
        let mask = Int { width: self.width, value: mask };
        // Xor with the mask will yield not
        self ^ &mask
    }
}

impl Int {
    pub fn zero_shr(&mut self, rhs: &Int) {
        // Prepare RHS
        let rhs = rhs.value.to_usize().unwrap_or(std::usize::MAX);
        self.value >>= rhs;
    }

    pub fn one_shr(&mut self, rhs: &Int) {
        // Prepare RHS
        let rhs = rhs.value.to_usize().unwrap_or(std::usize::MAX);
        // Calculate the mask
        let mut mask = BigUint::one();
        mask <<= rhs;
        mask -= 1 as u8;
        mask <<= self.width - rhs;
        self.value >>= rhs;
        self.value |= mask;
    }

    pub fn sign_shr(&mut self, rhs: &Int) {
        if self.bit_at(self.width - 1) {
            self.one_shr(rhs)
        } else {
            self.zero_shr(rhs)
        }
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}'", self.width)?;
        // We use a heuristics for display number. If the number is below 1024, display it as
        // decimal. Otherwise display as hex.
        if &self.value < &(1024 as u16).into() {
            write!(f, "d{}", self.value)
        } else {
            write!(f, "h{:X}", self.value)
        }
    }
}
