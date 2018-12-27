use num::{BigUint, BigInt, bigint::Sign, One, Zero, ToPrimitive};
use std::fmt;
use std::ops::*;
use std::cmp;

/// Represntation of number with fixed width.
#[derive(Clone, PartialEq, Eq)]
pub struct Int {
    width: usize,
    value: BigUint,
}

impl Int {

    /// Create a Int from BigUint
    pub fn new(width: usize, value: BigUint) -> Int {
        let mut ret = Int {
            width,
            value,
        };
        ret.trunc_to_fit();
        ret
    }

    pub fn from_bigint(width: usize, value: BigInt) -> Int {
        let value = if let Sign::Minus = value.sign() {
            let mut abs = (-value).to_biguint().unwrap();
            let mut val = BigUint::one();
            val <<= width;
            val -= 1 as u8;
            // Invert all bits
            abs ^= val;
            abs + 1 as u8
        } else {
            value.to_biguint().unwrap()
        };
        Int {width, value}
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

    /// Get the sign bit
    pub fn sign_bit(&self) -> bool {
        self.bit_at(self.width - 1)
    }

    pub fn width(&self) -> usize {
        self.width
    }

    pub fn is_zero(&self) -> bool {
        self.value.is_zero()
    }

    pub fn is_one(&self) -> bool {
        self.value.is_one()
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

    /// Truncate to current width
    fn trunc_to_fit(&mut self) {
        if self.width < self.value.bits() {
            let mut val = BigUint::one();
            val <<= self.width;
            val -= 1 as u8;
            self.value &= &val;
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

impl<'a> AddAssign<&'a Int> for Int {
    fn add_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value += &rhs.value;
        self.trunc_to_fit();
    }
}

impl<'a> SubAssign<&'a Int> for Int {
    fn sub_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);

        // Extend first to avoid small - big
        let mut mask = BigUint::one();
        mask <<= self.width;
        self.value |= mask;

        self.value -= &rhs.value;
        self.trunc_to_fit();
    }
}

impl<'a> MulAssign<&'a Int> for Int {
    fn mul_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value *= &rhs.value;
        self.trunc_to_fit();
    }
}

/// Unsigned division
impl<'a> DivAssign<&'a Int> for Int {
    fn div_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value /= &rhs.value;
    }
}

/// Unsigned remainder
impl<'a> RemAssign<&'a Int> for Int {
    fn rem_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value %= &rhs.value;
    }
}

impl Int {
    pub fn pow_assign(&mut self, rhs: &Self) {
        let pow = rhs.value.to_usize().unwrap();
        let val = std::mem::replace(&mut self.value, BigUint::zero());
        self.value = num::pow(val, pow);
        self.trunc_to_fit();
    }
}

impl Int {
    /// Do a signed division. Panic if RHS is zero.
    pub fn signed_div(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        let value = self.clone().to_bigint() / rhs.clone().to_bigint();
        *self = Self::from_bigint(self.width, value);
    }

    /// Do a signed remainder. Panic if RHS is zero.
    pub fn signed_rem(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        let value = self.clone().to_bigint() % rhs.clone().to_bigint();
        *self = Self::from_bigint(self.width, value);
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

impl<'a> BitOrAssign<&'a Int> for Int {
    fn bitor_assign(&mut self, rhs: &Self) {
        assert!(self.width == rhs.width);
        self.value |= &rhs.value;
    }
}

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

impl<'a> ShlAssign<&'a Int> for Int {
    fn shl_assign(&mut self, rhs: &Self) {
        // Prepare RHS
        let rhs = rhs.value.to_usize().unwrap_or(std::usize::MAX);
        // Guard against corner case (e.g. super large RHS)
        if rhs > self.width {
            self.value = BigUint::zero();
            return;
        }
        self.value <<= rhs;
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

impl Int {
    pub fn reduce_or(&self) -> bool {
        !self.value.is_zero()
    }
}

//
// Comparisions
//

impl cmp::Ord for Int {
    fn cmp(&self, rhs: &Int) -> cmp::Ordering {
        assert!(self.width == rhs.width);
        cmp::Ord::cmp(&self.value, &rhs.value)
    }
}

impl cmp::PartialOrd for Int {
    fn partial_cmp(&self, rhs: &Int) -> Option<cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Int {
    pub fn signed_cmp(&self, rhs: &Int) -> cmp::Ordering {
        let lsign = self.bit_at(self.width - 1);
        let rsign = self.bit_at(self.width - 1);
        match (lsign, rsign) {
            (false, false) => self.cmp(rhs),
            (false, true) => cmp::Ordering::Greater,
            (true, false) => cmp::Ordering::Less,
            (true, true) => rhs.cmp(self),
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
