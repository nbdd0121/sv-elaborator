use num::{bigint::Sign, BigInt, BigUint, FromPrimitive, One, Zero};
use std::cmp;
use std::fmt;
use std::ops;

mod int;
pub use self::int::Int;

/// Represntation of Verilog's 4-state logic
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    Z,
    X,
}

impl ops::Not for LogicValue {
    type Output = LogicValue;
    fn not(self) -> LogicValue {
        match self {
            LogicValue::Zero => LogicValue::One,
            LogicValue::One => LogicValue::Zero,
            _ => LogicValue::X,
        }
    }
}

impl From<bool> for LogicValue {
    fn from(val: bool) -> Self {
        match val {
            false => LogicValue::Zero,
            true => LogicValue::One,
        }
    }
}

impl From<LogicValue> for Option<bool> {
    fn from(val: LogicValue) -> Option<bool> {
        match val {
            LogicValue::Zero => Some(false),
            LogicValue::One => Some(true),
            _ => None,
        }
    }
}

impl fmt::Display for LogicValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LogicValue::Zero => write!(f, "0"),
            LogicValue::One => write!(f, "1"),
            LogicValue::Z => write!(f, "z"),
            LogicValue::X => write!(f, "x"),
        }
    }
}

/// Represntation of Verilog's 4-state logic array
#[derive(Clone, PartialEq)]
pub struct LogicVec {
    pub signed: bool,
    value: Int,
    xz: Int,
}

impl LogicVec {
    /// Construct a LogicVec from Int's
    pub fn new(signed: bool, value: Int, xz: Int) -> LogicVec {
        assert!(value.width() == xz.width());
        LogicVec { signed, value, xz }
    }

    /// Construct a two-state LogicVec from Int
    pub fn from_int(signed: bool, value: Int) -> LogicVec {
        let xz = Int::zero(value.width());
        LogicVec { signed, value, xz }
    }

    /// Construct a 4-state logic array from 2-state logic.
    pub fn new_xz(width: usize, signed: bool, value: BigUint, xz: BigUint) -> LogicVec {
        LogicVec {
            signed,
            value: Int::new(width, value),
            xz: Int::new(width, xz),
        }
    }

    /// Construct a 4-state logic array from 2-state logic.
    pub fn from_biguint(width: usize, signed: bool, mut value: BigUint) -> LogicVec {
        if width < value.bits() as usize {
            let mut val = BigUint::one();
            val <<= width;
            val -= 1 as u8;
            value &= val;
        }

        LogicVec::new_xz(width, signed, value, BigUint::zero())
    }

    /// Convert from BigInt
    pub fn from(width: usize, signed: bool, value: BigInt) -> LogicVec {
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
        Self::new_xz(width, signed, value, BigUint::zero())
    }

    /// Convert from i32 to integer
    pub fn from_integer(value: i32) -> LogicVec {
        Self::from(32, true, BigInt::from_i32(value).unwrap())
    }

    /// Fill a vector with a value
    pub fn fill(width: usize, signed: bool, value: LogicValue) -> LogicVec {
        let (xz, value) = match value {
            LogicValue::Zero => (false, false),
            LogicValue::One => (false, true),
            LogicValue::Z => (true, false),
            LogicValue::X => (true, true),
        };
        LogicVec {
            signed,
            value: Int::fill(width, value),
            xz: Int::fill(width, xz),
        }
    }

    fn replace_with_x(&mut self) {
        *self = Self::fill(self.value.width(), self.signed, LogicValue::X);
    }

    /// Get the width of this number
    pub fn width(&self) -> usize {
        self.value.width()
    }

    /// Check if this is signed
    pub fn signed(&self) -> bool {
        self.signed
    }

    /// Check if this is a 2-state logic
    pub fn is_two_state(&self) -> bool {
        self.xz.is_zero()
    }

    /// Convert to two state value. If there is a Z or X, `None` is returned.
    pub fn get_two_state(&self) -> Option<BigInt> {
        if self.is_two_state() {
            if self.signed {
                Some(self.value.clone().to_bigint())
            } else {
                Some(BigInt::from_biguint(
                    Sign::Minus,
                    self.value.clone().to_biguint(),
                ))
            }
        } else {
            None
        }
    }

    /// Force as two state value. X and Zs will be converted to 0.
    pub fn force_two_state(mut self) -> Self {
        self.value &= &!self.xz;
        self.xz = Int::zero(self.value.width());
        self
    }

    /// Perform sign extension or truncation
    pub fn sign_extend_or_trunc(&self, width: usize) -> Self {
        let mut value = self.value.clone();
        let mut xz = self.xz.clone();
        value.sign_extend_or_trunc(width);
        xz.sign_extend_or_trunc(width);

        Self {
            signed: self.signed,
            value,
            xz,
        }
    }

    /// Perform xz-extension or truncation
    pub fn xz_extend_or_trunc(&self, width: usize) -> Self {
        let mut value = self.value.clone();
        let mut xz = self.xz.clone();

        if self.xz.bit_at(self.value.width() - 1) {
            // If highest bit is XZ, do sign extension on value
            value.sign_extend_or_trunc(width);
            xz.one_extend_or_trunc(width);
        } else {
            value.zero_extend_or_trunc(width);
            xz.zero_extend_or_trunc(width);
        }

        Self {
            signed: self.signed,
            value,
            xz,
        }
    }

    /// Perform extension or truncation
    pub fn extend_or_trunc(&self, width: usize) -> Self {
        if self.signed {
            self.sign_extend_or_trunc(width)
        } else {
            self.xz_extend_or_trunc(width)
        }
    }

    pub fn concat_assign(&mut self, another: &Self) {
        self.signed = false;
        self.value.concat_assign(&another.value);
        self.xz.concat_assign(&another.xz);
    }

    pub fn duplicate(&self, count: usize) -> Self {
        Self {
            signed: self.signed,
            value: self.value.duplicate(count),
            xz: self.xz.duplicate(count),
        }
    }

    pub fn cmp_with_zero(&self) -> cmp::Ordering {
        if self.value.is_zero() {
            cmp::Ordering::Equal
        } else if self.signed && self.value.sign_bit() {
            return cmp::Ordering::Less;
        } else {
            cmp::Ordering::Greater
        }
    }
}

impl From<LogicValue> for LogicVec {
    fn from(val: LogicValue) -> LogicVec {
        LogicVec::fill(1, false, val)
    }
}

//
// Arithmetic of LogicVec
//

impl<'a> ops::AddAssign<&'a LogicVec> for LogicVec {
    fn add_assign(&mut self, rhs: &Self) {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() {
            return self.replace_with_x();
        }
        self.value += &rhs.value;
    }
}

impl ops::AddAssign<i32> for LogicVec {
    fn add_assign(&mut self, rhs: i32) {
        if !self.is_two_state() {
            return self.replace_with_x();
        }
        let v = Int::from_bigint(self.width(), rhs.into());
        self.value += &v;
    }
}

impl<'a> ops::SubAssign<&'a LogicVec> for LogicVec {
    fn sub_assign(&mut self, rhs: &Self) {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() {
            return self.replace_with_x();
        }
        self.value -= &rhs.value;
    }
}

impl<'a> ops::MulAssign<&'a LogicVec> for LogicVec {
    fn mul_assign(&mut self, rhs: &Self) {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() {
            return self.replace_with_x();
        }
        self.value *= &rhs.value;
    }
}

impl<'a> ops::DivAssign<&'a LogicVec> for LogicVec {
    fn div_assign(&mut self, rhs: &Self) {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() || rhs.value.is_zero() {
            return self.replace_with_x();
        }
        if self.signed {
            self.value.signed_div(&rhs.value);
        } else {
            self.value /= &rhs.value;
        }
    }
}

impl<'a> ops::RemAssign<&'a LogicVec> for LogicVec {
    fn rem_assign(&mut self, rhs: &Self) {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() || rhs.value.is_zero() {
            return self.replace_with_x();
        }
        if self.signed {
            self.value.signed_rem(&rhs.value);
        } else {
            self.value %= &rhs.value;
        }
    }
}

impl LogicVec {
    pub fn pow_assign(&mut self, rhs: &Self) {
        if !self.is_two_state() || !rhs.is_two_state() {
            return self.replace_with_x();
        }

        // Take the value out as there are few operations that require value access.
        let width = self.value.width();
        let mut value = std::mem::replace(&mut self.value, Int::zero(width));

        // self ** 0 == 1
        if rhs.value.is_zero() {
            return self.value = Int::new(value.width(), BigUint::one());
        }

        // If self is zero
        if value.is_zero() {
            // If rhs is negative, return 'x, otherwise stick with 0
            if rhs.signed && rhs.value.sign_bit() {
                self.replace_with_x();
            }
            return;
        }

        if self.signed && value.sign_bit() {
            // If self negative number
            let rhs_parity = rhs.value.bit_at(0);
            // Get the positive part of value
            value = -value;
            // Cases for -1:
            if value.is_one() {
                // If RHS is odd, return -1, otherwise 1.
                if rhs_parity {
                    value = -value;
                }
            } else {
                if rhs.signed && rhs.value.sign_bit() {
                    // Value is already replaced with zero.
                    return;
                } else {
                    value.pow_assign(&rhs.value);
                    if rhs_parity {
                        value = -value;
                    }
                }
            }
        } else {
            // self is positive number
            if rhs.signed && rhs.value.sign_bit() {
                // Value is already replaced with zero.
                return;
            } else {
                value.pow_assign(&rhs.value);
            }
        }
        self.value = value;
    }
}

impl ops::Neg for LogicVec {
    type Output = LogicVec;
    fn neg(mut self) -> Self {
        if !self.is_two_state() {
            self.replace_with_x();
            return self;
        }
        self.value = -self.value;
        self
    }
}

impl<'a> ops::ShlAssign<&'a LogicVec> for LogicVec {
    fn shl_assign(&mut self, rhs: &Self) {
        // The rhs should always be unsigned.
        assert!(!rhs.signed);

        if !rhs.is_two_state() {
            return self.replace_with_x();
        }

        self.value <<= &rhs.value;
        self.xz <<= &rhs.value;
    }
}

impl LogicVec {
    pub fn l_shr(&mut self, rhs: &Self) {
        // The rhs should always be unsigned.
        assert!(!rhs.signed);

        // If right hand side is not two-state, then this is a X.
        if !rhs.is_two_state() {
            return *self = Self::fill(self.value.width(), self.signed, LogicValue::X);
        }

        self.value.zero_shr(&rhs.value);
        self.xz.zero_shr(&rhs.value);
    }

    pub fn a_shr(&mut self, rhs: &Self) {
        // The rhs should always be unsigned.
        assert!(!rhs.signed);

        // If right hand side is not two-state, then this is a X.
        if !rhs.is_two_state() {
            return *self = Self::fill(self.value.width(), self.signed, LogicValue::X);
        }

        if self.signed {
            self.value.sign_shr(&rhs.value);
            self.xz.sign_shr(&rhs.value);
        } else {
            self.value.zero_shr(&rhs.value);
            self.xz.zero_shr(&rhs.value);
        }
    }
}

//
// Bitwise operations
//

impl<'a> ops::BitXorAssign<&'a LogicVec> for LogicVec {
    fn bitxor_assign(&mut self, rhs: &Self) {
        self.value ^= &rhs.value;
        // When X or Z exist, corresponding bit will be an X.
        // So it is basically OR xz onto value
        self.value |= &self.xz;
        self.value |= &rhs.xz;
    }
}

impl<'a> ops::BitAndAssign<&'a LogicVec> for LogicVec {
    fn bitand_assign(&mut self, rhs: &Self) {
        // The calculation is derived from logical minimisation
        self.value |= &self.xz;
        self.value &= &{
            let mut tmp = rhs.value.clone();
            tmp |= &rhs.xz;
            tmp
        };
        self.xz |= &rhs.xz;
        self.xz &= &self.value;
    }
}

impl<'a> ops::BitOrAssign<&'a LogicVec> for LogicVec {
    fn bitor_assign(&mut self, rhs: &Self) {
        // The calculation is derived from logical minimisation
        let mut new_xz = !self.value.clone();
        new_xz &= &rhs.xz;
        self.value |= &rhs.value;
        self.value |= &self.xz;
        self.value |= &rhs.xz;
        new_xz |= &{
            let mut tmp = !rhs.value.clone();
            tmp &= &self.xz;
            tmp
        };
        self.xz &= &rhs.xz;
        self.xz |= &new_xz;
    }
}

impl ops::Not for LogicVec {
    type Output = Self;
    fn not(mut self) -> Self {
        self.value = !self.value;
        // When X or Z exist, corresponding bit will be an X.
        // So it is basically OR xz onto value
        self.value |= &self.xz;
        self
    }
}

// Logical operations
impl LogicVec {
    /// Reduction or
    pub fn reduce_or(&self) -> LogicValue {
        if self.is_two_state() {
            // Simple case, just check if value is zero
            return self.value.reduce_or().into();
        }

        // First convert to two-state (this convert all X's and Z's into 0)
        let two_state = self.clone().force_two_state();
        if two_state.value.reduce_or() {
            // This means that there is no single digit or one.
            LogicValue::X
        } else {
            LogicValue::One
        }
    }

    pub fn logic_eq(&self, rhs: &Self) -> LogicValue {
        if self.is_two_state() && rhs.is_two_state() {
            // Simple case, direct comparision
            return (self.value == rhs.value).into();
        }

        // Otherwise XNOR them together for bitwise-equality test
        let mut val = self.clone();
        val ^= rhs;
        val = !val;

        // Then covnert the value to boolean
        val.to_bool()
    }

    /// Convert to boolean (single LogicValue)
    pub fn to_bool(&self) -> LogicValue {
        // This is equivalent to a reduction or.
        self.reduce_or()
    }
}

//
// Numerical Comparision
//
impl LogicVec {
    pub fn lt(&self, rhs: &Self) -> LogicValue {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() {
            return LogicValue::X;
        }
        (if self.signed {
            self.value.signed_cmp(&rhs.value)
        } else {
            self.value.cmp(&rhs.value)
        } == cmp::Ordering::Less)
            .into()
    }

    pub fn le(&self, rhs: &Self) -> LogicValue {
        assert!(self.signed == rhs.signed);
        if !self.is_two_state() || !rhs.is_two_state() {
            return LogicValue::X;
        }
        (if self.signed {
            self.value.signed_cmp(&rhs.value)
        } else {
            self.value.cmp(&rhs.value)
        } != cmp::Ordering::Greater)
            .into()
    }
}

impl fmt::Debug for LogicVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let width = self.width();
        let value = self.value.clone().to_biguint();
        let xz = self.xz.clone().to_biguint();
        write!(f, "{}'", width)?;
        if self.signed {
            write!(f, "s")?;
        }
        if self.is_two_state() {
            // We use a heuristics for display number. If the number is below 1024, display it as
            // decimal. Otherwise display as hex.
            if &value < &(1024 as u16).into() {
                write!(f, "d{}", value)
            } else {
                write!(f, "h{:X}", value)
            }
        } else {
            let mut str = format!("{:0width$b}", value, width = width).into_bytes();
            let xz = format!("{:0width$b}", xz, width = width).into_bytes();
            for i in 0..xz.len() {
                if xz[i] == b'1' {
                    str[i] = if str[i] == b'0' { b'z' } else { b'x' }
                }
            }
            write!(f, "b{}", String::from_utf8(str).unwrap())
        }
    }
}

/// Represntation of Verilog's 4-state logic array
#[derive(Clone, PartialEq)]
pub struct LogicNumber {
    pub sized: bool,
    pub value: LogicVec,
}

impl fmt::Display for LogicNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let width = self.value.width();
        let value = self.value.value.clone().to_biguint();
        let xz = self.value.xz.clone().to_biguint();

        // In this case we can simply print out a decimal
        if self.value.is_two_state() && !self.sized && self.value.signed {
            return write!(f, "{}", value);
        }
        if self.sized {
            write!(f, "{}", width)?;
        }
        write!(f, "'")?;
        if self.value.signed {
            write!(f, "s")?;
        }
        if xz.is_zero() {
            write!(f, "d{}", value)
        } else {
            let mut str = format!("{:0width$b}", value, width = width).into_bytes();
            let xz = format!("{:0width$b}", xz, width = width).into_bytes();
            for i in 0..xz.len() {
                if xz[i] == b'1' {
                    str[i] = if str[i] == b'0' { b'z' } else { b'x' }
                }
            }
            write!(f, "b{}", String::from_utf8(str).unwrap())
        }
    }
}

impl fmt::Debug for LogicNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, f)
    }
}
