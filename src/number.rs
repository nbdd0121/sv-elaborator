use num::{BigUint, BigInt, bigint::Sign, One, Zero, ToPrimitive};
use std::fmt;

/// Represntation of Verilog's 4-state logic
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicValue {
    Zero,
    One,
    Z,
    X,
}

impl fmt::Display for LogicValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
    pub width: usize,
    pub signed: bool,
    pub value: BigUint,
    pub xz: BigUint,
}

impl LogicVec {

    /// Construct a 4-state logic array from 2-state logic.
    pub fn new(width: usize, signed: bool, mut value: BigUint) -> LogicVec {
        if width < value.bits() {
            let mut val = BigUint::one();
            val <<= width;
            val -= 1 as u8;
            value &= val;
        }

        LogicVec {
            width,
            signed,
            value,
            xz: BigUint::zero(),
        }
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
        Self::new(width, signed, value)
    }

    /// Fill a vector with a value
    pub fn fill(width: usize, signed: bool, value: LogicValue) -> LogicVec {
        let mut vec: Self = (&value).into();
        vec.signed = signed;
        vec.duplicate(width)
    }

    fn value_bit_at(&self, index: usize) -> bool {
        if index >= self.width {
            panic!("out of bound");
        }
        ((&self.value >> index).to_u32().unwrap() & 1) != 0
    }

    fn xz_bit_at(&self, index: usize) -> bool {
        if index >= self.width {
            panic!("out of bound");
        }
        ((&self.xz >> index).to_u32().unwrap() & 1) != 0
    }

    fn truncate(&self, width: usize) -> Self {
        let mut val = BigUint::one();
        val <<= width;
        val -= 1 as u8;
        let value = &self.value & &val;
        let xz = &self.xz & &val;
        Self {
            width,
            signed: self.signed,
            value,
            xz,
        }
    }

    /// Check if this is a 2-state logic
    pub fn is_two_state(&self) -> bool {
        self.xz.is_zero()
    }

    /// Convert to unsigned two state value. If there is a Z or X, `None` is returned.
    pub fn get_two_state_unsigned(&self) -> Option<BigUint> {
        if self.is_two_state() {
            Some(self.value.clone())
        } else {
            None
        }
    }

    /// Convert to two state value. If there is a Z or X, `None` is returned.
    pub fn get_two_state(&self) -> Option<BigInt> {
        if self.is_two_state() {
            if self.signed && self.value_bit_at(self.width - 1) {
                let mut ret = BigUint::one();
                ret <<= self.width;
                ret -= &self.value;
                Some(BigInt::from_biguint(Sign::Minus, ret))
            } else {
                Some(BigInt::from_biguint(Sign::Plus, self.value.clone()))
            }
        } else {
            None
        }
    }

    /// Force as two state value. X and Zs will be converted to 0.
    pub fn force_two_state(mut self) -> Self {
        self.value |= &self.xz;
        self.value ^= self.xz;
        self.xz = BigUint::zero();
        self
    }

    /// Perform sign extension or truncation
    pub fn sign_extend_or_trunc(&self, width: usize) -> Self {
        if width == self.width { return self.clone(); }
        if width < self.width { return self.truncate(width); }

        // Calculate the mask for extension
        let mut extend_mask = BigUint::one();
        extend_mask <<= width - self.width;
        extend_mask -= 1 as u8;
        extend_mask <<= self.width;

        let mut value = self.value.clone();
        let mut xz = self.xz.clone();
        if self.value_bit_at(self.width - 1) { value |= &extend_mask; }
        if self.xz_bit_at(self.width - 1) { xz |= &extend_mask; }

        Self {
            width,
            signed: self.signed,
            value,
            xz,
        }
    }

    /// Perform xz-extension or truncation
    pub fn xz_extend_or_trunc(&self, width: usize) -> Self {
        if width == self.width { return self.clone(); }
        if width < self.width { return self.truncate(width); }

        let mut value = self.value.clone();
        let mut xz = self.xz.clone();

        if self.xz_bit_at(self.width - 1) {
            // Calculate the mask for extension
            let mut extend_mask = BigUint::one();
            extend_mask <<= width - self.width;
            extend_mask -= 1 as u8;
            extend_mask <<= self.width;

            if self.value_bit_at(self.width - 1) { value |= &extend_mask; }
            xz |= &extend_mask;
        }

        Self {
            width,
            signed: self.signed,
            value,
            xz,
        }
    }

    pub fn duplicate(&self, count: usize) -> Self {
        // Currently we assume count is small, and does not use O(logn) algorithm.
        let mut value = BigUint::zero();
        let mut xz = BigUint::zero();
        for _ in 0..count {
            value <<= self.width;
            value |= &self.value;
            xz <<= self.width;
            xz |= &self.xz;
        }
        Self {
            width: self.width * count,
            signed: self.signed,
            value,
            xz,
        }
    }
}

impl<'a> From<&'a LogicValue> for LogicVec {
    fn from(val: &'a LogicValue) -> LogicVec {
        let (xz, value) = match val {
            LogicValue::Zero => (BigUint::zero(), BigUint::zero()),
            LogicValue::One => (BigUint::zero(), BigUint::one()),
            LogicValue::Z => (BigUint::one(), BigUint::zero()),
            LogicValue::X => (BigUint::one(), BigUint::one()),
        };
        LogicVec {
            width: 1,
            signed: false,
            value,
            xz,
        }
    }
}

impl fmt::Debug for LogicVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut str = format!("{:0width$b}", self.value, width=self.width).into_bytes();
        let xz = format!("{:0width$b}", self.xz, width=self.width).into_bytes();
        for i in 0..xz.len() {
            if xz[i] == b'1' {
                str[i] = if str[i] == b'0' { b'z' } else { b'x' }
            }
        }
        write!(f, "b{}", String::from_utf8(str).unwrap())
    }
}

/// Represntation of Verilog's 4-state logic array
#[derive(Clone, PartialEq)]
pub struct LogicNumber {
    pub sized: bool,
    pub value: LogicVec,
}

impl fmt::Display for LogicNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        // In this case we can simply print out a decimal
        if self.value.is_two_state() && !self.sized && self.value.signed {
            return write!(f, "{}", self.value.value)
        }
        if self.sized {
            write!(f, "{}", self.value.width)?;
        }
        write!(f, "'")?;
        if self.value.signed {
            write!(f, "s")?;
        }
        if self.value.xz.is_zero() {
            write!(f, "d{}", self.value.value)
        } else {
            let mut str = format!("{:0width$b}", self.value.value, width=self.value.width).into_bytes();
            let xz = format!("{:0width$b}", self.value.xz, width=self.value.width).into_bytes();
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
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, f)
    }
}
