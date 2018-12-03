use num::{BigUint, One, Zero, ToPrimitive};
use std::fmt;

/// Represntation of Verilog's 4-value logic
#[derive(Debug, Clone)]
pub enum LogicValue {
    Zero,
    One,
    Z,
    X,
}

/// Represntation of Verilog's 4-value logic array
#[derive(Clone)]
pub struct LogicNumber {
    pub width: usize,
    pub sized: bool,
    pub signed: bool,
    pub value: BigUint,
    pub xz: BigUint,
}

impl LogicNumber {

    pub fn value_bit_at(&self, index: usize) -> bool {
        if index >= self.width {
            panic!("out of bound");
        }
        ((&self.value >> index).to_u32().unwrap() & 1) != 0
    }

    pub fn xz_bit_at(&self, index: usize) -> bool {
        if index >= self.width {
            panic!("out of bound");
        }
        ((&self.xz >> index).to_u32().unwrap() & 1) != 0
    }

    /// Perform x/z-extension or truncation
    pub fn x_extend(&mut self, width: usize) {
        if width == self.width { return; }
        if width < self.width {
            // Truncate
            let mut val = BigUint::one();
            val <<= width;
            val -= 1 as u8;
            self.value &= &val;
            self.xz &= val;
        } else {
            // X-Extension
            let need_extend = self.xz_bit_at(self.width - 1);
            if need_extend {
                let mut extend_mask = BigUint::one();
                extend_mask <<= width - self.width;
                extend_mask -= 1 as u8;
                extend_mask <<= self.width;

                if self.value_bit_at(self.width - 1) {
                    self.value |= &extend_mask;
                }
                self.xz |= extend_mask;
            }
        }
        self.width = width;
    }
}

impl fmt::Display for LogicNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.sized {
            write!(f, "{}", self.width)?;
        }
        write!(f, "'")?;
        if self.signed {
            write!(f, "s")?;
        }
        if self.xz.is_zero() {
            write!(f, "d{}", self.value)
        } else {
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
}

impl fmt::Debug for LogicNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, f)
    }
}
