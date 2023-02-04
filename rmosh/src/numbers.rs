use std::fmt::{self, Display};

use num_rational::Rational64;

use crate::{
    gc::{GcHeader, ObjectType},
    objects::Object,
};

// Float number
// We use this struct which wraps f64.
// Because we can't implement Hash for f64.
#[derive(Copy, Clone)]
pub union Flonum {
    value: f64,
    u64_value: u64,
}

impl std::hash::Hash for Flonum {
    fn hash<H>(&self, state: &mut H)
    where
        H: std::hash::Hasher,
    {
        state.write_u64(unsafe { self.u64_value });
        state.finish();
    }
}

impl Flonum {
    pub fn new(value: f64) -> Self {
        Self { value: value }
    }
    #[inline(always)]
    pub fn value(&self) -> f64 {
        unsafe { self.value }
    }
}

impl PartialEq for Flonum {
    fn eq(&self, other: &Flonum) -> bool {
        unsafe { self.u64_value == other.u64_value }
    }
}

impl Display for Flonum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

/// Rational number.
#[derive(Debug)]
pub struct Ratnum {
    pub header: GcHeader,
    pub ratio: Rational64,
}

impl Ratnum {
    pub fn new(numer: isize, denom: isize) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Ratnum),
            ratio: Rational64::new_raw(numer as i64, denom as i64),
        }
    }
    pub fn new_from_ratio(ratio: Rational64) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Ratnum),
            ratio: ratio,
        }
    }
}

impl Display for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<ratnum>")
    }
}

/// Complex number.
#[derive(Debug)]
pub struct Compnum {
    pub header: GcHeader,
    real: Object,
    imag: Object,
}

impl Compnum {
    pub fn new(real: Object, imag: Object) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Compnum),
            real: real,
            imag: imag,
        }
    }
}

impl Display for Compnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<compnum>")
    }
}
