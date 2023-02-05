use std::{
    fmt::{self, Display},
    ops::{Deref, DerefMut},
};

use num_bigint::BigInt;
use num_rational::Rational64;
use num_traits::ToPrimitive;

use crate::{
    gc::{GcHeader, GcRef, ObjectType},
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

    pub fn eq(f1: isize, f2: Flonum) -> bool {
        if f2.is_finite() || f2.is_nan() {
            false
        } else {
            (f1 as f64) == *f2
        }
    }
}

impl Deref for Flonum {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.value }
    }
}

impl DerefMut for Flonum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.value }
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

impl Deref for Ratnum {
    type Target = Rational64;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.ratio }
    }
}

impl DerefMut for Ratnum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.ratio }
    }
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

    pub fn eq(f1: isize, r2: &GcRef<Ratnum>) -> bool {
        match r2.to_isize() {
            Some(v) => v == f1,
            None => false,
        }
    }
}

impl Display for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<ratnum>")
    }
}

/// Big number.
#[derive(Debug)]
pub struct Bignum {
    pub header: GcHeader,
    pub value: BigInt,
}

impl Deref for Bignum {
    type Target = BigInt;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.value }
    }
}

impl DerefMut for Bignum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut self.value }
    }
}

impl Bignum {}

impl Display for Bignum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<bignum>")
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

    pub fn is_real(&self) -> bool {
        number_eq(self.imag, Object::Fixnum(0)) && self.imag.is_exact()
    }
}

impl Display for Compnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<compnum>")
    }
}

/// Number functions.
pub fn number_eq(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f1), Object::Fixnum(f2)) => f1 == f2,
        (Object::Fixnum(f1), Object::Flonum(f2)) => Flonum::eq(f1, f2),
        (Object::Fixnum(f1), Object::Ratnum(r2)) => Ratnum::eq(f1, &r2),

        _ => todo!(),
    }
}
/*
MOSH_ASSERT(n1.isNumber());
MOSH_ASSERT(n2.isNumber());
if (n1.isFixnum()) {
    if (n2.isFixnum()) {
        return Fixnum::eq(n1.toFixnum(), n2.toFixnum());
    } else if (n2.isFlonum()) {
        return Flonum::eq(n1.toFixnum(), n2.toFlonum());
    } else if (n2.isRatnum()) {
        return Ratnum::eq(n1.toFixnum(), n2.toRatnum());
    } else if (n2.isBignum()) {
        return Bignum::eq(n1.toFixnum(), n2.toBignum());
    } else if (n2.isCompnum()) {
        return Compnum::eq(n1, n2.toCompnum());
    }
} else if (n1.isFlonum()) {
    if (n2.isFixnum()) {
        return Flonum::eq(n1.toFlonum(), n2.toFixnum());
    } else if (n2.isFlonum()) {
        return Flonum::eq(n1.toFlonum(), n2.toFlonum());
    } else if (n2.isRatnum()) {
        return Flonum::eq(n1.toFlonum(), n2.toRatnum());
    } else if (n2.isBignum()) {
        Bignum* b1 = new Bignum(n1.toFlonum()->value());
        return Bignum::eq(b1, n2.toBignum());
    } else if (n2.isCompnum()) {
        return Compnum::eq(n1, n2.toCompnum());
    }
} else if (n1.isBignum()) {
    if (n2.isFixnum()) {
        return Bignum::eq(n1.toBignum(), n2.toFixnum());
    } else if (n2.isFlonum()) {
        Bignum *b2 = new Bignum(n2.toFlonum()->value());
        return Bignum::eq(n1.toBignum(), b2);
    } else if (n2.isRatnum()) {
        return Ratnum::eq(n1.toBignum(), n2.toRatnum());
    } else if (n2.isBignum()) {
        return Bignum::eq(n1.toBignum(), n2.toBignum());
    } else if (n2.isCompnum()) {
        return Compnum::eq(n1, n2.toCompnum());
    }
} else if (n1.isRatnum()) {
    if (n2.isFixnum()) {
        return Ratnum::eq(n1.toRatnum(), n2.toFixnum());
    } else if (n2.isFlonum()) {
        return Flonum::eq(n1.toRatnum(), n2.toFlonum());
    } else if (n2.isRatnum()) {
        return Ratnum::eq(n1.toRatnum(), n2.toRatnum());
    } else if (n2.isBignum()) {
        return Ratnum::eq(n1.toRatnum(), n2.toBignum());
    } else if (n2.isCompnum()) {
        return Compnum::eq(n1, n2.toCompnum());
    }
} else if (n1.isCompnum()) {
    if (n2.isFixnum()) {
        return Compnum::eq(n1.toCompnum(), n2);
    } else if (n2.isRatnum()) {
        return Compnum::eq(n1.toCompnum(), n2);
    } else if (n2.isFlonum()) {
        return Compnum::eq(n1.toCompnum(), n2);
    } else if (n2.isBignum()) {
        return Compnum::eq(n1.toCompnum(), n2);
    } else if (n2.isCompnum()) {
        return Compnum::eq(n1.toCompnum(), n2.toCompnum());
    }
}
return false;
*/
impl Object {
    #[inline(always)]
    pub fn is_exact(&self) -> bool {
        false
    }
    #[inline(always)]
    pub fn is_number(&self) -> bool {
        self.is_real() || self.is_compnum()
    }

    #[inline(always)]
    pub fn is_complex(&self) -> bool {
        self.is_real() || self.is_compnum()
    }

    #[inline(always)]
    pub fn is_exact_integer(&self) -> bool {
        self.is_fixnum() || self.is_bignum()
    }

    #[inline(always)]
    pub fn is_real(&self) -> bool {
        self.is_fixnum()
            || self.is_bignum()
            || self.is_flonum()
            || self.is_ratnum()
            || (self.is_compnum() && self.to_compnum().is_real())
    }
}
