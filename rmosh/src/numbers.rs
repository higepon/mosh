use std::{
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut},
};

use num_bigint::BigInt;
use num_rational::Rational64;
use num_traits::{FromPrimitive, Signed, ToPrimitive};

use crate::{
    gc::{Gc, GcHeader, GcRef, ObjectType},
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

    pub fn new_from_64(u64_value: u64) -> Self {
        Self {
            u64_value: u64_value,
        }
    }

    #[inline(always)]
    pub fn value(&self) -> f64 {
        unsafe { self.value }
    }

    #[inline(always)]
    pub fn u64_value(&self) -> u64 {
        unsafe { self.u64_value }
    }

    #[inline(always)]
    pub fn eq(&self, other: &Flonum) -> bool {
        self.value() == other.value()
    }

    #[inline(always)]
    pub fn add(&self, other: &Flonum) -> Object {
        Object::Flonum(Flonum::new(self.value() + other.value()))
    }

    #[inline(always)]
    pub fn gt(&self, other: &Flonum) -> bool {
        self.value() > other.value()
    }

    #[inline(always)]
    pub fn abs(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().abs()))
    }

    pub fn fx_add(&self, fx: isize) -> Object {
        let f = (fx as f64) + self.value();
        Object::Flonum(Flonum::new(f))
    }

    pub fn fx_div(&self, fx: isize) -> Result<Object, SchemeError> {
        if fx == 0 {
            Err(SchemeError::Div0)
        } else if fx == 1 {
            Ok(Object::Flonum(Flonum::new(self.value())))
        } else {
            let value = self.value() / (fx as f64);
            Ok(Object::Flonum(Flonum::new(value)))
        }
    }

    pub fn fx_eq(&self, fx: isize) -> bool {
        if self.is_finite() || self.is_nan() {
            false
        } else {
            (fx as f64) == self.value()
        }
    }

    pub fn fx_lt(&self, f: isize) -> bool {
        if self.is_finite() || self.is_nan() {
            false
        } else {
            (f as f64) < self.value()
        }
    }

    pub fn is_rational(&self) -> bool {
        !self.is_nan() && !self.is_infinite()
    }

    pub fn to_exact(&self, gc: &mut Box<Gc>) -> Object {
        match Rational64::from_f64(**self) {
            Some(r) => Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r))),
            None => {
                todo!()
            }
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

impl Debug for Flonum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}

/// Rational number.
pub struct Ratnum {
    pub header: GcHeader,
    pub ratio: Rational64,
}

impl Deref for Ratnum {
    type Target = Rational64;

    fn deref(&self) -> &Self::Target {
        &self.ratio
    }
}

impl DerefMut for Ratnum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.ratio
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

    pub fn eq(&self, other: &Ratnum) -> bool {
        self.ratio.eq(&other.ratio)
    }

    pub fn denom(&self) -> Object {
        Object::Fixnum(*self.ratio.denom() as isize)
    }

    pub fn numer(&self) -> Object {
        Object::Fixnum(*self.ratio.numer() as isize)
    }

    pub fn fx_eq(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v == f,
            None => false,
        }
    }

    pub fn abs(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.abs())))
    }

    pub fn fx_add(&self, gc: &mut Box<Gc>, fx: isize) -> Object {
        let r = self.ratio + Rational64::new_raw(fx as i64, 1);
        if r.is_integer() {
            Object::Fixnum(*r.numer() as isize)
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }

    pub fn div_by_fx(&self, gc: &mut Box<Gc>, fx: isize) -> Result<Object, SchemeError> {
        if fx == 0 {
            Err(SchemeError::Div0)
        } else {
            let r = self.ratio / Rational64::new_raw(fx as i64, 1);
            if r.is_integer() {
                Ok(Object::Fixnum(*r.numer() as isize))
            } else {
                Ok(Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r))))
            }
        }
    }

    pub fn fx_div(&self, gc: &mut Box<Gc>, fx: isize) -> Result<Object, SchemeError> {
        let r = Rational64::new_raw(fx as i64, 1) / self.ratio;
        if r.is_integer() {
            Ok(Object::Fixnum(*r.numer() as isize))
        } else {
            Ok(Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r))))
        }
    }

    pub fn fx_lt(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v < f,
            None => false,
        }
    }
    pub fn fl_eq(&self, fl: &Flonum) -> bool {
        match self.to_f64() {
            Some(v) => v == **fl,
            None => false,
        }
    }
    pub fn fl_lt(&self, fl: &Flonum) -> bool {
        match self.to_f64() {
            Some(v) => v < **fl,
            None => false,
        }
    }

    pub fn bi_eq(&self, b: &Bignum) -> bool {
        match (b.to_f64(), self.to_f64()) {
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }

    pub fn bi_lt(&self, b: &Bignum) -> bool {
        match (b.to_f64(), self.to_f64()) {
            (Some(l), Some(r)) => l < r,
            _ => false,
        }
    }

    pub fn add(&self, gc: &mut Box<Gc>, other: GcRef<Ratnum>) -> Object {
        let r = self.ratio + other.ratio;
        if r.is_integer() {
            Object::Fixnum(r.to_isize().unwrap())
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }
}

impl Display for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<ratnum {}>", self.ratio)
    }
}

impl Debug for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<ratnum {}>", self.ratio)
    }
}

/// Big number.
pub struct Bignum {
    pub header: GcHeader,
    pub value: BigInt,
}

impl Deref for Bignum {
    type Target = BigInt;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl DerefMut for Bignum {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl Bignum {
    pub fn new(value: BigInt) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Bignum),
            value: value,
        }
    }
    pub fn fx_eq(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v == f,
            None => false,
        }
    }
    pub fn fx_lt(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v < f,
            None => false,
        }
    }
    pub fn eq(&self, other: &Bignum) -> bool {
        self.value.eq(&other.value)
    }
    pub fn fl_eq(&self, fl: &Flonum) -> bool {
        match self.to_f64() {
            Some(v) => v == **fl,
            None => false,
        }
    }
    pub fn fl_lt(&self, fl: &Flonum) -> bool {
        match self.to_f64() {
            Some(v) => v < **fl,
            None => false,
        }
    }

    pub fn abs(&self, gc: &mut Box<Gc>) -> Object {
        Object::Bignum(gc.alloc(Bignum::new(self.value.abs())))
    }

    pub fn fx_mul(gc: &mut Box<Gc>, fx1: isize, fx2: isize) -> Object {
        match fx1.checked_mul(fx2) {
            Some(value) => Object::Fixnum(value),
            None => {
                let b1 = BigInt::from_isize(fx1).unwrap();
                let b2 = BigInt::from_isize(fx2).unwrap();
                let b = b1 * b2;
                Object::Bignum(gc.alloc(Bignum::new(b)))
            }
        }
    }

    pub fn fx_add(gc: &mut Box<Gc>, fx1: isize, fx2: isize) -> Object {
        match fx1.checked_add(fx2) {
            Some(value) => Object::Fixnum(value),
            None => {
                let b1 = BigInt::from_isize(fx1).unwrap();
                let b2 = BigInt::from_isize(fx2).unwrap();
                let b = b1 + b2;
                Object::Bignum(gc.alloc(Bignum::new(b)))
            }
        }
    }
    pub fn fx_div(gc: &mut Box<Gc>, fx1: isize, fx2: isize) -> Result<Object, SchemeError> {
        if fx2 == 0 {
            Err(SchemeError::Div0)
        } else if fx2 == 1 {
            Ok(Object::Fixnum(fx1))
        } else {
            let r = Ratnum::new(fx1, fx2);
            if r.is_integer() {
                Ok(r.numer())
            } else {
                Ok(Object::Ratnum(gc.alloc(r)))
            }
        }
    }
}

impl Display for Bignum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<bignum>")
    }
}

impl Debug for Bignum {
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

    pub fn obj_eq(&self, o: Object) -> bool {
        assert!(o.is_fixnum() || o.is_bignum() || o.is_flonum() || o.is_ratnum());
        number_eq(self.imag, Object::Fixnum(0)) && number_eq(self.real, o)
    }
    pub fn eq(&self, other: &Compnum) -> bool {
        number_eq(self.real, other.real) && number_eq(other.imag, other.imag)
    }
}

impl Display for Compnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+/-{}i", self.real.to_string(), self.imag.to_string())
    }
}

/// Number functions.
pub fn number_add(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => Bignum::fx_add(gc, fx1, fx2),
        (Object::Fixnum(fx), Object::Flonum(fl)) => fl.fx_add(fx),
        (Object::Fixnum(fx), Object::Ratnum(r)) => r.fx_add(gc, fx),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fl.fx_add(fx),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.add(&fl2),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => r.fx_add(gc, fx),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.add(gc, r2),
        (Object::Ratnum(_), Object::Bignum(_)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(_)) => todo!(),
        (Object::Compnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(_), Object::Flonum(_)) => todo!(),
        (Object::Compnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(_), Object::Bignum(_)) => todo!(),
        (Object::Compnum(_), Object::Compnum(_)) => todo!(),
        _ => todo!(),
    }
}
pub fn number_sub(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => todo!(),
        (Object::Fixnum(fx), Object::Flonum(fl)) => todo!(),
        (Object::Fixnum(fx), Object::Ratnum(r)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => todo!(),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) =>todo!(), 
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => todo!(),
        (Object::Ratnum(_), Object::Bignum(_)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(_)) => todo!(),
        (Object::Compnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(_), Object::Flonum(_)) => todo!(),
        (Object::Compnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(_), Object::Bignum(_)) => todo!(),
        (Object::Compnum(_), Object::Compnum(_)) => todo!(),
        _ => todo!(),
    }
}

pub enum SchemeError {
    Div0,
}
pub fn number_div(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => Bignum::fx_div(gc, fx1, fx2),
        (Object::Fixnum(_), Object::Flonum(_)) => todo!(),
        (Object::Fixnum(fx), Object::Ratnum(r)) => r.fx_div(gc, fx),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fl.fx_div(fx),
        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => r.div_by_fx(gc, fx),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Bignum(_)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(_)) => todo!(),
        (Object::Compnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(_), Object::Flonum(_)) => todo!(),
        (Object::Compnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(_), Object::Bignum(_)) => todo!(),
        (Object::Compnum(_), Object::Compnum(_)) => todo!(),
        _ => todo!(),
    }
}
pub fn number_mul(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => Bignum::fx_mul(gc, fx1, fx2),
        (Object::Fixnum(_), Object::Flonum(_)) => todo!(),
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(_), Object::Fixnum(_)) => todo!(),
        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Bignum(_)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(_)) => todo!(),
        (Object::Compnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(_), Object::Flonum(_)) => todo!(),
        (Object::Compnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(_), Object::Bignum(_)) => todo!(),
        (Object::Compnum(_), Object::Compnum(_)) => todo!(),
        _ => todo!(),
    }
}

pub fn number_gt(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f), Object::Fixnum(fl)) => f > fl,
        (Object::Fixnum(f), Object::Flonum(fl)) => todo!(),
        (Object::Fixnum(f), Object::Ratnum(r)) => todo!(),
        (Object::Fixnum(f), Object::Bignum(b)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(c)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(f)) => todo!(),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.gt(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => todo!(),
        (Object::Flonum(_), Object::Compnum(c)) => todo!(),
        (Object::Bignum(b), Object::Fixnum(f)) => todo!(),
        (Object::Bignum(b), Object::Flonum(fl)) => todo!(),
        (Object::Bignum(b), Object::Ratnum(r)) => todo!(),
        (Object::Bignum(b1), Object::Bignum(b2)) => todo!(),
        (Object::Bignum(_), Object::Compnum(c)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(f)) => todo!(),
        (Object::Ratnum(r), Object::Flonum(fl)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => todo!(),
        (Object::Ratnum(r), Object::Bignum(b)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(c)) => todo!(),
        (Object::Compnum(c), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(c), Object::Flonum(_)) => todo!(),
        (Object::Compnum(c), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(c), Object::Bignum(_)) => todo!(),
        (Object::Compnum(c1), Object::Compnum(c2)) => todo!(),
        _ => todo!(),
    }
}
pub fn number_ge(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f), Object::Fixnum(fl)) => f >= fl,
        (Object::Fixnum(f), Object::Flonum(fl)) => todo!(),
        (Object::Fixnum(f), Object::Ratnum(r)) => todo!(),
        (Object::Fixnum(f), Object::Bignum(b)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(c)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(f)) => todo!(),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => todo!(),
        (Object::Flonum(fl), Object::Ratnum(r)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => todo!(),
        (Object::Flonum(_), Object::Compnum(c)) => todo!(),
        (Object::Bignum(b), Object::Fixnum(f)) => todo!(),
        (Object::Bignum(b), Object::Flonum(fl)) => todo!(),
        (Object::Bignum(b), Object::Ratnum(r)) => todo!(),
        (Object::Bignum(b1), Object::Bignum(b2)) => todo!(),
        (Object::Bignum(_), Object::Compnum(c)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(f)) => todo!(),
        (Object::Ratnum(r), Object::Flonum(fl)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => todo!(),
        (Object::Ratnum(r), Object::Bignum(b)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(c)) => todo!(),
        (Object::Compnum(c), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(c), Object::Flonum(_)) => todo!(),
        (Object::Compnum(c), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(c), Object::Bignum(_)) => todo!(),
        (Object::Compnum(c1), Object::Compnum(c2)) => todo!(),
        _ => todo!(),
    }
}

pub fn number_le(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f), Object::Fixnum(fl)) => f <= fl,
        (Object::Fixnum(f), Object::Flonum(fl)) => todo!(),
        (Object::Fixnum(f), Object::Ratnum(r)) => todo!(),
        (Object::Fixnum(f), Object::Bignum(b)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(c)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(f)) => todo!(),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => todo!(),
        (Object::Flonum(fl), Object::Ratnum(r)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => todo!(),
        (Object::Flonum(_), Object::Compnum(c)) => todo!(),
        (Object::Bignum(b), Object::Fixnum(f)) => todo!(),
        (Object::Bignum(b), Object::Flonum(fl)) => todo!(),
        (Object::Bignum(b), Object::Ratnum(r)) => todo!(),
        (Object::Bignum(b1), Object::Bignum(b2)) => todo!(),
        (Object::Bignum(_), Object::Compnum(c)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(f)) => todo!(),
        (Object::Ratnum(r), Object::Flonum(fl)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => todo!(),
        (Object::Ratnum(r), Object::Bignum(b)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(c)) => todo!(),
        (Object::Compnum(c), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(c), Object::Flonum(_)) => todo!(),
        (Object::Compnum(c), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(c), Object::Bignum(_)) => todo!(),
        (Object::Compnum(c1), Object::Compnum(c2)) => todo!(),
        _ => todo!(),
    }
}
pub fn number_eq(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f), Object::Fixnum(fl)) => f == fl,
        (Object::Fixnum(f), Object::Flonum(fl)) => fl.fx_eq(f),
        (Object::Fixnum(f), Object::Ratnum(r)) => r.fx_eq(f),
        (Object::Fixnum(f), Object::Bignum(b)) => b.fx_eq(f),
        (Object::Fixnum(_), Object::Compnum(c)) => c.obj_eq(n1),
        (Object::Flonum(fl), Object::Fixnum(f)) => fl.fx_eq(f),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.eq(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => r.fl_eq(&fl),
        (Object::Flonum(fl), Object::Bignum(b)) => b.fl_eq(&fl),
        (Object::Flonum(_), Object::Compnum(c)) => c.obj_eq(n1),
        (Object::Bignum(b), Object::Fixnum(f)) => b.fx_eq(f),
        (Object::Bignum(b), Object::Flonum(fl)) => b.fl_eq(&fl),
        (Object::Bignum(b), Object::Ratnum(r)) => r.bi_eq(&b),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.eq(&b2),
        (Object::Bignum(_), Object::Compnum(c)) => c.obj_eq(n1),
        (Object::Ratnum(r), Object::Fixnum(f)) => r.fx_eq(f),
        (Object::Ratnum(r), Object::Flonum(fl)) => r.fl_eq(&fl),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.eq(&r2),
        (Object::Ratnum(r), Object::Bignum(b)) => r.bi_eq(&b),
        (Object::Ratnum(_), Object::Compnum(c)) => c.obj_eq(n1),
        (Object::Compnum(c), Object::Fixnum(_)) => c.obj_eq(n2),
        (Object::Compnum(c), Object::Flonum(_)) => c.obj_eq(n2),
        (Object::Compnum(c), Object::Ratnum(_)) => c.obj_eq(n2),
        (Object::Compnum(c), Object::Bignum(_)) => c.obj_eq(n2),
        (Object::Compnum(c1), Object::Compnum(c2)) => c1.eq(&c2),
        _ => todo!(),
    }
}

pub fn number_lt(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(f), Object::Fixnum(fl)) => f < fl,
        (Object::Fixnum(f), Object::Flonum(fl)) => fl.fx_lt(f),
        (Object::Fixnum(f), Object::Ratnum(r)) => r.fx_lt(f),
        (Object::Fixnum(f), Object::Bignum(b)) => b.fx_lt(f),
        (Object::Flonum(fl), Object::Fixnum(f)) => fl.fx_lt(f),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.lt(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => r.fl_lt(&fl),
        (Object::Flonum(fl), Object::Bignum(b)) => b.fl_lt(&fl),
        (Object::Bignum(b), Object::Fixnum(f)) => b.fx_lt(f),
        (Object::Bignum(b), Object::Flonum(fl)) => b.fl_lt(&fl),
        (Object::Bignum(b), Object::Ratnum(r)) => r.bi_lt(&b),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.lt(&b2),
        (Object::Ratnum(r), Object::Fixnum(f)) => r.fx_lt(f),
        (Object::Ratnum(r), Object::Flonum(fl)) => r.fl_lt(&fl),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.lt(&r2),
        (Object::Ratnum(r), Object::Bignum(b)) => r.bi_lt(&b),
        _ => todo!(),
    }
}

pub fn exp(_gc: &mut Box<Gc>, n: Object) -> Object {
    match n {
        Object::Flonum(fl) => Object::Flonum(Flonum::new(fl.value().exp())),
        _ => todo!(),
    }
}

pub fn expt(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    assert!(!n2.is_bignum());
    match (n1, n2) {
        (Object::Fixnum(f1), Object::Fixnum(f2)) => match (BigInt::from_isize(f1), f2 as u32) {
            (Some(b1), b2) => {
                let b = b1.pow(b2);
                match b.to_isize() {
                    Some(v) => Object::Fixnum(v),
                    None => Object::Bignum(gc.alloc(Bignum::new(b))),
                }
            }
            _ => todo!(),
        },
        (Object::Fixnum(_), Object::Flonum(_)) => todo!(),
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(_), Object::Fixnum(_)) => todo!(),
        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Ratnum(_), Object::Bignum(_)) => todo!(),
        (Object::Ratnum(_), Object::Compnum(_)) => todo!(),
        (Object::Compnum(_), Object::Fixnum(_)) => todo!(),
        (Object::Compnum(_), Object::Flonum(_)) => todo!(),
        (Object::Compnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Compnum(_), Object::Bignum(_)) => todo!(),
        (Object::Compnum(_), Object::Compnum(_)) => todo!(),
        _ => todo!(),
    }
}

fn fx_log(n: isize) -> Object {
    if n == 1 {
        // Exact 0.
        Object::Fixnum(0)
    } else {
        Object::Flonum(Flonum::new((n as f64).ln()))
    }
}

pub fn log(n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx_log(fx),
        Object::Compnum(_) => todo!(),
        _ => todo!(),
    }
}

pub fn abs(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_real());
    match n {
        Object::Fixnum(fx) => Object::Fixnum(fx.abs()),
        Object::Flonum(fl) => fl.abs(),
        Object::Bignum(b) => b.abs(gc),
        Object::Ratnum(r) => r.abs(gc),
        _ => panic!(),
    }
}

fn inexact(gc: &mut Box<Gc>, obj: Object) -> Object {
    assert!(obj.is_number());
    match obj {
        Object::Fixnum(f) => match f.to_f64() {
            Some(v) => Object::Flonum(Flonum::new(v)),
            None => todo!(),
        },
        Object::Bignum(b) => match b.to_f64() {
            Some(v) => Object::Flonum(Flonum::new(v)),
            None => todo!(),
        },
        Object::Flonum(_) => obj,
        Object::Ratnum(r) => match r.to_f64() {
            Some(v) => Object::Flonum(Flonum::new(v)),
            None => todo!(),
        },
        Object::Compnum(c) => {
            let real = inexact(gc, c.real);
            let imag = inexact(gc, c.imag);
            Object::Compnum(gc.alloc(Compnum::new(real, imag)))
        }
        _ => todo!(),
    }
}

fn denominator(gc: &mut Box<Gc>, obj: Object) -> Object {
    assert!(obj.is_rational());
    match obj {
        Object::Ratnum(r) => r.denom(),
        Object::Flonum(fl) => {
            let m = fl.to_exact(gc);
            let denom = denominator(gc, m);
            inexact(gc, denom)
        }
        _ => Object::Fixnum(1),
    }
}

// http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_448
fn is_integer(gc: &mut Box<Gc>, obj: Object) -> bool {
    assert!(obj.is_number());
    match obj {
        Object::Flonum(f) if f.is_nan() || f.is_infinite() => false,
        Object::Compnum(c) => c.imag.is_exact_zero() && c.real.is_integer(gc),
        _ => number_eq(denominator(gc, obj), Object::Fixnum(1)),
    }
}

impl Object {
    #[inline(always)]
    pub fn is_exact(&self) -> bool {
        assert!(self.is_number());
        match self {
            Object::Fixnum(_) | Object::Bignum(_) | Object::Ratnum(_) => true,
            Object::Flonum(_) => false,
            Object::Compnum(c) => c.real.is_exact() && c.imag.is_exact(),
            _ => todo!(),
        }
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
    pub fn is_integer(&self, gc: &mut Box<Gc>) -> bool {
        self.is_fixnum() || self.is_bignum() || (self.is_number() && is_integer(gc, *self))
    }

    #[inline(always)]
    pub fn is_real(&self) -> bool {
        self.is_fixnum()
            || self.is_bignum()
            || self.is_flonum()
            || self.is_ratnum()
            || (self.is_compnum() && self.to_compnum().is_real())
    }

    #[inline(always)]
    pub fn is_rational(&self) -> bool {
        self.is_fixnum()
            || self.is_bignum()
            || (self.is_flonum() && self.to_flonum().is_rational())
            || self.is_ratnum()
    }
    #[inline(always)]
    pub fn is_exact_zero(&self) -> bool {
        self.is_zero() && self.is_exact()
    }

    #[inline(always)]
    fn is_zero(&self) -> bool {
        number_eq(Object::Fixnum(0), *self)
    }
}
