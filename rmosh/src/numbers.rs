use std::{
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut, Neg, Rem},
};

use num_bigint::BigInt;
use num_rational::Rational64;
use num_traits::{FromPrimitive, Signed, ToPrimitive};

use crate::{
    gc::{Gc, GcHeader, GcRef, ObjectType},
    objects::Object,
};

// Fixnum.
trait FixnumExt {
    // Fixnum vs Fixnum
    fn add(self, gc: &mut Box<Gc>, fx: isize) -> Object;
    fn sub(self, gc: &mut Box<Gc>, fx: isize) -> Object;
    fn mul(self, gc: &mut Box<Gc>, fx: isize) -> Object;
    fn div(self, gc: &mut Box<Gc>, fx: isize) -> Result<Object, SchemeError>;
    fn integer_div(self, fx: isize) -> Result<Object, SchemeError>;

    // Fixnum vs Flonum
    fn add_fl(self, fl: &Flonum) -> Object;
    fn sub_fl(self, fl: &Flonum) -> Object;
    fn mul_fl(self, fl: &Flonum) -> Object;
    fn div_fl(self, fl: &Flonum) -> Result<Object, SchemeError>;
    fn eqv_fl(self, fl: &Flonum) -> bool;
    fn lt_fl(self, fl: &Flonum) -> bool;
    fn gt_fl(self, fl: &Flonum) -> bool;

    // Fixnum vs Bignum
    fn add_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object;
    fn sub_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object;
    fn mul_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object;
    fn eqv_big(self, b: &Bignum) -> bool;
    fn lt_big(self, b: &Bignum) -> bool;
    fn gt_big(self, b: &Bignum) -> bool;

    // Fixnum vs Ratnum
    fn add_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object;
    fn sub_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object;
    fn mul_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object;
    fn div_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Result<Object, SchemeError>;
    fn eqv_rat(self, r: &Ratnum) -> bool;
    fn lt_rat(self, r: &Ratnum) -> bool;
    fn gt_rat(self, r: &Ratnum) -> bool;

    // Arith
    fn exp(self) -> Object;
    fn log(self) -> Object;
    fn cos(self) -> Object;
    fn sin(self) -> Object;
    fn tan(self) -> Object;
    fn asin(self) -> Object;
    fn acos(self) -> Object;
    fn atan(self) -> Object;
    fn sqrt(self, gc: &mut Box<Gc>) -> Object;
}
impl FixnumExt for isize {
    // Fixnum vs Fixnum
    fn add(self, gc: &mut Box<Gc>, fx: isize) -> Object {
        match self.checked_add(fx) {
            Some(value) => Object::Fixnum(value),
            None => {
                let b1 = BigInt::from_isize(self).unwrap();
                let b2 = BigInt::from_isize(fx).unwrap();
                let b = b1 + b2;
                Object::Bignum(gc.alloc(Bignum::new(b)))
            }
        }
    }
    fn sub(self, gc: &mut Box<Gc>, fx: isize) -> Object {
        match self.checked_sub(fx) {
            Some(value) => Object::Fixnum(value),
            None => {
                let b1 = BigInt::from_isize(self).unwrap();
                let b2 = BigInt::from_isize(fx).unwrap();
                let b = b1 - b2;
                Object::Bignum(gc.alloc(Bignum::new(b)))
            }
        }
    }
    fn mul(self, gc: &mut Box<Gc>, fx: isize) -> Object {
        match self.checked_mul(fx) {
            Some(value) => Object::Fixnum(value),
            None => {
                let b1 = BigInt::from_isize(self).unwrap();
                let b2 = BigInt::from_isize(fx).unwrap();
                let b = b1 * b2;
                Object::Bignum(gc.alloc(Bignum::new(b)))
            }
        }
    }
    fn div(self, gc: &mut Box<Gc>, fx: isize) -> Result<Object, SchemeError> {
        if fx == 0 {
            Err(SchemeError::Div0)
        } else if fx == 1 {
            Ok(Object::Fixnum(self))
        } else {
            let r = Ratnum::new(self, fx);
            if r.is_integer() {
                Ok(r.numer())
            } else {
                Ok(Object::Ratnum(gc.alloc(r)))
            }
        }
    }
    fn integer_div(self, fx: isize) -> Result<Object, SchemeError> {
        if fx == 0 {
            return Err(SchemeError::Div0);
        }
        let ret;
        if self == 0 {
            ret = 0;
        } else if self > 0 {
            ret = self / fx;
        } else if fx > 0 {
            ret = (self - fx + 1) / fx;
        } else {
            ret = (self + fx + 1) / fx;
        }
        Ok(Object::Fixnum(ret))
    }

    // Fixnum vs Flonum
    fn add_fl(self, fl: &Flonum) -> Object {
        let f = (self as f64) + fl.value();
        Object::Flonum(Flonum::new(f))
    }
    fn sub_fl(self, fl: &Flonum) -> Object {
        let f = (self as f64) - fl.value();
        Object::Flonum(Flonum::new(f))
    }
    fn mul_fl(self, fl: &Flonum) -> Object {
        let f = (self as f64) * fl.value();
        Object::Flonum(Flonum::new(f))
    }
    fn div_fl(self, fl: &Flonum) -> Result<Object, SchemeError> {
        let f = (self as f64) / fl.value();
        Ok(Object::Flonum(Flonum::new(f)))
    }
    fn eqv_fl(self, fl: &Flonum) -> bool {
        // No data loss.
        if (self as f64) as isize == self {
            self as f64 == fl.value()
        } else {
            match (BigInt::from_f64(fl.value()), BigInt::from_isize(self)) {
                (Some(b1), Some(b2)) => b1 == b2,
                _ => false,
            }
        }
    }
    fn lt_fl(self, fl: &Flonum) -> bool {
        (self as f64) < fl.value()
    }
    fn gt_fl(self, fl: &Flonum) -> bool {
        (self as f64) > fl.value()
    }

    // Fixnum vs Bignum
    fn add_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object {
        let other = BigInt::from_isize(self).unwrap();
        let result = b.value.clone() + other;
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }
    fn sub_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object {
        let other = BigInt::from_isize(self).unwrap();
        let result = b.value.clone() - other;
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }
    fn mul_big(self, gc: &mut Box<Gc>, b: &Bignum) -> Object {
        let other = BigInt::from_isize(self).unwrap();
        let result = b.value.clone() * other;
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }
    fn eqv_big(self, b: &Bignum) -> bool {
        match b.to_isize() {
            Some(v) => v == self,
            None => false,
        }
    }
    fn lt_big(self, b: &Bignum) -> bool {
        match b.to_isize() {
            Some(v) => self < v,
            None => match BigInt::from_isize(self) {
                Some(self_value) => self_value < b.value,
                None => panic!(),
            },
        }
    }
    fn gt_big(self, b: &Bignum) -> bool {
        match b.to_isize() {
            Some(v) => self > v,
            None => match BigInt::from_isize(self) {
                Some(self_value) => self_value > b.value,
                None => panic!(),
            },
        }
    }

    // Fixnum vs Ratnum
    fn add_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object {
        let r = r.ratio + Rational64::new_raw(self as i64, 1);
        if r.is_integer() {
            Object::Fixnum(*r.numer() as isize)
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }
    fn sub_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object {
        let r = r.ratio - Rational64::new_raw(self as i64, 1);
        if r.is_integer() {
            Object::Fixnum(*r.numer() as isize)
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }
    fn mul_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Object {
        let r = r.ratio * Rational64::new_raw(self as i64, 1);
        if r.is_integer() {
            Object::Fixnum(*r.numer() as isize)
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }
    fn div_rat(self, gc: &mut Box<Gc>, r: &Ratnum) -> Result<Object, SchemeError> {
        let r = Rational64::new_raw(self as i64, 1) / r.ratio;
        if r.is_integer() {
            Ok(Object::Fixnum(*r.numer() as isize))
        } else {
            Ok(Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r))))
        }
    }
    fn eqv_rat(self, r: &Ratnum) -> bool {
        match Rational64::from_isize(self) {
            Some(self_r) => self_r == r.ratio,
            None => false,
        }
    }
    fn lt_rat(self, r: &Ratnum) -> bool {
        match Rational64::from_isize(self) {
            Some(self_r) => self_r < r.ratio,
            None => todo!(),
        }
    }
    fn gt_rat(self, r: &Ratnum) -> bool {
        match Rational64::from_isize(self) {
            Some(self_r) => self_r > r.ratio,
            None => todo!(),
        }
    }

    // Arith
    fn exp(self) -> Object {
        if self == 0 {
            // Exact 1.
            Object::Fixnum(1)
        } else {
            Object::Flonum(Flonum::new((self as f64).exp()))
        }
    }
    fn log(self) -> Object {
        if self == 1 {
            // Exact 0.
            Object::Fixnum(0)
        } else {
            Object::Flonum(Flonum::new((self as f64).ln()))
        }
    }
    fn cos(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).cos()))
    }
    fn sin(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).sin()))
    }
    fn tan(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).tan()))
    }
    fn asin(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).asin()))
    }
    fn acos(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).acos()))
    }
    fn atan(self) -> Object {
        Object::Flonum(Flonum::new((self as f64).atan()))
    }
    fn sqrt(self, gc: &mut Box<Gc>) -> Object {
        if self == 0 {
            return Object::Fixnum(0);
        } else if self > 0 {
            let root = (self as f64).sqrt();
            let root_as_int = root.floor() as isize;
            // exact
            if root_as_int * root_as_int == self {
                Object::Fixnum(root_as_int)
            } else {
                Object::Flonum(Flonum::new(root))
            }
        } else {
            // negative
            let root = (-self as f64).sqrt();
            let root_as_int = root.floor() as isize;
            // exact
            if root_as_int * root_as_int == -self {
                Object::Compnum(
                    gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(root_as_int))),
                )
            } else {
                Object::Compnum(gc.alloc(Compnum::new(
                    Object::Flonum(Flonum::new(0.0)),
                    Object::Flonum(Flonum::new(root)),
                )))
            }
        }
    }
}

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
    pub fn is_even(&self) -> bool {
        self.value() * 0.5 == (self.value() * 0.5).floor()
    }

    #[inline(always)]
    pub fn add(&self, other: &Flonum) -> Object {
        Object::Flonum(Flonum::new(self.value() + other.value()))
    }

    #[inline(always)]
    pub fn sub(&self, other: &Flonum) -> Object {
        Object::Flonum(Flonum::new(self.value() - other.value()))
    }

    #[inline(always)]
    pub fn mul(&self, other: &Flonum) -> Object {
        Object::Flonum(Flonum::new(self.value() * other.value()))
    }

    #[inline(always)]
    pub fn div(&self, other: &Flonum) -> Result<Object, SchemeError> {
        Ok(Object::Flonum(Flonum::new(self.value() / other.value())))
    }

    pub fn integer_div(&self, other: &Flonum) -> Result<Object, SchemeError> {
        let ret = if other.value() > 0.0 {
            (self.value() / other.value()).floor()
        } else {
            (self.value() / (-other.value())).floor()
        };
        Ok(Object::Flonum(Flonum::new(ret)))
    }

    #[inline(always)]
    pub fn eqv(&self, other: &Flonum) -> bool {
        self.value() == other.value()
    }
    #[inline(always)]
    pub fn gt(&self, other: &Flonum) -> bool {
        self.value() > other.value()
    }

    // Flonum vs Fixnum.
    pub fn sub_fx(&self, fx: isize) -> Object {
        let value = self.value() - (fx as f64);
        Object::Flonum(Flonum::new(value))
    }
    pub fn div_fx(&self, fx: isize) -> Result<Object, SchemeError> {
        if fx == 0 {
            Err(SchemeError::Div0)
        } else if fx == 1 {
            Ok(Object::Flonum(Flonum::new(self.value())))
        } else {
            let value = self.value() / (fx as f64);
            Ok(Object::Flonum(Flonum::new(value)))
        }
    }

    // Flonum vs Ratnum
    pub fn add_rat(&self, r: &GcRef<Ratnum>) -> Object {
        match r.to_f64() {
            Some(rv) => Object::Flonum(Flonum::new(self.value() + rv)),
            None => todo!(),
        }
    }
    pub fn sub_rat(&self, r: &GcRef<Ratnum>) -> Object {
        match r.to_f64() {
            Some(rv) => Object::Flonum(Flonum::new(self.value() - rv)),
            None => todo!(),
        }
    }
    pub fn eqv_rat(&self, r: &GcRef<Ratnum>) -> bool {
        match r.to_f64() {
            Some(v) => v == self.value(),
            None => false,
        }
    }
    pub fn lt_rat(&self, r: &GcRef<Ratnum>) -> bool {
        match r.ratio.to_f64() {
            Some(rv) => self.value() < rv,
            None => false,
        }
    }
    pub fn gt_rat(&self, r: &GcRef<Ratnum>) -> bool {
        match r.ratio.to_f64() {
            Some(rv) => self.value() > rv,
            None => false,
        }
    }

    // Flonum vs Bignum
    pub fn eqv_big(&self, b: &GcRef<Bignum>) -> bool {
        match BigInt::from_f64(self.value()) {
            Some(b2) => b2 == b.value,
            None => false,
        }
    }
    pub fn lt_big(&self, b: &Bignum) -> bool {
        match (self.value(), b.to_f64()) {
            (l, Some(r)) => l < r,
            _ => false,
        }
    }
    pub fn gt_big(&self, b: &Bignum) -> bool {
        match (self.value(), b.to_f64()) {
            (l, Some(r)) => l > r,
            _ => false,
        }
    }

    #[inline(always)]
    pub fn ceiling(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().ceil()))
    }

    #[inline(always)]
    pub fn floor(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().floor()))
    }

    #[inline(always)]
    pub fn round(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().round()))
    }

    #[inline(always)]
    pub fn sqrt(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().sqrt()))
    }

    #[inline(always)]
    pub fn truncate(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().trunc()))
    }

    #[inline(always)]
    pub fn abs(&self) -> Object {
        Object::Flonum(Flonum::new(self.value().abs()))
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
        write!(f, "{:e}", self.value())
    }
}

impl Debug for Flonum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:e}", self.value())
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
            ratio: Rational64::new_raw(numer as i64, denom as i64).reduced(),
        }
    }
    pub fn new_from_ratio(ratio: Rational64) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Ratnum),
            ratio: ratio,
        }
    }

    pub fn denom(&self) -> Object {
        Object::Fixnum(*self.ratio.denom() as isize)
    }

    pub fn numer(&self) -> Object {
        Object::Fixnum(*self.ratio.numer() as isize)
    }

    pub fn add(&self, gc: &mut Box<Gc>, other: &GcRef<Ratnum>) -> Object {
        let r = self.ratio + other.ratio;
        if r.is_integer() {
            Object::Fixnum(r.to_isize().unwrap())
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }

    pub fn sub(&self, gc: &mut Box<Gc>, other: &GcRef<Ratnum>) -> Object {
        let r = self.ratio - other.ratio;
        if r.is_integer() {
            Object::Fixnum(r.to_isize().unwrap())
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }
    pub fn mul(&self, gc: &mut Box<Gc>, other: &GcRef<Ratnum>) -> Object {
        let r = self.ratio * other.ratio;
        if r.is_integer() {
            Object::Fixnum(r.to_isize().unwrap())
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }

    pub fn eqv(&self, other: &Ratnum) -> bool {
        self.ratio.eq(&other.ratio)
    }

    // Ratnum vs Fixnum
    pub fn sub_fx(&self, gc: &mut Box<Gc>, fx: isize) -> Object {
        let r = self.ratio - Rational64::new_raw(fx as i64, 1);
        if r.is_integer() {
            Object::Fixnum(*r.numer() as isize)
        } else {
            Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(r)))
        }
    }

    pub fn div_fx(&self, gc: &mut Box<Gc>, fx: isize) -> Result<Object, SchemeError> {
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

    pub fn abs(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.abs())))
    }

    pub fn ceiling(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.ceil())))
    }

    pub fn floor(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.floor())))
    }

    pub fn round(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.round())))
    }

    pub fn truncate(&self, gc: &mut Box<Gc>) -> Object {
        Object::Ratnum(gc.alloc(Ratnum::new_from_ratio(self.ratio.trunc())))
    }
}

impl Display for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ratio)
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

    pub fn sub(&self, gc: &mut Box<Gc>, other: &Bignum) -> Object {
        let result = self.value.clone() - other.value.clone();
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }
    pub fn mul(&self, gc: &mut Box<Gc>, other: &Bignum) -> Object {
        let result = self.value.clone() * other.value.clone();
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }

    pub fn eqv(&self, other: &Bignum) -> bool {
        self.value.eq(&other.value)
    }

    // Bignum vs Fixnum

    pub fn sub_fx(&self, gc: &mut Box<Gc>, fx: isize) -> Object {
        let other = BigInt::from_isize(fx).unwrap();
        let result = self.value.clone() - other;
        match result.to_isize() {
            Some(v) => Object::Fixnum(v),
            None => Object::Bignum(gc.alloc(Bignum::new(result))),
        }
    }

    // Bignum vs Ratnum.
    pub fn eqv_rat(&self, r: &Ratnum) -> bool {
        match (self.to_f64(), r.to_f64()) {
            (Some(l), Some(r)) => l == r,
            _ => false,
        }
    }
    pub fn lt_rat(&self, r: &Ratnum) -> bool {
        match (self.to_f64(), r.to_f64()) {
            (Some(l), Some(r)) => l < r,
            _ => false,
        }
    }
    pub fn gt_rat(&self, r: &Ratnum) -> bool {
        match (self.to_f64(), r.to_f64()) {
            (Some(l), Some(r)) => l > r,
            _ => false,
        }
    }

    pub fn is_even(&self) -> bool {
        let r = (self.value.clone() % BigInt::from(2)).to_isize().unwrap();
        r == 0
    }

    pub fn abs(&self, gc: &mut Box<Gc>) -> Object {
        Object::Bignum(gc.alloc(Bignum::new(self.value.abs())))
    }

    pub fn sqrt(&self, gc: &mut Box<Gc>) -> Object {
        if self.is_positive() {
            let b = self.value.sqrt();
            match b.to_isize() {
                Some(v) => Object::Fixnum(v),
                None => Object::Bignum(gc.alloc(Bignum::new(b))),
            }
        } else {
            let b = self.value.clone().neg();
            let b = b.sqrt();
            match b.to_isize() {
                Some(v) => {
                    Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(v))))
                }
                None => {
                    let b = gc.alloc(Bignum::new(b));
                    Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Bignum(b))))
                }
            }
        }
    }
}

impl Display for Bignum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Debug for Bignum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
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
    pub fn eqv(&self, other: &Compnum) -> bool {
        eqv(self.real, other.real) && eqv(other.imag, other.imag)
    }

    pub fn is_real(&self) -> bool {
        eqv(self.imag, Object::Fixnum(0)) && self.imag.is_exact()
    }

    pub fn eqv_real(&self, n: Object) -> bool {
        assert!(n.is_fixnum() || n.is_bignum() || n.is_flonum() || n.is_ratnum());
        eqv(self.imag, Object::Fixnum(0)) && eqv(self.real, n)
    }
    pub fn mul_real(&self, gc: &mut Box<Gc>, n: Object) -> Object {
        assert!(n.is_fixnum() || n.is_bignum() || n.is_flonum() || n.is_ratnum());
        let real = mul(gc, self.real, n);
        let imag = mul(gc, self.imag, n);
        Object::Compnum(gc.alloc(Compnum::new(real, imag)))
    }

    pub fn exp(&self, gc: &mut Box<Gc>) -> Object {
        let real = real_to_f64(self.real);
        let imag = real_to_f64(self.imag);
        let r = real.exp();

        let re = Object::Flonum(Flonum::new(r * imag.cos()));
        let im = Object::Flonum(Flonum::new(r * imag.sin()));
        Object::Compnum(gc.alloc(Compnum::new(re, im)))
    }

    pub fn cos(&self, gc: &mut Box<Gc>) -> Object {
        // cos(iy) = (e^-y + e^y) / 2
        // sin(iy) = (e^-y - e^y) / 2i
        // cos(z)  = cos(x+iy) = cos(x)cos(iy) - sin(x)sin(iy)
        //         = cos(x) * (e^-y + e^y) / 2 - sin(x) * (e^-y - e^y) / 2i
        //         = cos(x) * (e^-y + e^y) / 2 + sin(x) * ((e^-y - e^y) / 2) * i
        let real = real_to_f64(self.real);
        let imag = real_to_f64(self.imag);
        let a = imag.exp();
        let b = 1.0 / a;
        let im = Object::Flonum(Flonum::new(real.cos() * (b + a) * 0.5));
        let re = Object::Flonum(Flonum::new(real.sin() * (b - a) * 0.5));
        Object::Compnum(gc.alloc(Compnum::new(re, im)))
    }

    pub fn sin(&self, gc: &mut Box<Gc>) -> Object {
        // cos(iy) = (e^-y + e^y) / 2
        // sin(iy) = (e^-y - e^y) / 2i
        // sin(z)  = sin(x+iy) = sin(x)cos(iy) + sin(iy)cos(x)
        //         = sin(x) * ((e^-y + e^y) / 2) + cos(x) * ((e^-y - e^y) / 2i)
        //         = sin(x) * ((e^-y + e^y) / 2) + cos(x) * ((e^y - e^-y) / 2) * i
        let real = real_to_f64(self.real);
        let imag = real_to_f64(self.imag);
        let a = imag.exp();
        let b = 1.0 / a;
        let im = Object::Flonum(Flonum::new(real.sin() * (b + a) * 0.5));
        let re = Object::Flonum(Flonum::new(real.cos() * (a - b) * 0.5));
        Object::Compnum(gc.alloc(Compnum::new(re, im)))
    }
    pub fn tan(&self, gc: &mut Box<Gc>) -> Result<Object, SchemeError> {
        let lhs = self.sin(gc);
        let rhs = self.cos(gc);
        div(gc, lhs, rhs)
    }

    pub fn asin(gc: &mut Box<Gc>, n: Object) -> Object {
        assert!(n.is_compnum());
        let square = mul(gc, n, n);
        let a = sub(gc, Object::Fixnum(1), square);
        let b = sqrt(gc, a);
        let c = Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(1))));
        let c = mul(gc, c, n);
        let d = add(gc, b, c);
        let d = log(gc, d);
        let e = Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(-1))));
        mul(gc, e, d)
    }

    // arccos(z) = -i * log(z + sqrt(1-z*z)i)
    pub fn acos(gc: &mut Box<Gc>, n: Object) -> Object {
        assert!(n.is_compnum());
        let square = mul(gc, n, n);
        let a = sub(gc, Object::Fixnum(1), square);
        let b = sqrt(gc, a);
        let c = Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(1))));
        let c = mul(gc, c, b);
        let d = add(gc, n, c);
        let d = log(gc, d);
        let e = Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(-1))));
        mul(gc, e, d)
    }

    // atan(z) = (i/2)*log((i+z)/(i-z))
    pub fn atan(gc: &mut Box<Gc>, n: Object) -> Result<Object, SchemeError> {
        assert!(n.is_compnum());
        let square = mul(gc, n, n);
        let a = Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), Object::Fixnum(1))));
        let b = add(gc, a, n);
        let c = sub(gc, a, n);
        let d = div(gc, b, c)?;
        let d = log(gc, d);
        let e = div(gc, Object::Fixnum(1), Object::Fixnum(2))?;
        Object::Compnum(gc.alloc(Compnum::new(Object::Fixnum(0), e)));
        Ok(mul(gc, e, d))
    }

    // \sqrt{r}e^{\frac{i\theta}{2}}
    pub fn sqrt(&self, gc: &mut Box<Gc>) -> Object {
        let real = real_to_f64(self.real);
        let imag = real_to_f64(self.imag);
        if imag == 0.0 {
            if real >= 0.0 {
                Object::Flonum(Flonum::new(real.sqrt()))
            } else {
                let re = Object::Flonum(Flonum::new(0.0));
                let im = Object::Flonum(Flonum::new(real.abs().sqrt()));
                Object::Compnum(gc.alloc(Compnum::new(re, im)))
            }
        } else {
            let r = (real * real + imag * imag).sqrt();
            let theta = f64::atan2(imag, real);

            let c = Object::Compnum(gc.alloc(Compnum::new(
                Object::Fixnum(0),
                Object::Flonum(Flonum::new(0.5 * theta)),
            )));
            let c = exp(gc, c);
            mul(gc, Object::Flonum(Flonum::new(r.sqrt())), c)
        }
    }

    pub fn magnitude(&self, gc: &mut Box<Gc>) -> Object {
        let x = mul(gc, self.real, self.real);
        let y = mul(gc, self.imag, self.imag);
        let z = add(gc, x, y);
        sqrt(gc, z)
    }

    // theta = atan2(imaginary, real)
    pub fn angle(&self) -> Object {
        let re = real_to_f64(self.real);
        let im = real_to_f64(self.imag);
        Object::Flonum(Flonum::new(f64::atan2(im, re)))
    }
}
impl Display for Compnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}+/-{}i", self.real.to_string(), self.imag.to_string())
    }
}

/// Number functions.
pub fn add(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => fx1.add(gc, fx2),
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.add_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.add_rat(gc, &r),
        (Object::Fixnum(fx), Object::Bignum(b)) => fx.add_big(gc, &b),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fx.add_fl(&fl),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.add(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => fl.add_rat(&r),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(b), Object::Fixnum(fx)) => fx.add_big(gc, &b),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => fx.add_rat(gc, &r),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.add(gc, &r2),
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
pub fn sub(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => fx1.sub(gc, fx2),
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.sub_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.sub_rat(gc, &r),
        (Object::Fixnum(fx), Object::Bignum(b)) => fx.sub_big(gc, &b),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fl.sub_fx(fx),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.sub(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => fl.sub_rat(&r),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(b), Object::Fixnum(fx)) => b.sub_fx(gc, fx),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.sub(gc, &b2),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => r.sub_fx(gc, fx),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.sub(gc, &r2),
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

pub fn mul(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => fx1.mul(gc, fx2),
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.mul_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.mul_rat(gc, &r),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(c)) => c.mul_real(gc, n1),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fx.mul_fl(&fl),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.mul(&fl2),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.mul(gc, &b2),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => fx.mul_rat(gc, &r),
        (Object::Ratnum(_), Object::Flonum(_)) => todo!(),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.mul(gc, &r2),
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
    NonZeroRequired,
    NanOrInfinite,
}
pub fn div(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => fx1.div(gc, fx2),
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.div_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.div_rat(gc, &r),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fl.div_fx(fx),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.div(&fl2),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(_), Object::Bignum(_)) => todo!(),
        (Object::Flonum(_), Object::Compnum(_)) => todo!(),
        (Object::Bignum(_), Object::Fixnum(_)) => todo!(),
        (Object::Bignum(_), Object::Flonum(_)) => todo!(),
        (Object::Bignum(_), Object::Ratnum(_)) => todo!(),
        (Object::Bignum(_), Object::Bignum(_)) => todo!(),
        (Object::Bignum(_), Object::Compnum(_)) => todo!(),
        (Object::Ratnum(r), Object::Fixnum(fx)) => r.div_fx(gc, fx),
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

pub fn integer_div(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_real());
    assert!(n2.is_real());

    if n1.is_flonum() {
        let f = n1.to_flonum();
        if f.is_infinite() || f.is_nan() {
            return Err(SchemeError::NanOrInfinite);
        }
    }

    if n2.is_fixnum() {
        let fx = n2.to_isize();
        if fx == 0 {
            return Err(SchemeError::Div0);
        }
    }

    if n2.is_flonum() {
        let f = n2.to_flonum();
        if 0.0 == f.value() {
            return Err(SchemeError::Div0);
        }
    }

    if n1.is_fixnum() && n2.is_fixnum() {
        return n1.to_isize().integer_div(n2.to_isize());
    } else if n1.is_flonum() && n2.is_flonum() {
        n1.to_flonum().integer_div(&n2.to_flonum())
    } else {
        let ret;
        if n2.is_negative() {
            let r = negate(gc, n2);
            let r = div(gc, n1, r)?;
            let r = floor(gc, r);
            ret = negate(gc, r);
        } else {
            let r = div(gc, n1, n2)?;
            ret = floor(gc, r);
        }
        return Ok(ret);
    }
}

pub fn eqv(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => fx1 == fx2,
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.eqv_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.eqv_rat(&r),
        (Object::Fixnum(fx), Object::Bignum(b)) => fx.eqv_big(&b),
        (Object::Fixnum(_), Object::Compnum(c)) => c.eqv_real(n1),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fx.eqv_fl(&fl),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.eqv(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => fl.eqv_rat(&r),
        (Object::Flonum(fl), Object::Bignum(b)) => fl.eqv_big(&b),
        (Object::Flonum(_), Object::Compnum(c)) => c.eqv_real(n1),
        (Object::Bignum(b), Object::Fixnum(fx)) => fx.eqv_big(&b),
        (Object::Bignum(b), Object::Flonum(fl)) => fl.eqv_big(&b),
        (Object::Bignum(b), Object::Ratnum(r)) => b.eqv_rat(&r),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.eqv(&b2),
        (Object::Bignum(_), Object::Compnum(c)) => c.eqv_real(n1),
        (Object::Ratnum(r), Object::Fixnum(fx)) => fx.eqv_rat(&r),
        (Object::Ratnum(r), Object::Flonum(fl)) => fl.eqv_rat(&r),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.eqv(&r2),
        (Object::Ratnum(r), Object::Bignum(b)) => b.eqv_rat(&r),
        (Object::Ratnum(_), Object::Compnum(c)) => c.eqv_real(n1),
        (Object::Compnum(c), Object::Fixnum(_)) => c.eqv_real(n2),
        (Object::Compnum(c), Object::Flonum(_)) => c.eqv_real(n2),
        (Object::Compnum(c), Object::Ratnum(_)) => c.eqv_real(n2),
        (Object::Compnum(c), Object::Bignum(_)) => c.eqv_real(n2),
        (Object::Compnum(c1), Object::Compnum(c2)) => c1.eqv(&c2),
        _ => todo!(),
    }
}

pub fn lt(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx), Object::Fixnum(fl)) => fx < fl,
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.lt_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.lt_rat(&r),
        (Object::Fixnum(fx), Object::Bignum(b)) => fx.lt_big(&b),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fx.gt_fl(&fl),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.lt(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => fl.lt_rat(&r),
        (Object::Flonum(fl), Object::Bignum(b)) => fl.lt_big(&b),
        (Object::Bignum(b), Object::Fixnum(fx)) => fx.gt_big(&b),
        (Object::Bignum(b), Object::Flonum(fl)) => fl.gt_big(&b),
        (Object::Bignum(b), Object::Ratnum(r)) => b.lt_rat(&r),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.lt(&b2),
        (Object::Ratnum(r), Object::Fixnum(fx)) => fx.gt_rat(&r),
        (Object::Ratnum(r), Object::Flonum(fl)) => fl.gt_rat(&r),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.lt(&r2),
        (Object::Ratnum(r), Object::Bignum(b)) => b.gt_rat(&r),
        _ => todo!(),
    }
}

pub fn ge(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    !lt(n1, n2)
}

pub fn gt(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx), Object::Fixnum(fl)) => fx > fl,
        (Object::Fixnum(fx), Object::Flonum(fl)) => fx.gt_fl(&fl),
        (Object::Fixnum(fx), Object::Ratnum(r)) => fx.gt_rat(&r),
        (Object::Fixnum(fx), Object::Bignum(b)) => fx.gt_big(&b),
        (Object::Flonum(fl), Object::Fixnum(fx)) => fx.lt_fl(&fl),
        (Object::Flonum(fl1), Object::Flonum(fl2)) => fl1.gt(&fl2),
        (Object::Flonum(fl), Object::Ratnum(r)) => fl.gt_rat(&r),
        (Object::Flonum(fl), Object::Bignum(b)) => fl.gt_big(&b),
        (Object::Bignum(b), Object::Fixnum(fx)) => fx.lt_big(&b),
        (Object::Bignum(b), Object::Flonum(fl)) => !fl.gt_big(&b),
        (Object::Bignum(b), Object::Ratnum(r)) => b.gt_rat(&r),
        (Object::Bignum(b1), Object::Bignum(b2)) => b1.gt(&b2),
        (Object::Ratnum(r), Object::Fixnum(fx)) => fx.lt_rat(&r),
        (Object::Ratnum(r), Object::Flonum(fl)) => fl.lt_rat(&r),
        (Object::Ratnum(r1), Object::Ratnum(r2)) => r1.gt(&r2),
        (Object::Ratnum(r), Object::Bignum(b)) => b.lt_rat(&r),
        _ => todo!(),
    }
}

pub fn le(n1: Object, n2: Object) -> bool {
    assert!(n1.is_number());
    assert!(n2.is_number());
    !gt(n1, n2)
}

pub fn exp(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.exp(),
        Object::Compnum(c) => c.exp(gc),
        _ => Object::Flonum(Flonum::new(real_to_f64(n).exp())),
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
        (Object::Fixnum(_), Object::Flonum(fl)) => {
            let fl1 = real_to_f64(n1);
            let fl2 = fl.value();
            Object::Flonum(Flonum::new(fl1.powf(fl2)))
        }
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Fixnum(_)) => {
            let fl1 = fl.value();
            let fl2 = real_to_f64(n2);
            Object::Flonum(Flonum::new(fl1.powf(fl2)))
        }
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

pub fn quotient(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), _) if fx1 == 0 => Ok(Object::Fixnum(0)),
        (Object::Fixnum(_), Object::Fixnum(fx2)) if fx2 == 0 => Err(SchemeError::NonZeroRequired),
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => Ok(Object::Fixnum(fx1 / fx2)),
        (Object::Fixnum(_), Object::Flonum(_)) => todo!(),
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(_), Object::Fixnum(fx)) if fx == 0 => Err(SchemeError::NonZeroRequired),
        (Object::Flonum(fl), Object::Fixnum(fx)) => Ok(Object::Flonum(Flonum::new(
            (fl.value() / fx.to_f64().unwrap()).trunc(),
        ))),

        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => Ok(Object::Flonum(Flonum::new(
            (fl.value() / b.value.to_f64().unwrap()).trunc(),
        ))),
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

pub fn remainder(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), _) if fx1 == 0 => Ok(Object::Fixnum(0)),
        (Object::Fixnum(_), Object::Fixnum(fx2)) if fx2 == 0 => Err(SchemeError::NonZeroRequired),
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => Ok(Object::Fixnum(fx1 % fx2)),
        (Object::Fixnum(fx), Object::Flonum(fl)) => {
            if fl.value() == 0.0 {
                Err(SchemeError::NonZeroRequired)
            } else {
                Ok(Object::Flonum(Flonum::new(
                    fx.to_f64().unwrap().rem(fl.value()),
                )))
            }
        }
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(_), Object::Fixnum(fx)) if fx == 0 => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => Ok(Object::Flonum(Flonum::new(
            fl.value().rem(fx.to_f64().unwrap()),
        ))),

        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => todo!(),
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

pub fn modulo(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    assert!(n1.is_number());
    assert!(n2.is_number());
    match (n1, n2) {
        (Object::Fixnum(fx1), _) if fx1 == 0 => Ok(Object::Fixnum(0)),
        (Object::Fixnum(_), Object::Fixnum(fx2)) if fx2 == 0 => Err(SchemeError::NonZeroRequired),
        (Object::Fixnum(fx1), Object::Fixnum(fx2)) => {
            let mut r = fx1 % fx2;
            if r == 0 {
                return Ok(Object::Fixnum(0));
            }
            if (fx2 > 0 && r <= 0) || (fx2 <= 0 && r > 0) {
                r = r + fx2
            }
            Ok(Object::Fixnum(r))
        }
        (Object::Fixnum(_), Object::Flonum(_)) => todo!(),
        (Object::Fixnum(_), Object::Ratnum(_)) => todo!(),
        (Object::Fixnum(_), Object::Bignum(_)) => todo!(),
        (Object::Fixnum(_), Object::Compnum(_)) => todo!(),
        (Object::Flonum(_), Object::Fixnum(fx)) if fx == 0 => todo!(),
        (Object::Flonum(fl), Object::Fixnum(fx)) => Ok(Object::Flonum(Flonum::new(
            fl.value().rem(fx.to_f64().unwrap()),
        ))),

        (Object::Flonum(_), Object::Flonum(_)) => todo!(),
        (Object::Flonum(_), Object::Ratnum(_)) => todo!(),
        (Object::Flonum(fl), Object::Bignum(b)) => todo!(),
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

pub fn real(n: Object) -> Object {
    match n {
        Object::Compnum(c) => c.real,
        x if x.is_number() => x,
        _ => todo!(),
    }
}

pub fn imag(n: Object) -> Object {
    match n {
        Object::Compnum(c) => c.imag,
        x if x.is_number() => Object::Fixnum(0),
        _ => todo!(),
    }
}

pub fn to_string(n: Object, radix: usize) -> String {
    assert!(radix == 2 || radix == 8 || radix == 10 || radix == 16);
    match n {
        Object::Fixnum(fx) if radix == 2 => {
            format!("{:b}", fx)
        }
        Object::Fixnum(fx) if radix == 8 => {
            format!("{:o}", fx)
        }
        Object::Fixnum(fx) if radix == 10 => {
            format!("{}", fx)
        }
        Object::Fixnum(fx) if radix == 16 => {
            format!("{:x}", fx)
        }

        _ => panic!(),
    }
}

pub fn log(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.log(),
        Object::Compnum(_) => todo!(),
        _ => {
            let value = real_to_f64(n);
            if value.is_infinite() && n.is_bignum() && gt(n, Object::Fixnum(0)) {
                let ret = n.to_bignum().sqrt(gc);
                let lhs = log(gc, ret);
                let rhs = log(gc, ret);
                add(gc, lhs, rhs)
            } else if value >= 0.0 {
                Object::Flonum(Flonum::new(value.ln()))
            } else {
                let real = Object::Flonum(Flonum::new((-value).ln()));
                let imag = Object::Flonum(Flonum::new(0.0f64.atan2(value)));
                Object::Compnum(gc.alloc(Compnum::new(real, imag)))
            }
        }
    }
}

pub fn log2(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Result<Object, SchemeError> {
    let lhs = log(gc, n1);
    let rhs = log(gc, n2);
    div(gc, lhs, rhs)
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

pub fn ceiling(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_real());
    match n {
        Object::Fixnum(_) | Object::Bignum(_) => n,
        Object::Flonum(fl) => fl.ceiling(),
        Object::Ratnum(r) => r.ceiling(gc),
        _ => panic!(),
    }
}

pub fn cos(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.cos(),
        Object::Flonum(fl) => Object::Flonum(Flonum::new(fl.value().cos())),
        Object::Compnum(c) => c.cos(gc),
        _ if n.is_real() => Object::Flonum(Flonum::new(real_to_f64(n).cos())),
        _ => panic!(),
    }
}

pub fn sin(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.sin(),
        Object::Flonum(fl) => Object::Flonum(Flonum::new(fl.value().sin())),
        Object::Compnum(c) => c.sin(gc),
        _ if n.is_real() => Object::Flonum(Flonum::new(real_to_f64(n).sin())),
        _ => panic!(),
    }
}

pub fn tan(gc: &mut Box<Gc>, n: Object) -> Result<Object, SchemeError> {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => Ok(fx.tan()),
        Object::Compnum(c) => c.tan(gc),
        _ if n.is_real() => Ok(Object::Flonum(Flonum::new(real_to_f64(n).tan()))),
        _ => panic!(),
    }
}

pub fn asin(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.asin(),
        Object::Compnum(c) => Compnum::asin(gc, n),
        _ if n.is_real() => Object::Flonum(Flonum::new(real_to_f64(n).asin())),
        _ => panic!(),
    }
}

pub fn acos(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => fx.acos(),
        Object::Compnum(c) => Compnum::acos(gc, n),
        _ if n.is_real() => Object::Flonum(Flonum::new(real_to_f64(n).acos())),
        _ => panic!(),
    }
}

pub fn atan(gc: &mut Box<Gc>, n: Object) -> Result<Object, SchemeError> {
    assert!(n.is_number());
    match n {
        Object::Fixnum(fx) => Ok(fx.atan()),
        Object::Compnum(c) => Compnum::atan(gc, n),
        _ if n.is_real() => Ok(Object::Flonum(Flonum::new(real_to_f64(n).atan()))),
        _ => panic!(),
    }
}

pub fn atan2(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_real());
    assert!(n2.is_real());
    if n1.is_exact_zero() {
        Object::Fixnum(0)
    } else {
        let r = f64::atan2(real_to_f64(n1), real_to_f64(n2));
        Object::Flonum(Flonum::new(r))
    }
}

pub fn magnitude(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    if n.is_real() {
        if n.is_negative() {
            negate(gc, n)
        } else {
            n
        }
    } else if n.is_compnum() {
        n.to_compnum().magnitude(gc)
    } else {
        panic!()
    }
}

pub fn angle(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    if n.is_real() {
        if n.is_negative() {
            Object::Flonum(Flonum::new(-1.0f64.acos()))
        } else {
            if n.is_flonum() {
                Object::Flonum(Flonum::new(0.0))
            } else {
                Object::Fixnum(0)
            }
        }
    } else if n.is_compnum() {
        n.to_compnum().angle()
    } else {
        panic!()
    }
}

pub fn floor(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_real());
    match n {
        Object::Fixnum(_) | Object::Bignum(_) => n,
        Object::Flonum(fl) => fl.floor(),
        Object::Ratnum(r) => r.floor(gc),
        _ => panic!(),
    }
}

pub fn round(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_real());
    match n {
        Object::Fixnum(_) | Object::Bignum(_) => n,
        Object::Flonum(fl) => fl.round(),
        Object::Ratnum(r) => r.round(gc),
        _ => panic!(),
    }
}

pub fn sqrt(gc: &mut Box<Gc>, obj: Object) -> Object {
    match obj {
        Object::Fixnum(fx) => fx.sqrt(gc),
        Object::Flonum(fl) => fl.sqrt(),
        Object::Bignum(b) => b.sqrt(gc),
        Object::Compnum(c) => c.sqrt(gc),
        Object::Ratnum(r) => todo!(),
        _ => panic!(),
    }
}

pub fn negate(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_real());
    mul(gc, Object::Fixnum(-1), n)
}

pub fn truncate(gc: &mut Box<Gc>, obj: Object) -> Object {
    assert!(obj.is_real());
    match obj {
        Object::Fixnum(_) | Object::Bignum(_) => obj,
        Object::Flonum(f) => f.truncate(),
        Object::Ratnum(r) => r.truncate(gc),
        _ => panic!(),
    }
}

pub fn exact(gc: &mut Box<Gc>, n: Object) -> Object {
    assert!(n.is_number());
    match n {
        Object::Fixnum(_) | Object::Bignum(_) | Object::Ratnum(_) => n,
        Object::Flonum(fl) => fl.to_exact(gc),
        Object::Compnum(c) => {
            let real = exact(gc, c.real);
            let imag = exact(gc, c.imag);
            Object::Compnum(gc.alloc(Compnum::new(real, imag)))
        }
        _ => panic!(),
    }
}

pub fn inexact(gc: &mut Box<Gc>, obj: Object) -> Object {
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

pub fn denominator(gc: &mut Box<Gc>, obj: Object) -> Object {
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

pub fn numerator(gc: &mut Box<Gc>, obj: Object) -> Object {
    assert!(obj.is_rational());
    match obj {
        Object::Ratnum(r) => r.numer(),
        Object::Flonum(fl) => {
            let m = fl.to_exact(gc);
            let denom = numerator(gc, m);
            inexact(gc, denom)
        }
        _ => obj,
    }
}

// http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_448
fn is_integer(gc: &mut Box<Gc>, obj: Object) -> bool {
    assert!(obj.is_number());
    match obj {
        Object::Flonum(f) if f.is_nan() || f.is_infinite() => false,
        Object::Compnum(c) => c.imag.is_exact_zero() && c.real.is_integer(gc),
        _ => eqv(denominator(gc, obj), Object::Fixnum(1)),
    }
}

pub fn real_to_f64(n: Object) -> f64 {
    //assert!(n.is_real_valued());
    match n {
        Object::Fixnum(fx) => fx as f64,
        Object::Flonum(fl) => fl.value(),
        Object::Bignum(b) => match b.to_f64() {
            Some(v) => v,
            None => todo!(),
        },
        Object::Ratnum(r) => match r.to_f64() {
            Some(v) => v,
            None => todo!(),
        },
        _ => panic!(),
    }
}

pub fn make_polar(gc: &mut Box<Gc>, n1: Object, n2: Object) -> Object {
    assert!(n1.is_real_valued());
    assert!(n2.is_real_valued());
    let real = if n1.is_compnum() {
        n1.to_compnum().real
    } else {
        n1
    };
    let imag = if n2.is_compnum() {
        n2.to_compnum().imag
    } else {
        n2
    };
    if eqv(imag, Object::Fixnum(0)) {
        real
    } else {
        let r = real_to_f64(real);
        let a = real_to_f64(imag);
        let lhs = Object::Flonum(Flonum::new(r * a.cos()));
        let rhs = Object::Flonum(Flonum::new(r * a.sin()));
        Object::Compnum(gc.alloc(Compnum::new(lhs, rhs)))
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
    pub fn is_integer_valued(&self, gc: &mut Box<Gc>) -> bool {
        if self.is_integer(gc) {
            true
        } else if self.is_compnum() {
            let c = self.to_compnum();
            c.imag.is_zero() && c.real.is_integer_valued(gc)
        } else {
            false
        }
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
        eqv(Object::Fixnum(0), *self)
    }

    #[inline(always)]
    fn is_negative(&self) -> bool {
        lt(*self, Object::Fixnum(0))
    }

    fn is_real_valued(&self) -> bool {
        if self.is_real() {
            true
        } else if self.is_compnum() {
            let c = self.to_compnum();
            c.imag.is_zero()
        } else {
            false
        }
    }

    pub fn is_even(&self) -> bool {
        match *self {
            Object::Fixnum(fx) => fx % 2 == 0,
            Object::Bignum(b) => b.is_even(),
            Object::Flonum(fl) => fl.is_even(),
            Object::Compnum(c) => c.real.is_even(),
            _ => panic!(),
        }
    }
}
