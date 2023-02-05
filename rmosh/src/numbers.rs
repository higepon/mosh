use std::{
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut},
};

use num_bigint::BigInt;
use num_rational::Rational64;
use num_traits::{FromPrimitive, ToPrimitive};

use crate::{
    gc::{Gc, GcHeader, ObjectType},
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

    #[inline(always)]
    pub fn eq(&self, other: &Flonum) -> bool {
        self.value() == other.value()
    }

    pub fn fx_eq(&self, f: isize) -> bool {
        if self.is_finite() || self.is_nan() {
            false
        } else {
            (f as f64) == self.value()
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

    pub fn fx_eq(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v == f,
            None => false,
        }
    }

    pub fn fl_eq(&self, fl: &Flonum) -> bool {
        match self.to_f64() {
            Some(v) => v == **fl,
            None => false,
        }
    }

    pub fn bi_eq(&self, b: &Bignum) -> bool {
        match (b.to_f64(), self.to_f64()) {
            (Some(l), Some(r)) => l == r,
            _ => false,
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
            value: value
        }
    }
    pub fn fx_eq(&self, f: isize) -> bool {
        match self.to_isize() {
            Some(v) => v == f,
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
        write!(f, "#<compnum>")
    }
}

/// Number functions.
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

fn is_exact_zero(obj: Object) -> bool {
    todo!()
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
        Object::Compnum(c) => is_exact_zero(c.imag) && is_integer(gc, c.real),
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
}
