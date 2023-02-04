use std::fmt::{Display, self};

use num_rational::Rational64;

use crate::{gc::{GcHeader, ObjectType}, objects::Object};

/// Rational number.
#[derive(Debug)]
pub struct Ratnum {
    pub header: GcHeader,
    ratio: Rational64,
}

impl Ratnum {
    pub fn new(numer: isize, denom: isize) -> Self {
        Self {
            header: GcHeader::new(ObjectType::Ratnum),
            ratio: Rational64::new_raw(numer as i64, denom as i64),
        }
    }
}

impl Display for Ratnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<ratnum>")
    }
}
