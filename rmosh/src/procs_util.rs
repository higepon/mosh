#[macro_export]
macro_rules! as_bytevector {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_bytevector() {
            return error::Error::assertion_violation($name, "bytevector required", &[o]);
        }
        o.to_bytevector()
    }};
}

#[macro_export]
macro_rules! as_char {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_char() {
            return error::Error::assertion_violation($name, "char required", &[o]);
        }
        o.to_char()
    }};
}

#[macro_export]
macro_rules! as_sstring {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_string() {
            return error::Error::assertion_violation($name, "string required", &[o]);
        }
        o.to_sstring()
    }};
}

#[macro_export]
macro_rules! as_symbol {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_symbol() {
            return error::Error::assertion_violation($name, "symbol required", &[o]);
        }
        o.to_symbol()
    }};
}

#[macro_export]
macro_rules! as_usize {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        o.to_isize() as usize
    }};
}

#[macro_export]
macro_rules! as_isize {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        o.to_isize()
    }};
}

#[macro_export]
macro_rules! as_f64 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum().value()
    }};
}

#[macro_export]
macro_rules! as_f32 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum().value() as f32
    }};
}

#[macro_export]
macro_rules! as_flonum {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($name, "flonum required", &[o]);
        }
        o.to_flonum()
    }};
}

// For bytevectors.
#[macro_export]
macro_rules! as_u8 {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($name, "number required", &[o]);
        }
        let fx = o.to_isize();
        if -128 <= fx && fx <= 255 {
            fx as u8
        } else {
            return error::Error::assertion_violation($name, "u8 value required", &[o]);
        }
    }};
}

#[macro_export]
macro_rules! as_transcoder {
    ($name:ident, $args:ident, $i:expr) => {{
        let o = $args[$i];
        if !o.is_transcoder() {
            return error::Error::assertion_violation($name, "transcoder required", &[o]);
        }
        o.to_transcoder()
    }};
}
