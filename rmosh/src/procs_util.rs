#[macro_export]
macro_rules! as_bytevector {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_bytevector() {
            return error::Error::assertion_violation($gc, $name, "bytevector required", &[o]);
        }
        o.to_bytevector()
    }};
}

#[macro_export]
macro_rules! as_char {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_char() {
            return error::Error::assertion_violation($gc, $name, "char required", &[o]);
        }
        o.to_char()
    }};
}

#[macro_export]
macro_rules! as_sstring {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_string() {
            return error::Error::assertion_violation($gc, $name, "string required", &[o]);
        }
        o.to_sstring()
    }};
}

#[macro_export]
macro_rules! as_symbol {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_symbol() {
            return error::Error::assertion_violation($gc, $name, "symbol required", &[o]);
        }
        o.to_symbol()
    }};
}

#[macro_export]
macro_rules! as_usize {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($gc, $name, "number required", &[o]);
        }
        o.to_isize() as usize
    }};
}

#[macro_export]
macro_rules! as_isize {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_fixnum() {
            return error::Error::assertion_violation($gc, $name, "number required", &[o]);
        }
        o.to_isize()
    }};
}

#[macro_export]
macro_rules! as_f64 {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($gc, $name, "flonum required", &[o]);
        }
        o.to_flonum().value()
    }};
}

#[macro_export]
macro_rules! as_flonum {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_flonum() {
            return error::Error::assertion_violation($gc, $name, "flonum required", &[o]);
        }
        o.to_flonum()
    }};
}
