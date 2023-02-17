#[macro_export]
macro_rules! as_bytevector {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_bytevector() {
            return Err(error::Error::new_from_string(
                $gc,
                $name,
                "bytevector required",
                &[o],
            ));
        }
        o.to_bytevector()
    }};
}

#[macro_export]
macro_rules! as_char99 {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let o = $args[$i];
        if !o.is_char() {
            return Err(error::Error::new_from_string(
                $gc,
                $name,
                "char required",
                &[o],
            ));
        }
        o.to_char()
    }};
}

