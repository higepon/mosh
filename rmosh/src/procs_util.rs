
#[macro_export]
macro_rules! as_bytevector {
    ($name:ident, $args:ident, $i:expr, $gc:expr) => {{
        let bv = $args[$i];
        if !bv.is_bytevector() {
            return Err(error::Error::new_from_string($gc, $name, "bytevector required", bv))        
        }
        bv.to_bytevector()
    }};
}