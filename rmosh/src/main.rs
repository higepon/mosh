struct ScmObj {
    header: isize
    // Empty for now.
}

const NUM_TAG_BITS: isize = 3;
const TAG_FIXNUM: isize = 0x1;

// Note: ScmObj reference has static lifetime
// because it lives for the entire lifetime of the running program.
unsafe fn create_fixnum(num: isize) -> &'static ScmObj {
    let obj = num << NUM_TAG_BITS;
    let obj = obj | TAG_FIXNUM;
    // Cast as raw pointer.
    let pointer = obj as *const ScmObj;
    // Pointer to reference.
    return &*pointer;
}

fn fixnum_value(obj: &ScmObj) -> isize {
    let pointer = obj as *const ScmObj;
    return pointer as isize >> NUM_TAG_BITS;
}

fn is_fixnum(obj: &ScmObj) -> bool {
    let pointer = obj as *const ScmObj;    
    print!("pointer={:#x}", (pointer as isize));
    return ((pointer as isize) & TAG_FIXNUM) != 0;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixnum() {
        unsafe {
            let obj = create_fixnum(123456);        
            assert!(is_fixnum(obj));
            assert_eq!(fixnum_value(obj), 123456);
        }
    }

    #[test]
    fn test_not_fixnum() {
        let obj = ScmObj {header:0};
        assert!(!is_fixnum(&obj));
    }    
}

fn main() {
    let address = 0xdeadbeefusize;
    let _raw_pointer = address as *const i32;
    /*
     * Of course this causes Segmentation fault.
    unsafe {
        println!("pointer={}", *_raw_pointer);
    }
    */
}
