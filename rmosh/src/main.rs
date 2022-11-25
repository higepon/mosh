struct ScmObj {
    header: isize
}

const NUM_TAG_BITS: isize = 3;
const TAG_FIXNUM: isize = 0x1;

fn create_fixnum(num: isize) -> *const ScmObj {
    let obj = num << NUM_TAG_BITS;
    let obj = obj | TAG_FIXNUM;
    // Cast as raw pointer.
    obj as *const ScmObj
}

fn fixnum_value(obj: *const ScmObj) -> isize {
    obj as isize >> NUM_TAG_BITS
}

fn is_fixnum(obj: *const ScmObj) -> bool {
    ((obj as isize) & TAG_FIXNUM) != 0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixnum() {
        let obj = create_fixnum(123456);        
        assert!(is_fixnum(obj));
        assert_eq!(fixnum_value(obj), 123456);
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
