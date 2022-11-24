
type ScmObj = *const isize;

const NUM_TAG_BITS: isize = 3;
const TAG_FIXNUM: isize = 0x1;

fn create_fixnum(num: isize) -> ScmObj {
    let obj = num << NUM_TAG_BITS;
    let obj = obj | TAG_FIXNUM;
    return obj as ScmObj;
}

fn fixnum_value(obj: ScmObj) -> isize {
    return obj as isize >> NUM_TAG_BITS;
}

fn is_fixnum(obj: ScmObj) -> bool {
    return ((obj as isize) & TAG_FIXNUM) != 0;
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
        let v: isize = 123456;
        let obj = &v; 
        assert!(!is_fixnum(obj));
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
