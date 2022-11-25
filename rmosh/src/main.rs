// We use least significant bits as object tag.
#[repr(C, align(8))]
struct ScmObj {
    //header: isize
}

struct Pair<'a> {
    first: &'a ScmObj,
    second: &'a ScmObj,
}

struct Symbol 

const NUM_TAG_BITS: isize = 3;
const TAG_FIXNUM: isize = 1;
const TAG_PAIR: isize = 1 << 1;

fn create_pair(first: &ScmObj, second: &ScmObj) -> &'static ScmObj {
    let obj = Box::new(Pair {
        first: first,
        second: second,
    });
    let pointer = Box::into_raw(obj) as *const Pair;
    let pointer = pointer as isize;
    let pointer = pointer | TAG_PAIR;
    let pointer = pointer as *const ScmObj;
    unsafe { &*pointer }
}

fn is_pair(obj: &ScmObj) -> bool {
    let pointer = obj as *const ScmObj;
    ((pointer as isize) & TAG_PAIR) != 0
}

fn to_pair(obj: &ScmObj) -> &Pair {
    let pointer = obj as *const ScmObj;
    let pointer = pointer as isize;
    let pointer = pointer & !3;
    let pointer = pointer as *const Pair;
    unsafe { &*pointer }
}

// Note: ScmObj reference has static lifetime
// because it lives for the entire lifetime of the running program.
fn create_fixnum(num: isize) -> &'static ScmObj {
    let obj = num << NUM_TAG_BITS;
    let obj = obj | TAG_FIXNUM;
    // Cast as raw pointer.
    let pointer = obj as *const ScmObj;
    // Pointer to reference.
    unsafe { &*pointer }
}

fn fixnum_value(obj: &ScmObj) -> isize {
    let pointer = obj as *const ScmObj;
    pointer as isize >> NUM_TAG_BITS
}

fn is_fixnum(obj: &ScmObj) -> bool {
    let pointer = obj as *const ScmObj;
    ((pointer as isize) & TAG_FIXNUM) != 0
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
        let obj = ScmObj {};
        assert!(!is_fixnum(&obj));
    }

    #[test]
    fn test_pair() {
        let first = create_fixnum(1234);
        let second = create_fixnum(5678);
        let obj = create_pair(first, second);
        assert!(is_pair(obj));
        let pair = to_pair(obj);
        assert!(is_fixnum(pair.first));
        assert!(is_fixnum(pair.second));
        assert_eq!(fixnum_value(pair.first), 1234);
        assert_eq!(fixnum_value(pair.second), 5678);
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
