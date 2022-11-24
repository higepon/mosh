
type ScmObj = *const i64;

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

fn main() {
    let address = 0xdeadbeefusize;
    let _raw_pointer = address as *const i32;
    /*
     * Of course this causes Segmentation fault.
    unsafe {
        println!("pointer={}", *_raw_pointer);
    }
    */

    let obj = create_fixnum(123456);
    if is_fixnum(obj) {
        print!("num={}\n", fixnum_value(obj));
    } else {
        
    }
}
