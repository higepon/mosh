// TODO
// - Scm prefix looks weird. Use namespace?

#[derive(Debug, PartialEq)]
pub enum ScmObjType {
    Symbol,
}

// We use least significant bits as object tag.
#[repr(C, align(8))]
pub struct ScmObj {
    obj_type: ScmObjType,
    ptr: *const u8,
}

impl ScmObj {
    pub fn tag(&self) -> isize {
        let ptr = self as *const ScmObj as isize;
        ptr & TAG_MASK
    }

    pub fn is_pair(&self) -> bool {
        self.tag() == TAG_PAIR
    }    

    pub fn is_symbol(&self) -> bool {
        if self.tag() != TAG_HEAP_OBJ {
            return false;
        }
        self.obj_type == ScmObjType::Symbol
    }
    
    pub fn is_fixnum(&self) -> bool {
        self.tag() == TAG_FIXNUM
    }
    
}

pub struct Pair<'a> {
    first: &'a ScmObj,
    second: &'a ScmObj,
}

#[repr(C, align(8))]
pub struct Symbol {
    value: String,
}

const NUM_TAG_BITS: isize = 3;
const TAG_MASK: isize = 7;
const TAG_HEAP_OBJ: isize = 0;
const TAG_FIXNUM: isize = 1;
const TAG_PAIR: isize = 1 << 1;

pub fn create_symbol(value: &str) -> &'static ScmObj {
    let symbol = Box::new(Symbol {
        value: value.to_string(),
    });
    let ptr = Box::into_raw(symbol) as *const u8;
    let obj = Box::new(ScmObj {
        obj_type: ScmObjType::Symbol,
        ptr: ptr,
    });
    let ptr = Box::into_raw(obj) as *const ScmObj;
    unsafe { &*ptr }
}

pub fn to_symbol(obj: &ScmObj) -> &Symbol {
    let ptr = obj.ptr;
    let ptr = ptr as *const Symbol;
    unsafe { &*ptr }
}

pub fn create_pair(first: &ScmObj, second: &ScmObj) -> &'static ScmObj {
    let obj = Box::new(Pair {
        first: first,
        second: second,
    });
    let ptr = Box::into_raw(obj) as isize;
    let ptr = ptr | TAG_PAIR;
    let ptr = ptr as *const ScmObj;
    unsafe { &*ptr }
}



pub fn to_pair(obj: &ScmObj) -> &Pair {
    let ptr = obj as *const ScmObj;
    let ptr = ptr as isize;
    let ptr = ptr & !3;
    let ptr = ptr as *const Pair;
    unsafe { &*ptr }
}

// Note: ScmObj reference has static lifetime
// because it lives for the entire lifetime of the running program.
pub fn create_fixnum(num: isize) -> &'static ScmObj {
    let obj = num << NUM_TAG_BITS;
    let obj = obj | TAG_FIXNUM;
    // Cast as raw pointer.
    let ptr = obj as *const ScmObj;
    // Pointer to reference.
    unsafe { &*ptr }
}

pub fn fixnum_value(obj: &ScmObj) -> isize {
    let ptr = obj as *const ScmObj;
    ptr as isize >> NUM_TAG_BITS
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixnum() {
        let obj = create_fixnum(123456);
        assert!(obj.is_fixnum());
        assert_eq!(fixnum_value(obj), 123456);
    }

    #[test]
    fn test_not_fixnum() {
        let obj = create_symbol("foo");
        assert!(!obj.is_fixnum());
    }

    #[test]
    fn test_pair() {
        let first = create_fixnum(1234);
        let second = create_fixnum(5678);
        let obj = create_pair(first, second);
        assert!(obj.is_pair());
        let pair = to_pair(obj);
        assert!(pair.first.is_fixnum());
        assert!(pair.second.is_fixnum());
        assert_eq!(fixnum_value(pair.first), 1234);
        assert_eq!(fixnum_value(pair.second), 5678);
    }

    #[test]
    fn test_symbol() {
        let obj = create_symbol("foo");
        assert!(obj.is_symbol());
        let symbol = to_symbol(obj);
        assert_eq!(symbol.value, String::from("foo"));
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
