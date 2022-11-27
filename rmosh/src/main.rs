// TODO
// - Swap allocator
// - Use RawPtr?
// - Understand Trait better.
// - http://blog.pnkfx.org/blog/categories/gc/




pub mod scheme {

    pub struct Vm {}

    impl Vm {
        pub fn run(&self, ops: &Vec<&Object>) -> &Object {
            Object::new_fixnum(3)
        }
    }
    

    #[derive(Debug, PartialEq)]
    pub enum ObjectType {
        Symbol,
    }

    // We use least significant bits as object tag.
    #[repr(C, align(8))]
    pub struct Object {
        obj_type: ObjectType,
        ptr: *const u8,
    }

    impl Object {
        const NUM_TAG_BITS: isize = 3;
        const TAG_MASK: isize = 7;
        const TAG_HEAP_OBJ: isize = 0;
        const TAG_FIXNUM: isize = 1;
        const TAG_PAIR: isize = 1 << 1;
        const TAG_CLEAR: isize = !Self::TAG_MASK;

        // Note: Object reference has static lifetime
        // because it lives for the entire lifetime of the running program.
        pub fn new_fixnum(n: isize) -> &'static Object {
            let obj = n << Self::NUM_TAG_BITS;
            let obj = obj | Self::TAG_FIXNUM;
            // Cast as raw pointer.
            let ptr = obj as *const Object;
            // Pointer to reference.
            unsafe { &*ptr }
        }

        pub fn new_pair(bump: &bumpalo::Bump, first: &Object, second: &Object) -> &'static Object {
            let obj = bump.alloc(Pair {
                first: first,
                second: second,
            });
            let ptr = obj as *const Pair as isize;
            let ptr = ptr | Self::TAG_PAIR;
            let ptr = ptr as *const Object;
            unsafe { &*ptr }
        }

        pub fn new_symbol(bump: &bumpalo::Bump, name_ptr: *const u8) -> &'static Object {
            let symbol = bump.alloc(Symbol {name_ptr: name_ptr});
            let ptr = symbol as *const Symbol as *const u8;
            let obj = bump.alloc(Object {
                obj_type: ObjectType::Symbol,
                ptr: ptr,
            });
            let ptr = obj as *const Object;
            unsafe { &*ptr }
        }

        pub fn is_pair(&self) -> bool {
            self.tag() == Self::TAG_PAIR
        }

        pub fn is_symbol(&self) -> bool {
            if self.tag() != Self::TAG_HEAP_OBJ {
                return false;
            }
            self.obj_type == ObjectType::Symbol
        }

        pub fn is_fixnum(&self) -> bool {
            self.tag() == Self::TAG_FIXNUM
        }

        pub fn to_symbol(&self) -> &Symbol {
            let ptr = self.ptr as *const Symbol;
            unsafe { &*ptr }
        }

        pub fn to_pair(&self) -> &Pair {
            let ptr = self as *const Object;
            let ptr = ptr as isize;
            let ptr = ptr & Self::TAG_CLEAR;
            let ptr = ptr as *const Pair;
            unsafe { &*ptr }
        }

        pub fn to_fixnum(&self) -> isize {
            let ptr = self as *const Object;
            ptr as isize >> Self::NUM_TAG_BITS
        }

        fn tag(&self) -> isize {
            let ptr = self as *const Object as isize;
            ptr & Self::TAG_MASK
        }
    }

    pub struct Pair<'a> {
        pub first: &'a Object,
        pub second: &'a Object,
    }

    #[repr(C, align(8))]
    pub struct Symbol {
        pub name_ptr: *const u8,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fixnum() {
        let obj = scheme::Object::new_fixnum(123456);
        assert!(obj.is_fixnum());
        assert_eq!(obj.to_fixnum(), 123456);
    }

    #[test]
    fn test_not_fixnum() {
        let name = "foo";
        let name_ptr = name as *const str;
        let bump: bumpalo::Bump = bumpalo::Bump::new() ;                        
        let obj = scheme::Object::new_symbol(&bump, name_ptr as *const u8);
        assert!(!obj.is_fixnum());
    }

    #[test]
    fn test_pair() {
        let bump: bumpalo::Bump = bumpalo::Bump::new() ;                                
        let first = scheme::Object::new_fixnum(1234);
        let second = scheme::Object::new_fixnum(5678);       
        let obj = scheme::Object::new_pair(&bump, first, second);
        assert!(obj.is_pair());
        let pair = obj.to_pair();
        assert!(pair.first.is_fixnum());
        assert!(pair.second.is_fixnum());
        assert_eq!(pair.first.to_fixnum(), 1234);
        assert_eq!(pair.second.to_fixnum(), 5678);
    }

    #[test]
    fn test_symbol() {
        let name = "foo";
        let name_ptr = name as *const str;        
        let name_ptr = name_ptr as *const u8;
        let bump: bumpalo::Bump = bumpalo::Bump::new() ;                                
        let obj = scheme::Object::new_symbol(&bump, name_ptr);
        assert!(obj.is_symbol());
        let symbol = obj.to_symbol();
        assert_eq!(symbol.name_ptr, name_ptr);
    }

    #[test]
    fn test_vm_run() {
        let ops = vec![scheme::Object::new_fixnum(3)];
        let vm = scheme::Vm {};
        vm.run(&ops);
    }    
}



fn main() {

}
