/*
    Requirements for scheme::Object.
    Object should be able to represent
    - Fixnum (tag bits object: Immdiate value).
    - Pair (tag bits object: Heap allocated).
    - Symbol (Type tag object: Heap allocated)


*/

pub mod scheme {

    type Fixnum = isize;

    #[derive(Copy, Clone, Debug)]
    pub enum Op<'a> {
        CONSTANT(&'a Object),
        PUSH,
        ADD,
    }

    pub struct Vm<'a> {
        pub ac: &'a Object,
    }

    impl<'a> Vm<'a> {
        pub fn run(&mut self, ops: &Vec<Op<'a>>) -> &Object {
            const STACK_SIZE: usize = 256;
            let mut stack: [&Object; STACK_SIZE] = [Object::new_fixnum(0); STACK_SIZE];
            let mut sp: usize = 0;
            let len = ops.len();
            let mut idx = 0;
            while idx < len {
                let op = ops[idx];
                idx += 1;
                match op {
                    Op::CONSTANT(c) => {
                        self.ac = c;
                    }
                    Op::PUSH => {
                        assert!(sp < STACK_SIZE);
                        stack[sp] = self.ac;
                        sp += 1;
                    }
                    Op::ADD => {
                        sp -= 1;
                        match stack[sp].into_fixnum() {
                            Err(why) => panic!("{:?}", why),
                            Ok(a) => match self.ac.into_fixnum() {
                                Err(why) => panic!("{:?}", why),
                                Ok(b) => {
                                    println!("{} + {}", a, b);
                                    self.ac = Object::new_fixnum(a + b);
                                }
                            },
                        }
                    }
                }
            }
            self.ac
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum ObjectType {
        Symbol,
    }

    // We use least significant bits as object tag.
    #[repr(C, align(8))]
    #[derive(Debug)]
    pub struct Object {
        obj_type: ObjectType,
        ptr: *const u8,
    }

/*    impl std::convert::TryFrom<&Object> for isize {
        type Error = ();
        fn try_from(obj: &Object) -> Result<Self, Self::Error> {
            if obj.is_fixnum() {
                let ptr = obj as *const Object;
                Ok(ptr as isize >> Object::NUM_TAG_BITS)
            } else {
                Err(())
            }
        }
    }*/

    #[derive(Debug, PartialEq)]
    pub enum ConvError {
        NotPair,
        NotSymbol,
        NotFixnum,
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
            let symbol = bump.alloc(Symbol { name_ptr: name_ptr });
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

        fn tag(&self) -> isize {
            let ptr = self as *const Object as isize;
            ptr & Self::TAG_MASK
        }

        pub fn into_pair(&self) -> Result<&Pair, ConvError> {
            if self.is_pair() {
                let ptr = self as *const Object;
                let ptr = ptr as isize;
                let ptr = ptr & Self::TAG_CLEAR;
                let ptr = ptr as *const Pair;
                Ok(unsafe { &*ptr })
            } else {
                Err(ConvError::NotPair)
            }
        }

        pub fn into_symbol(&self) -> Result<&Symbol, ConvError> {
            if self.is_symbol() {
                let ptr = self.ptr as *const Symbol;
                Ok(unsafe { &*ptr })
            } else {
                Err(ConvError::NotSymbol)
            }
        }

        pub fn into_fixnum(&self) -> Result<Fixnum, ConvError> {
            if self.is_fixnum() {
                let ptr = self as *const Object;
                Ok(ptr as Fixnum >> Object::NUM_TAG_BITS)
            } else {
                Err(ConvError::NotFixnum)
            }
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
        assert_eq!(obj.into_fixnum(), Ok(123456_isize));
    }

    #[test]
    fn test_not_fixnum() {
        let name = "foo";
        let name_ptr = name as *const str;
        let bump: bumpalo::Bump = bumpalo::Bump::new();
        let obj = scheme::Object::new_symbol(&bump, name_ptr as *const u8);
        assert!(!obj.is_fixnum());
    }

    #[test]
    fn test_pair() {
        let bump: bumpalo::Bump = bumpalo::Bump::new();
        let first = scheme::Object::new_fixnum(1234);
        let second = scheme::Object::new_fixnum(5678);
        let obj = scheme::Object::new_pair(&bump, first, second);
        assert!(obj.is_pair());
        match obj.into_pair() {
            Err(why) => panic!("{:?}", why),
            Ok(pair) => {
                assert!(pair.first.is_fixnum());
                assert!(pair.second.is_fixnum());
                assert_eq!(pair.first.into_fixnum(), Ok(1234));
                assert_eq!(pair.second.into_fixnum(), Ok(5678));
            }
        }
    }

    #[test]
    fn test_symbol() {
        let name = "foo";
        let name_ptr = name as *const str;
        let name_ptr = name_ptr as *const u8;
        let bump: bumpalo::Bump = bumpalo::Bump::new();
        let obj = scheme::Object::new_symbol(&bump, name_ptr);
        assert!(obj.is_symbol());
        match obj.into_symbol() {
            Err(why) => panic!("{:?}", why),
            Ok(symbol) => {
                assert_eq!(symbol.name_ptr, name_ptr);
            }
        }
    }

    #[test]
    fn test_vm_run() {
        let ops = vec![
            scheme::Op::CONSTANT(scheme::Object::new_fixnum(99)),
            scheme::Op::PUSH,
            scheme::Op::CONSTANT(scheme::Object::new_fixnum(1)),
            scheme::Op::ADD,
        ];
        let mut vm = scheme::Vm {
            ac: scheme::Object::new_fixnum(0),
        };
        let ret = vm.run(&ops);
        assert_eq!(ret.into_fixnum(), Ok(100));
    }
}

fn main() {}
