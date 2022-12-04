// GC implementation based on loxido.
use std::ptr::NonNull;

#[derive(Debug)]
pub enum ObjectType {
    Fixnum,
}


#[repr(C)]
pub struct GcObject {
    marked: bool,
    next: Option<NonNull<GcObject>>,
    obj_type: ObjectType,
}



#[repr(transparent)]
#[derive(Clone, Copy, Debug)]
pub struct Object {
    raw_val: isize,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum HeapObjectType {
    Symbol,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct HeapObject {
    obj_type: HeapObjectType,
    ptr: *const u8,
}

pub struct Pair<'a> {
    pub first: &'a Object,
    pub second: &'a Object,
}

#[repr(transparent)]
pub struct Symbol {
    pub name_ptr: *const str,
}

#[derive(Debug, PartialEq)]
pub struct ConversionError {}

impl Object {
    const NUM_TAG_BITS: isize = 3;
    const TAG_MASK: isize = 7;
    const TAG_HEAP_OBJ: isize = 0;
    const TAG_FIXNUM: isize = 1;
    const TAG_PAIR: isize = 1 << 1;
    const TAG_CLEAR: isize = !Self::TAG_MASK;

    fn tag(&self) -> isize {
        self.raw_val & Self::TAG_MASK
    }

    pub fn from_fixnum(n: isize) -> Object {
        let raw_val = n << Self::NUM_TAG_BITS;
        let raw_val = raw_val | Self::TAG_FIXNUM;
        Object { raw_val: raw_val }
    }

    pub fn new_pair(bump: &bumpalo::Bump, first: &Object, second: &Object) -> Object {
        let pair = bump.alloc(Pair {
            first: first,
            second: second,
        });
        let raw_val = pair as *const Pair as isize;
        let raw_val = raw_val | Self::TAG_PAIR;
        Object { raw_val: raw_val }
    }

    pub fn new_symbol(bump: &bumpalo::Bump, name_ptr: *const str) -> Object {
        let symbol = bump.alloc(Symbol { name_ptr: name_ptr });
        let ptr = symbol as *const Symbol as *const u8;
        let heap_obj = bump.alloc(HeapObject {
            obj_type: HeapObjectType::Symbol,
            ptr: ptr,
        });
        let raw_val = heap_obj as *const HeapObject as isize;
        let raw_val = raw_val | Self::TAG_HEAP_OBJ;
        Object { raw_val: raw_val }
    }

    pub fn is_symbol(&self) -> bool {
        if self.tag() != Self::TAG_HEAP_OBJ {
            return false;
        }
        let ptr = self.raw_val & Self::TAG_CLEAR;
        let ptr = ptr as *const HeapObject;
        let heap_obj = unsafe { *ptr };
        heap_obj.obj_type == HeapObjectType::Symbol
    }

    pub fn into_symbol(&self) -> Result<&Symbol, ConversionError> {
        if self.is_symbol() {
            let ptr = self.raw_val & Self::TAG_CLEAR;
            let ptr = ptr as *const HeapObject;
            let heap_obj = unsafe { *ptr };
            let ptr = heap_obj.ptr as *const Symbol;
            Ok(unsafe { &*ptr })
        } else {
            Err(ConversionError {})
        }
    }

    pub fn is_pair(&self) -> bool {
        self.tag() == Self::TAG_PAIR
    }

    pub fn into_pair(&self) -> Result<&Pair, ConversionError> {
        if self.is_pair() {
            let ptr = self.raw_val & Self::TAG_CLEAR;
            let ptr = ptr as *const Pair;
            Ok(unsafe { &*ptr })
        } else {
            Err(ConversionError {})
        }
    }

    pub fn is_fixnum(&self) -> bool {
        self.tag() == Self::TAG_FIXNUM
    }

    pub fn into_fixnum(&self) -> Result<isize, ConversionError> {
        if self.is_fixnum() {
            Ok(self.raw_val >> Object::NUM_TAG_BITS)
        } else {
            Err(ConversionError {})
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    CONSTANT(Object),
    PUSH,
    ADD,
}

pub struct Vm {
    pub ac: Object,
}

impl<'a> Vm {
    pub fn run(&mut self, ops: &Vec<Op>) -> Object {
        const STACK_SIZE: usize = 256;
        let mut stack: [Object; STACK_SIZE] = [Object::from_fixnum(0); STACK_SIZE];
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
                                self.ac = Object::from_fixnum(a + b);
                            }
                        },
                    }
                }
            }
        }
        self.ac
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_fixnum() {
        let obj = Object::from_fixnum(123456);
        assert!(obj.is_fixnum());
        assert_eq!(obj.into_fixnum(), Ok(123456_isize));
    }
    #[test]
    fn test_pair() {
        let bump: bumpalo::Bump = bumpalo::Bump::new();
        let first = Object::from_fixnum(1234);
        let second = Object::from_fixnum(5678);
        let obj = Object::new_pair(&bump, &first, &second);
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
        let bump: bumpalo::Bump = bumpalo::Bump::new();
        let obj = Object::new_symbol(&bump, name);
        assert!(obj.is_symbol());
        match obj.into_symbol() {
            Err(why) => panic!("{:?}", why),
            Ok(symbol) => {
                assert_eq!(symbol.name_ptr, name);
            }
        }
    }

    #[test]
    fn test_vm_run() {
        let ops = vec![
            Op::CONSTANT(Object::from_fixnum(99)),
            Op::PUSH,
            Op::CONSTANT(Object::from_fixnum(1)),
            Op::ADD,
        ];
        let mut vm = Vm {
            ac: Object::from_fixnum(0),
        };
        let ret = vm.run(&ops);
        assert_eq!(ret.into_fixnum(), Ok(100));
    }
}
