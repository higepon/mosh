use std::{
    collections::HashMap,
    io::{self, Read},
};

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{
    gc::{Gc, GcRef},
    numbers::Flonum,
    objects::{EqHashtable, Object, SimpleStruct},
    ports::BinaryFileOutputPort,
};

#[derive(FromPrimitive)]
enum Tag {
    Fixnum = 0,
    True = 1,
    False = 2,
    Nil = 3,
    Char = 4,
    Symbol = 5,
    String = 6,
    Pair = 7,
    Vector = 8,
    CompilerInsn = 9,
    Struct = 10,
    Unspecified = 11,
    EqHashtable = 12,
    DefineShared = 13,
    LookupShared = 14,
    Flonum = 15,
}

// S-expression serializer.
pub struct FaslWriter {}

impl FaslWriter {
    pub fn new() -> Self {
        Self {}
    }
    pub fn write(
        &self,
        port: &mut GcRef<BinaryFileOutputPort>,
        obj: Object,
    ) -> Result<(), io::Error> {
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        let mut shared_id = 1;
        self.write_one(port, &mut seen, &mut shared_id, obj)
    }
    pub fn write_one(
        &self,
        port: &mut GcRef<BinaryFileOutputPort>,
        seen: &mut HashMap<Object, Object>,
        shared_id: &mut isize,
        obj: Object,
    ) -> Result<(), io::Error> {
        let seen_state = match seen.get(&obj) {
            Some(val) => *val,
            None => Object::False,
        };
        if seen_state.is_true() {
            seen.insert(obj, Object::Fixnum(*shared_id));
            self.put_tag(port, Tag::DefineShared)?;
            port.put_u32(*shared_id as u32)?;
            seen.insert(obj, Object::Fixnum(*shared_id));
            *shared_id += 1;
            // We don't return here and write the object.
        } else if seen_state.is_number() {
            self.put_tag(port, Tag::LookupShared)?;
            port.put_u32(seen_state.to_isize() as u32)?;
            return Ok(());
        }
        match obj {
            Object::Bytevector(_) => todo!(),
            Object::BytevectorInputPort(_) => todo!(),
            Object::BytevectorOutputPort(_) => todo!(),
            Object::Char(c) => {
                self.put_tag(port, Tag::Char)?;
                port.put_u32(c as u32)?;
            }
            Object::Closure(_) => todo!(),
            Object::Eof => todo!(),
            Object::EqHashtable(t) => {
                self.put_tag(port, Tag::EqHashtable)?;
                port.put_u16(t.size() as u16)?;
                for (key, value) in t.hash_map.iter() {
                    self.write_one(port, seen, shared_id, *key)?;
                    self.write_one(port, seen, shared_id, *value)?;
                }
                port.put_u8(if t.is_mutable { 1 } else { 0 })?;
            }
            Object::False => {
                self.put_tag(port, Tag::False)?;
            }
            Object::Flonum(f) => {
                self.put_tag(port, Tag::Flonum)?;
                port.put_u64(f.u64_value())?;
            }
            Object::Bignum(_r) => {
                todo!();
            }
            Object::Compnum(_r) => {
                todo!();
            }
            Object::Continuation(_r) => {
                todo!();
            }
            Object::ContinuationStack(_r) => {
                todo!();
            }
            Object::Ratnum(_r) => {
                todo!();
            }
            Object::Regexp(_r) => {
                todo!();
            }
            Object::StringInputPort(_) => todo!(),
            Object::FileInputPort(_) => todo!(),
            Object::FileOutputPort(_) => todo!(),
            Object::BinaryFileOutputPort(_) => todo!(),
            Object::BinaryFileInputPort(_) => todo!(),
            Object::StdInputPort(_) => todo!(),            
            Object::StdOutputPort(_) => todo!(),
            Object::StdErrorPort(_) => todo!(),
            Object::StringOutputPort(_) => todo!(),
            Object::Instruction(op) => {
                self.put_tag(port, Tag::CompilerInsn)?;
                port.put_u8(op as u8)?;
            }
            Object::Nil => {
                self.put_tag(port, Tag::Nil)?;
            }
            Object::Fixnum(n) => {
                self.put_tag(port, Tag::Fixnum)?;
                port.put_u64(n as u64)?;
            }
            Object::Pair(p) => {
                self.put_tag(port, Tag::Pair)?;
                self.write_one(port, seen, shared_id, p.car)?;
                self.write_one(port, seen, shared_id, p.cdr)?;
            }
            Object::Procedure(_) => todo!(),
            Object::SimpleStruct(s) => {
                self.put_tag(port, Tag::Struct)?;
                port.put_u16(s.len() as u16)?;
                for i in 0..s.len() {
                    self.write_one(port, seen, shared_id, s.field(i))?;
                }
                self.write_one(port, seen, shared_id, s.name)?;
            }
            Object::String(s) => {
                self.put_tag(port, Tag::String)?;
                port.put_u16(s.string.len() as u16)?;
                for c in s.string.chars() {
                    port.put_u32(c as u32)?;
                }
            }
            Object::Symbol(s) => {
                self.put_tag(port, Tag::Symbol)?;
                port.put_u16(s.string.len() as u16)?;
                for c in s.string.chars() {
                    port.put_u32(c as u32)?;
                }
            }
            Object::True => {
                self.put_tag(port, Tag::True)?;
            }
            Object::Unspecified => {
                self.put_tag(port, Tag::Unspecified)?;
            }
            Object::ObjectPointer(_) => todo!(),
            Object::ProgramCounter(_) => todo!(),
            Object::Vector(v) => {
                self.put_tag(port, Tag::Vector)?;
                port.put_u16(v.len() as u16)?;
                for i in 0..v.len() {
                    self.write_one(port, seen, shared_id, v.data[i])?
                }
            }
            Object::Vox(_) => todo!(),
        }
        Ok(())
    }

    fn put_tag(&self, port: &mut GcRef<BinaryFileOutputPort>, tag: Tag) -> Result<(), io::Error> {
        port.put_u8(tag as u8)?;
        Ok(())
    }

    fn scan(&self, obj: Object, seen: &mut HashMap<Object, Object>) {
        let mut o = obj;
        loop {
            match o {
                Object::Bytevector(_)
                | Object::BytevectorInputPort(_)
                | Object::BytevectorOutputPort(_)
                | Object::Closure(_)
                | Object::Continuation(_)
                | Object::ContinuationStack(_)
                | Object::Vox(_)
                | Object::ProgramCounter(_)
                | Object::ObjectPointer(_)
                | Object::Unspecified
                | Object::True
                | Object::Procedure(_)
                | Object::Char(_)
                | Object::EqHashtable(_)
                | Object::False
                | Object::Flonum(_)
                | Object::StringInputPort(_)
                | Object::FileInputPort(_)
                | Object::Eof
                | Object::Bignum(_)
                | Object::Compnum(_)
                | Object::Ratnum(_)
                | Object::Regexp(_)
                | Object::BinaryFileInputPort(_)
                | Object::BinaryFileOutputPort(_)
                | Object::FileOutputPort(_)
                | Object::StringOutputPort(_)
                | Object::StdInputPort(_)
                | Object::StdOutputPort(_)
                | Object::StdErrorPort(_)
                | Object::Instruction(_)
                | Object::Nil
                | Object::Symbol(_)
                | Object::String(_)
                | Object::Fixnum(_) => return,
                Object::Pair(p) => {
                    let val = match seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    self.scan(p.car, seen);
                    o = p.cdr;
                    continue;
                }
                Object::Vector(v) => {
                    let val = match seen.get(&o) {
                        Some(found) => *found,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    for i in 0..v.len() {
                        self.scan(v.data[i], seen);
                    }
                    break;
                }
                Object::SimpleStruct(s) => {
                    let val = match seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        seen.insert(obj, Object::False);
                    }
                    for i in 0..s.len() {
                        self.scan(s.field(i), seen);
                    }
                    break;
                }
            }
        }
    }
}

#[macro_export]
macro_rules! read_sym_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let sym = $self.read_sexp($gc)?;
        let m = $self.read_sexp($gc)?;
        Ok(OpOld::$op(sym.to_symbol(), m.to_number()))
    }};
}

#[macro_export]
macro_rules! read_num_constant {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        Ok(OpOld::$op(n.to_number(), c))
    }};
}

#[macro_export]
macro_rules! read_num_constant_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read_sexp($gc)?;
        let c = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(OpOld::$op(n.to_number(), c, o.to_number()))
    }};
}

#[macro_export]
macro_rules! read_sym1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let s = $self.read_sexp($gc)?;
        Ok(OpOld::$op(s.to_symbol()))
    }};
}

#[macro_export]
macro_rules! read_const1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let c = $self.read_sexp($gc)?;
        Ok(OpOld::$op(c))
    }};
}

#[macro_export]
macro_rules! read_num1 {
    ($self:ident, $gc:ident, $op:ident, $size:ident) => {{
        let m = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number() as $size))
    }};
}

#[macro_export]
macro_rules! read_num2 {
    ($self:ident, $gc:ident, $op:ident, $size:ident, $size2:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number() as $size, n.to_number() as $size2))
    }};
}

#[macro_export]
macro_rules! read_num3 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let m = $self.read_sexp($gc)?;
        let n = $self.read_sexp($gc)?;
        let o = $self.read_sexp($gc)?;
        Ok(OpOld::$op(m.to_number(), n.to_number(), o.to_number()))
    }};
}

// S-expression de-serializer.
pub struct FaslReader<'a> {
    pub bytes: &'a [u8],
    pub shared_objects: &'a mut HashMap<u32, Object>,
}

impl FaslReader<'_> {
    pub fn read_all_sexp(&mut self, gc: &mut Gc) -> Vec<Object> {
        let mut objects = vec![];
        loop {
            match self.read_sexp(gc) {
                Ok(sexp) => {
                    objects.push(sexp);
                }
                Err(_) => {
                    break;
                }
            }
        }
        objects
    }

    pub fn read_sexp(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let tag = self.read_tag()?;
        match tag {
            Tag::Char => self.read_char(),
            Tag::Fixnum => self.read_fixnum(),
            Tag::String => self.read_string(gc),
            Tag::Symbol => self.read_symbol(gc),
            Tag::Pair => self.read_pair(gc),
            Tag::Vector => self.read_vector(gc),
            Tag::True => Ok(Object::True),
            Tag::False => Ok(Object::False),
            Tag::Nil => Ok(Object::Nil),
            Tag::Unspecified => Ok(Object::Unspecified),
            Tag::CompilerInsn => self.read_compiler_insn(),
            Tag::Struct => self.read_struct(gc),
            Tag::EqHashtable => self.read_eq_hashtable(gc),
            Tag::DefineShared => self.read_define_shared(gc),
            Tag::LookupShared => self.read_lookup_shared(gc),
            Tag::Flonum => self.read_float(),
        }
    }

    fn read_compiler_insn(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        Ok(Object::Instruction(
            FromPrimitive::from_u8(buf[0]).expect("unknown Op"),
        ))
    }

    fn read_define_shared(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 4];
        self.bytes.read_exact(&mut buf)?;
        let uid = u32::from_le_bytes(buf);
        let obj = self.read_sexp(gc)?;
        self.shared_objects.insert(uid, obj);
        Ok(obj)
    }

    fn read_lookup_shared(&mut self, _gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 4];
        self.bytes.read_exact(&mut buf)?;
        let uid = u32::from_le_bytes(buf);
        match self.shared_objects.get(&uid) {
            Some(v) => Ok(*v),
            None => todo!(),
        }
    }

    fn read_fixnum(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 8];
        self.bytes.read_exact(&mut buf)?;
        let n = isize::from_le_bytes(buf);
        Ok(Object::Fixnum(n))
    }

    fn read_float(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 8];
        self.bytes.read_exact(&mut buf)?;
        let n = u64::from_le_bytes(buf);
        Ok(Object::Flonum(Flonum::new_from_64(n)))
    }

    fn read_symbol(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut chars = vec![];
        for _ in 0..len {
            let mut buf = [0; 4];
            self.bytes.read_exact(&mut buf)?;
            let n = u32::from_le_bytes(buf);
            match char::from_u32(n) {
                Some(c) => chars.push(c),
                None => return Err(self.create_read_error("invalid char")),
            }
        }
        Ok(gc.symbol_intern(&String::from_iter(chars)))
    }

    fn read_string(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut chars = vec![];
        for _ in 0..len {
            let mut buf = [0; 4];
            self.bytes.read_exact(&mut buf)?;
            let n = u32::from_le_bytes(buf);
            match char::from_u32(n) {
                Some(c) => chars.push(c),
                None => return Err(self.create_read_error("invalid char")),
            }
        }
        Ok(gc.new_string(&String::from_iter(chars)))
    }

    fn read_vector(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut objs = vec![];
        for _ in 0..len {
            objs.push(self.read_sexp(gc)?);
        }
        Ok(gc.new_vector(&objs))
    }

    fn read_struct(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf) as usize;
        let mut objs = vec![];
        for _ in 0..len {
            objs.push(self.read_sexp(gc)?);
        }
        let name = self.read_sexp(gc)?;
        let mut s = SimpleStruct::new(name, len as usize);
        for i in 0..len {
            s.set(i, objs[i]);
        }
        Ok(Object::SimpleStruct(gc.alloc(s)))
    }

    fn read_eq_hashtable(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut t = EqHashtable::new();
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf) as usize;
        for _ in 0..len {
            let key = self.read_sexp(gc)?;
            let value = self.read_sexp(gc)?;
            t.set(key, value);
        }
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        t.is_mutable = buf[0] == 1;
        Ok(Object::EqHashtable(gc.alloc(t)))
    }

    fn read_char(&mut self) -> Result<Object, io::Error> {
        let mut buf = [0; 4];
        self.bytes.read_exact(&mut buf)?;
        let n = u32::from_le_bytes(buf);
        match char::from_u32(n) {
            Some(c) => Ok(Object::Char(c)),
            None => Err(self.create_read_error("invalid char")),
        }
    }

    fn read_pair(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let first = self.read_sexp(gc)?;
        let second = self.read_sexp(gc)?;
        Ok(gc.cons(first, second))
    }

    fn create_read_error(&self, reason: &str) -> io::Error {
        io::Error::new(io::ErrorKind::Other, reason)
    }

    fn read_tag(&mut self) -> Result<Tag, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        Ok(FromPrimitive::from_u8(buf[0]).expect("unknown tag"))
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {
    use std::collections::HashMap;

    use crate::{equal::Equal, gc::Gc, objects::Object};

    use super::FaslReader;

    #[macro_export]
    macro_rules! assert_equal {
        ($gc:ident, $lhs:ident, $rhs:ident) => {{
            let e = Equal::new();
            if e.is_equal(&mut $gc, &$lhs, &$rhs) {
                assert!(true);
            } else {
                println!("{} is not equal to {}", $lhs, $rhs);
                assert!(false);
            }
        }};
    }

    #[test]
    fn test_constant_number() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[0, 3, 0, 0, 0, 0, 0, 0, 0];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = Object::Fixnum(3);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_true() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[1];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = Object::True;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_false() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[2];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = Object::False;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
    #[test]
    fn test_constant_nil() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[3];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = Object::Nil;
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
    #[test]
    fn test_constant_char() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[4, 97, 0, 0, 0];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = Object::Char('a');
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_symbol() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[
            5, 5, 0, 104, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0, 0, 0,
        ];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = gc.symbol_intern("hello");
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_string() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[6, 3, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let expected = gc.new_string("abc");
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }

    #[test]
    fn test_constant_simple_pair() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[7, 5, 1, 0, 97, 0, 0, 0, 3];
        let mut fasl = FaslReader {
            bytes: bytes,
            shared_objects: &mut HashMap::new(),
        };
        let sym = gc.symbol_intern("a");
        let expected = gc.cons(sym, Object::Nil);
        let obj = fasl.read_sexp(&mut gc).unwrap();
        assert_equal!(gc, expected, obj);
    }
}
