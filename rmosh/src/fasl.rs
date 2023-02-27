use std::{
    collections::HashMap,
    io::{self, Cursor, Read},
};

use num_bigint::{Sign, BigInt};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use crate::{
    gc::Gc,
    numbers::{Flonum, Ratnum, Compnum, Bignum},
    objects::{EqHashtable, Object, SimpleStruct},
    ports::BinaryOutputPort,
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
    Bytevector = 16,
    Eof = 17,
    Ratnum = 18,
    Compnum = 19,
    Bignum = 20,
}

// S-expression serializer.
pub struct FaslWriter {}

impl FaslWriter {
    pub fn new() -> Self {
        Self {}
    }
    pub fn write(&self, port: &mut dyn BinaryOutputPort, obj: Object) -> Result<(), io::Error> {
        let mut seen: HashMap<Object, Object> = HashMap::new();
        self.scan(obj, &mut seen);
        let mut shared_id = 1;
        self.write_one(port, &mut seen, &mut shared_id, obj)
    }
    pub fn write_one(
        &self,
        port: &mut dyn BinaryOutputPort,
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
        } else if seen_state.is_fixnum() {
            self.put_tag(port, Tag::LookupShared)?;
            port.put_u32(seen_state.to_isize() as u32)?;
            return Ok(());
        }
        match obj {
            Object::Bytevector(bv) => {
                self.put_tag(port, Tag::Bytevector)?;
                port.put_u16(bv.len() as u16)?;
                for e in bv.data.iter() {
                    port.put_u8(*e)?;
                }
            }
            Object::BytevectorInputPort(_) => todo!(),
            Object::BytevectorOutputPort(_) => todo!(),
            Object::Char(c) => {
                self.put_tag(port, Tag::Char)?;
                port.put_u32(c as u32)?;
            }
            Object::Closure(_) => todo!(),
            Object::Eof => {
                self.put_tag(port, Tag::Eof)?;
            }
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
            Object::Bignum(b) => {
                self.put_tag(port, Tag::Bignum)?;                
                let (sign, bytes) = b.value.to_bytes_le();
                let sign = if sign == Sign::Minus { 1 } else if sign == Sign::NoSign { 2 } else { 3};
                port.put_u8(sign);
                port.put_u16(bytes.len() as u16)?;
                port.write(&bytes)?;
            }
            Object::Compnum(c) => {
                self.put_tag(port, Tag::Compnum)?;
                self.write_one(port, seen, shared_id, c.real)?;
                self.write_one(port, seen, shared_id, c.imag)?;
            }
            Object::Continuation(_r) => {
                todo!();
            }
            Object::ContinuationStack(_r) => {
                todo!();
            }
            Object::Ratnum(r) => {
                self.put_tag(port, Tag::Ratnum)?;
                port.put_i64(*r.ratio.numer())?;
                port.put_i64(*r.ratio.denom())?;
            }
            Object::Regexp(_r) => {
                todo!()
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
            Object::DefinedShared(_) => todo!(),
        }
        Ok(())
    }

    fn put_tag(&self, port: &mut dyn BinaryOutputPort, tag: Tag) -> Result<(), io::Error> {
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
                Object::DefinedShared(_) => todo!(),
            }
        }
    }
}

#[macro_export]
macro_rules! read_sym_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let sym = $self.read($gc)?;
        let m = $self.read($gc)?;
        Ok(OpOld::$op(sym.to_symbol(), m.to_number()))
    }};
}

#[macro_export]
macro_rules! read_num_constant {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read($gc)?;
        let c = $self.read($gc)?;
        Ok(OpOld::$op(n.to_number(), c))
    }};
}

#[macro_export]
macro_rules! read_num_constant_num {
    ($self:ident, $gc:ident, $op:ident) => {{
        let n = $self.read($gc)?;
        let c = $self.read($gc)?;
        let o = $self.read($gc)?;
        Ok(OpOld::$op(n.to_number(), c, o.to_number()))
    }};
}

#[macro_export]
macro_rules! read_sym1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let s = $self.read($gc)?;
        Ok(OpOld::$op(s.to_symbol()))
    }};
}

#[macro_export]
macro_rules! read_const1 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let c = $self.read($gc)?;
        Ok(OpOld::$op(c))
    }};
}

#[macro_export]
macro_rules! read_num1 {
    ($self:ident, $gc:ident, $op:ident, $size:ident) => {{
        let m = $self.read($gc)?;
        Ok(OpOld::$op(m.to_number() as $size))
    }};
}

#[macro_export]
macro_rules! read_num2 {
    ($self:ident, $gc:ident, $op:ident, $size:ident, $size2:ident) => {{
        let m = $self.read($gc)?;
        let n = $self.read($gc)?;
        Ok(OpOld::$op(m.to_number() as $size, n.to_number() as $size2))
    }};
}

#[macro_export]
macro_rules! read_num3 {
    ($self:ident, $gc:ident, $op:ident) => {{
        let m = $self.read($gc)?;
        let n = $self.read($gc)?;
        let o = $self.read($gc)?;
        Ok(OpOld::$op(m.to_number(), n.to_number(), o.to_number()))
    }};
}

// S-expression de-serializer.
pub struct FaslReader {
    bytes: Cursor<Vec<u8>>,
    shared_objects: HashMap<u32, Object>,
    link_needed: bool,
}

impl FaslReader {
    pub fn new(bytes: &[u8]) -> Self {
        Self {
            bytes: Cursor::new(bytes.to_vec()),
            shared_objects: HashMap::new(),
            link_needed: false,
        }
    }
    pub fn read_all_sexp(&mut self, gc: &mut Gc) -> Vec<Object> {
        let mut objects = vec![];
        loop {
            match self.read(gc) {
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

    pub fn read(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let obj = self.read_sexp(gc)?;
        if self.link_needed {
            let mut seen: HashMap<Object, bool> = HashMap::new();
            self.link_shared(&mut seen, obj);
        }
        Ok(obj)
    }

    fn link_shared(&self, seen: &mut HashMap<Object, bool>, obj: Object) {
        println!("link shared");
        match seen.get(&obj) {
            Some(_) => return,
            None => {
                seen.insert(obj, true);
                match obj {
                    Object::Pair(mut p) => {
                        if let Object::DefinedShared(index) = p.car {
                            match self.shared_objects.get(&index) {
                                Some(v) => {
                                    p.car = *v;
                                }
                                None => panic!(),
                            }
                        } else {
                            self.link_shared(seen, p.car);
                        }
                        if let Object::DefinedShared(index) = p.cdr {
                            match self.shared_objects.get(&index) {
                                Some(v) => {
                                    p.cdr = *v;
                                }
                                None => panic!(),
                            }
                        } else {
                            self.link_shared(seen, p.cdr);
                        }
                    }
                    Object::Vector(mut v) => {
                        for i in 0..v.len() {
                            let obj = v.data[i];
                            if let Object::DefinedShared(index) = obj {
                                match self.shared_objects.get(&index) {
                                    Some(value) => {
                                        v.data[i] = *value;
                                    }
                                    None => panic!(),
                                }
                            } else {
                                self.link_shared(seen, obj);
                            }
                        }
                    }
                    Object::SimpleStruct(mut st) => {
                        for i in 0..st.len() {
                            let obj = st.field(i);
                            if let Object::DefinedShared(index) = obj {
                                match self.shared_objects.get(&index) {
                                    Some(value) => {
                                        st.set(i, *value);
                                    }
                                    None => panic!(),
                                }
                            } else {
                                self.link_shared(seen, obj);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn read_sexp(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
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
            Tag::Bytevector => self.read_bytevector(gc),
            Tag::Eof => Ok(Object::Eof),
            Tag::Ratnum => self.read_ratnum(gc),
            Tag::Compnum => self.read_compnum(gc),
            Tag::Bignum => self.read_bignum(gc),
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
            None => {
                self.link_needed = true;
                Ok(Object::DefinedShared(uid))
            }
        }
    }

    fn read_bignum(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 1];
        self.bytes.read_exact(&mut buf)?;
        let sign = buf[0]        ;
        let sign =  if sign == 1 { Sign::Minus } else if sign == 2 { Sign::NoSign } else { Sign::Plus};
        
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);

        let mut buf = vec![0; len as usize];
        self.bytes.read_exact(&mut buf)?;        
        let bigint = BigInt::from_bytes_le(sign, &buf);
        Ok(Object::Bignum(gc.alloc(Bignum::new(bigint))))
    }

    fn read_ratnum(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 8];
        self.bytes.read_exact(&mut buf)?;
        let numer = isize::from_le_bytes(buf);
        let denom = isize::from_le_bytes(buf);
        Ok(Object::Ratnum(gc.alloc(Ratnum::new(numer, denom))))
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

    fn read_bytevector(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let mut buf = [0; 2];
        self.bytes.read_exact(&mut buf)?;
        let len = u16::from_le_bytes(buf);
        let mut vu8 = vec![];
        let mut buf = [0; 1];

        for _ in 0..len {
            self.bytes.read_exact(&mut buf)?;
            vu8.push(buf[0]);
        }
        Ok(gc.new_bytevector_u8(&vu8))
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

    fn read_compnum(&mut self, gc: &mut Gc) -> Result<Object, io::Error> {
        let real = self.read_sexp(gc)?;
        let imag = self.read_sexp(gc)?;
        Ok(Object::Compnum(gc.alloc(Compnum::new(real, imag))))
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
    use crate::{
        equal::Equal,
        gc::Gc,
        numbers::Flonum,
        objects::{Object, SimpleStruct},
        op::Op,
        ports::{BinaryOutputPort, BytevectorOutputPort},
    };

    use super::{FaslReader, FaslWriter};

    #[macro_export]
    macro_rules! assert_equal {
        ($gc:expr, $lhs:expr, $rhs:expr) => {{
            let e = Equal::new();
            if e.is_equal($gc, &$lhs, &$rhs) {
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
        let mut fasl = FaslReader::new(bytes);
        let expected = Object::Fixnum(3);
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    #[test]
    fn test_constant_true() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[1];
        let mut fasl = FaslReader::new(bytes);
        let expected = Object::True;
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    #[test]
    fn test_constant_false() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[2];
        let mut fasl = FaslReader::new(bytes);
        let expected = Object::False;
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }
    #[test]
    fn test_constant_nil() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[3];
        let mut fasl = FaslReader::new(bytes);
        let expected = Object::Nil;
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }
    #[test]
    fn test_constant_char() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[4, 97, 0, 0, 0];
        let mut fasl = FaslReader::new(bytes);
        let expected = Object::Char('a');
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    #[test]
    fn test_constant_symbol() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[
            5, 5, 0, 104, 0, 0, 0, 101, 0, 0, 0, 108, 0, 0, 0, 108, 0, 0, 0, 111, 0, 0, 0,
        ];
        let mut fasl = FaslReader::new(bytes);
        let expected = gc.symbol_intern("hello");
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    #[test]
    fn test_constant_string() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[6, 3, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0];
        let mut fasl = FaslReader::new(bytes);
        let expected = gc.new_string("abc");
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    #[test]
    fn test_constant_simple_pair() {
        let mut gc = Box::new(Gc::new());
        let bytes: &[u8] = &[7, 5, 1, 0, 97, 0, 0, 0, 3];
        let mut fasl = FaslReader::new(bytes);
        let sym = gc.symbol_intern("a");
        let expected = gc.cons(sym, Object::Nil);
        let obj = fasl.read(&mut gc).unwrap();
        assert_equal!(&mut gc, expected, obj);
    }

    fn test_read_write(gc: &mut Box<Gc>, obj: Object) {
        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, obj).unwrap();
        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(gc).unwrap();
        assert_equal!(gc, read_obj, obj);
    }

    #[test]
    fn test_read_write_fixnum() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::Fixnum(123456789));
    }

    #[test]
    fn test_read_write_flonum() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::Flonum(Flonum::new(1.234)));
    }

    #[test]
    fn test_read_write_symbol() {
        let mut gc = Box::new(Gc::new());
        let symbol = gc.symbol_intern("hello");
        test_read_write(&mut gc, symbol);
    }

    #[test]
    fn test_read_write_string() {
        let mut gc = Box::new(Gc::new());
        let s = gc.new_string("Hello");
        test_read_write(&mut gc, s);
    }

    #[test]
    fn test_read_write_char() {
        let mut gc = Box::new(Gc::new());
        let c = Object::Char('a');
        test_read_write(&mut gc, c);
    }

    #[test]
    fn test_read_write_pair() {
        let mut gc = Box::new(Gc::new());
        let p = gc.cons(Object::Char('a'), Object::Fixnum(1234));
        test_read_write(&mut gc, p);
    }

    #[test]
    fn test_read_write_shared_pair() {
        let mut gc = Box::new(Gc::new());

        let p = gc.cons(Object::Char('a'), Object::Fixnum(1234));
        p.to_pair().car = p;
        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, p).unwrap();
        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let x = read_obj.cdr_unchecked();
        let y = p.cdr_unchecked();
        assert_equal!(&mut gc, x, y);
        assert_eq!(read_obj.car_unchecked(), read_obj);
    }

    #[test]
    fn test_read_write_true() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::True);
    }
    #[test]
    fn test_read_write_false() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::False);
    }

    #[test]
    fn test_read_write_bv() {
        let mut gc = Box::new(Gc::new());
        let bv = gc.new_bytevector_u8(&vec![3, 5, 8]);
        test_read_write(&mut gc, bv);
    }

    #[test]
    fn test_read_write_eof() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::Eof);
    }

    #[test]
    fn test_read_write_nil() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::Nil);
    }

    #[test]
    fn test_read_write_vector() {
        let mut gc = Box::new(Gc::new());
        let v = gc.new_vector(&vec![
            Object::Fixnum(1234),
            Object::Flonum(Flonum::new(1.23)),
        ]);
        test_read_write(&mut gc, v);
    }

    #[test]
    fn test_read_write_vector_shared() {
        let mut gc = Box::new(Gc::new());
        let v = gc.new_vector(&vec![
            Object::Fixnum(1234),
            Object::Flonum(Flonum::new(1.23)),
        ]);
        v.to_vector().data[0] = v;

        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, v).unwrap();
        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let x = read_obj.to_vector().data[1];
        let y = v.to_vector().data[1];
        assert_equal!(&mut gc, x, y);
        assert_eq!(read_obj.to_vector().data[0], read_obj);
    }

    #[test]
    fn test_read_write_instruction() {
        let mut gc = Box::new(Gc::new());
        test_read_write(&mut gc, Object::Instruction(Op::Return));
    }

    #[test]
    fn test_read_write_struct() {
        let mut gc = Box::new(Gc::new());
        let name = gc.symbol_intern("struct_name");
        let mut st = gc.alloc(SimpleStruct::new(name, 3));
        st.set(0, Object::Fixnum(1234));
        st.set(1, Object::Unspecified);
        st.set(2, gc.symbol_intern("hoge"));
        let obj = Object::SimpleStruct(st);

        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, obj).unwrap();

        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let st2 = read_obj.to_simple_struct();
        assert_eq!(st.len(), st2.len());
        assert_equal!(&mut gc, st.name, st2.name);
        assert_equal!(&mut gc, st.field(0), st2.field(0));
        assert_equal!(&mut gc, st.field(1), st2.field(1));
        assert_equal!(&mut gc, st.field(2), st2.field(2));
    }

    #[test]
    fn test_read_write_struct_shared() {
        let mut gc = Box::new(Gc::new());
        let name = gc.symbol_intern("struct_name");
        let mut st = gc.alloc(SimpleStruct::new(name, 3));
        let p = gc.cons(Object::Fixnum(1234), Object::Fixnum(5678));
        st.set(0, p);
        st.set(1, p);
        st.set(2, gc.symbol_intern("hoge"));
        let obj = Object::SimpleStruct(st);

        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, obj).unwrap();

        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let st2 = read_obj.to_simple_struct();
        assert_eq!(st.len(), st2.len());
        assert_equal!(&mut gc, st.name, st2.name);
        assert_equal!(&mut gc, st.field(0), st2.field(0));
        assert_equal!(&mut gc, st.field(1), st2.field(1));
        assert_equal!(&mut gc, st.field(2), st2.field(2));
    }

    #[test]
    fn test_read_write_struct_shared2() {
        let mut gc = Box::new(Gc::new());
        let name = gc.symbol_intern("struct_name");
        let mut st = gc.alloc(SimpleStruct::new(name, 3));
        let p = gc.cons(Object::Fixnum(1234), Object::Fixnum(5678));
        p.to_pair().car = p;
        st.set(0, p);
        st.set(1, p);
        st.set(2, gc.symbol_intern("hoge"));
        let obj = Object::SimpleStruct(st);

        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, obj).unwrap();

        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let st2 = read_obj.to_simple_struct();
        assert_eq!(st.len(), st2.len());
        assert_equal!(&mut gc, st.name, st2.name);
        assert_equal!(&mut gc, st.field(2), st2.field(2));
    }

    #[test]
    fn test_read_write_eq_hashtable() {
        let mut gc = Box::new(Gc::new());

        let hashtable = gc.new_eq_hashtable();
        hashtable
            .to_eq_hashtable()
            .set(Object::Fixnum(1356), gc.symbol_intern("hello"));
        hashtable
            .to_eq_hashtable()
            .set(Object::Fixnum(1357), gc.symbol_intern("hello2"));
        let mut port = BytevectorOutputPort::new();
        let bport: &mut dyn BinaryOutputPort = &mut port;
        let writer = FaslWriter::new();
        writer.write(bport, hashtable).unwrap();
        let mut reader = FaslReader::new(&port.data);
        let read_obj = reader.read(&mut gc).unwrap();
        let v1 = read_obj
            .to_eq_hashtable()
            .get(Object::Fixnum(1356), Object::False);
        let v2 = hashtable
            .to_eq_hashtable()
            .get(Object::Fixnum(1356), Object::True);
        assert_equal!(&mut gc, v1, v2);

        let v1 = read_obj
            .to_eq_hashtable()
            .get(Object::Fixnum(1357), Object::False);
        let v2 = hashtable
            .to_eq_hashtable()
            .get(Object::Fixnum(1357), Object::True);
        assert_equal!(&mut gc, v1, v2);
    }
}
