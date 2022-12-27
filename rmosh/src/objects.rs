use crate::gc::GcRef;
use crate::gc::{Gc, GcHeader, ObjectType};
use crate::op::Op;
use crate::vm::Vm;
use rand::Rng;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ptr::{null, null_mut};

/// Wrapper of heap allocated or simple stack objects.
#[derive(Copy, Clone, PartialEq, Hash)]
pub enum Object {
    Char(char),
    Closure(GcRef<Closure>),
    Eof,
    EqHashtable(GcRef<EqHashtable>),
    False,
    InputPort(GcRef<InputPort>),
    Nil,
    Number(isize),
    Pair(GcRef<Pair>),
    Procedure(GcRef<Procedure>),
    String(GcRef<SString>),
    Symbol(GcRef<Symbol>),
    True,
    Unspecified,
    StackPointer(*mut Object),
    OpPointer(*const Op),
    Vector(GcRef<Vector>),
    Vox(GcRef<Vox>),
}

impl Object {
    pub fn is_false(&self) -> bool {
        match self {
            Object::False => true,
            _ => false,
        }
    }

    pub fn is_list(&self) -> bool {
        Pair::is_list(*self)
    }

    pub fn is_pair(&self) -> bool {
        match self {
            Object::Pair(_) => true,
            _ => false,
        }
    }

    pub fn is_vox(&self) -> bool {
        match self {
            Object::Vox(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Object::Nil => true,
            _ => false,
        }
    }

    pub fn is_symbol(&self) -> bool {
        match self {
            Object::Symbol(_) => true,
            _ => false,
        }
    }

    pub fn is_unspecified(&self) -> bool {
        match self {
            Object::Unspecified => true,
            _ => false,
        }
    }

    pub fn make_bool(pred: bool) -> Self {
        if pred {
            Object::True
        } else {
            Object::False
        }
    }

    pub fn eq(&self, other: &Self) -> bool {
        self == other
    }

    pub fn to_number(self) -> isize {
        if let Self::Number(n) = self {
            n
        } else {
            panic!("Not a Object::Number")
        }
    }
    pub fn to_vox(self) -> GcRef<Vox> {
        if let Self::Vox(v) = self {
            v
        } else {
            panic!("Not a Object::Vox")
        }
    }

    // TODO: Implement eqv?
    pub fn eqv(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Number(a), Object::Number(b)) => {
                return a == b;
            }
            _ => {
                return self.eq(other);
            }
        }
    }
}

// For HashMap<Object, Object>
impl Eq for Object {}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::InputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::OpPointer(op) => {
                write!(f, "{:?}", *op)
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
            Object::Symbol(symbol) => {
                write!(f, "{}", unsafe { symbol.pointer.as_ref() })
            }
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::StackPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
            }
            Object::Nil => {
                write!(f, "()")
            }
            Object::Procedure(proc) => {
                write!(f, "#<procedure {}>", proc.name)
            }
            Object::Vector(vector) => {
                write!(f, "{}", unsafe { vector.pointer.as_ref() })
            }
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::InputPort(port) => {
                write!(f, "{}", unsafe { port.pointer.as_ref() })
            }
            Object::OpPointer(op) => {
                write!(f, "{:?}", *op)
            }
            Object::Char(c) => {
                write!(f, "{}", c)
            }
            Object::Number(n) => {
                write!(f, "{}", n)
            }
            Object::Vox(obj) => {
                write!(f, "#<vox {}>", obj.value)
            }
            Object::Closure(closure) => {
                write!(f, "#<closure {:?}>", closure.pointer.as_ptr())
            }
            Object::EqHashtable(table) => {
                write!(f, "#<eq-hashtable {:?}>", table.pointer.as_ptr())
            }
            Object::Pair(pair) => {
                write!(f, "{}", unsafe { pair.pointer.as_ref() })
            }
            Object::String(s) => {
                write!(f, "{}", unsafe { s.pointer.as_ref() })
            }
            Object::Symbol(symbol) => {
                write!(f, "{}", unsafe { symbol.pointer.as_ref() })
            }
            Object::Eof => {
                write!(f, "#<eof>")
            }
            Object::True => {
                write!(f, "#t")
            }
            Object::False => {
                write!(f, "#f")
            }
            Object::StackPointer(v) => {
                write!(f, "#<stack pointer {:?}>", v)
            }
            Object::Unspecified => {
                write!(f, "#<unspecified>")
            }
            Object::Nil => {
                write!(f, "()")
            }
            Object::Procedure(proc) => {
                write!(f, "#<procedure {}>", proc.name)
            }
            Object::Vector(vector) => {
                write!(f, "{}", unsafe { vector.pointer.as_ref() })
            }
        }
    }
}

/// Vector
#[derive(Debug)]
pub struct Vector {
    pub header: GcHeader,
    pub data: Vec<Object>,
}

impl Vector {
    pub fn new(data: &Vec<Object>) -> Self {
        Vector {
            header: GcHeader::new(ObjectType::Vector),
            data: data.to_owned(),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[")?;
        for i in 0..self.data.len() {
            write!(f, "{}", self.data[i])?;
            if i != self.data.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}

/// Cons cell
#[derive(Debug)]
pub struct Pair {
    pub header: GcHeader,
    pub car: Object,
    pub cdr: Object,
}

impl Pair {
    pub fn new(first: Object, second: Object) -> Self {
        Pair {
            header: GcHeader::new(ObjectType::Pair),
            car: first,
            cdr: second,
        }
    }

    pub fn is_list(obj: Object) -> bool {
        let mut obj = obj;
        let mut seen = obj;
        loop {
            if obj.is_nil() {
                return true;
            }
            match obj {
                Object::Pair(pair) => {
                    obj = pair.cdr;
                    if obj.is_nil() {
                        return true;
                    }

                    match obj {
                        Object::Pair(pair) => {
                            obj = pair.cdr;
                            match seen {
                                Object::Pair(pair) => {
                                    seen = pair.cdr;
                                    if obj == seen {
                                        // Circular
                                        return false;
                                    }
                                }
                                _ => {
                                    panic!("seen not a pair")
                                }
                            }
                        }
                        _ => {
                            // Dot pair
                            return false;
                        }
                    }
                }
                _ => {
                    // Dot pair.
                    return false;
                }
            }
        }
    }

    fn print_abbreviated(&self, f: &mut fmt::Formatter<'_>) -> bool {
        match self.cdr {
            Object::Pair(cdr) => {
                if !cdr.cdr.is_nil() {
                    return false;
                }
                let car = self.car;
                match car {
                    Object::Symbol(symbol) => {
                        if symbol.string.eq("quote") {
                            match write!(f, "'") {
                                Ok(_) => {
                                    return true;
                                }
                                Err(_) => {
                                    return false;
                                }
                            }
                        }
                        return false;
                    }
                    _ => {
                        return false;
                    }
                }
            }
            _ => {
                return false;
            }
        }
    }

    fn last_pair(p: Object) -> Object {
        let mut o = p;
        loop {
            match o {
                Object::Pair(pair) => {
                    let kdr = pair.cdr;
                    if kdr.is_nil() {
                        return o;
                    } else {
                        o = kdr;
                    }
                }
                _ => {
                    panic!("last_pair: pair requied but got {}", o);
                }
            }
        }
    }

    // append!
    pub fn append_destructive(l1: Object, l2: Object) -> Object {
        if l1.is_nil() {
            return l2;
        }
        let last = Self::last_pair(l1);
        match last {
            Object::Pair(mut pair) => {
                pair.cdr = l2;
                return l1;
            }
            _ => {
                panic!("never reached");
            }
        }
    }
}

impl Display for Pair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let abbreviated = self.print_abbreviated(f);
        let mut e = self.cdr;
        if abbreviated {
            match e {
                Object::Pair(pair) => {
                    let car_str = pair.car.to_string();
                    write!(f, "{}", car_str)?;
                    e = pair.cdr;
                }
                _ => panic!("should not reach"),
            }
        } else {
            let car_str = self.car.to_string();
            write!(f, "({}", car_str)?;
        }

        loop {
            match e {
                Object::Pair(pair) => {
                    write!(f, " ")?;
                    write!(f, "{}", pair.car)?;
                    e = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    write!(f, " . ")?;
                    write!(f, "{}", e)?;
                    break;
                }
            }
        }
        if !abbreviated {
            write!(f, ")")?;
        }
        Ok(())
    }
}

impl PartialEq for Pair {
    fn eq(&self, other: &Self) -> bool {
        (self.car == other.car) && (self.cdr == other.cdr)
    }
}

/// Vox
#[derive(Debug)]
pub struct Vox {
    pub header: GcHeader,
    pub value: Object,
}

impl Vox {
    pub fn new(value: Object) -> Self {
        Vox {
            header: GcHeader::new(ObjectType::Vox),
            value: value,
        }
    }
}

impl Display for Vox {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Vox({})", self.value)
    }
}

/// SString (Sceheme String)
#[derive(Debug)]
pub struct SString {
    pub header: GcHeader,
    pub string: String,
}

impl SString {
    pub fn new(string: &str) -> Self {
        SString {
            header: GcHeader::new(ObjectType::String),
            string: string.to_string(),
        }
    }
}

impl Display for SString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.string)
    }
}

impl PartialEq for SString {
    fn eq(&self, other: &Self) -> bool {
        self.string.eq(&other.string)
    }
}

/// Symbol
#[derive(Debug)]
pub struct Symbol {
    pub header: GcHeader,
    pub string: String,
}

impl Symbol {
    pub fn new(string: String) -> Self {
        Symbol {
            header: GcHeader::new(ObjectType::Symbol),
            string: string,
        }
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.string)
    }
}

/// Procedures written in Rust.
pub struct Procedure {
    pub header: GcHeader,
    pub func: fn(&mut Vm, &[Object]) -> Object,
    pub name: String,
}

impl Procedure {
    pub fn new(func: fn(&mut Vm, &[Object]) -> Object, name: String) -> Self {
        Procedure {
            header: GcHeader::new(ObjectType::Procedure),
            func: func,
            name: name,
        }
    }
}

impl fmt::Debug for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<procedure>")
    }
}

impl Display for Procedure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<procedure>")
    }
}

/// Closure
#[derive(Debug)]
pub struct Closure {
    pub header: GcHeader,
    pub ops: *const Op,
    pub ops_len: usize,
    pub argc: isize,
    pub is_optional_arg: bool,
    pub free_vars: Vec<Object>,
    pub prev: Object,
}

impl Closure {
    pub fn new(
        ops: *const Op,
        ops_len: usize,
        argc: isize,
        is_optional_arg: bool,
        free_vars: Vec<Object>,
    ) -> Self {
        Closure {
            header: GcHeader::new(ObjectType::Closure),
            ops: ops,
            ops_len: ops_len,
            argc: argc,
            is_optional_arg: is_optional_arg,
            free_vars: free_vars,
            prev: Object::Unspecified,
        }
    }

    pub fn refer_free(&mut self, n: usize) -> Object {
        self.free_vars[n]
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure>")
    }
}

/// EqHashtable
#[derive(Debug)]
pub struct EqHashtable {
    pub header: GcHeader,
    pub hash_map: HashMap<Object, Object>,
    pub is_mutable: bool,
}

impl EqHashtable {
    pub fn new() -> Self {
        EqHashtable {
            header: GcHeader::new(ObjectType::EqHashtable),
            hash_map: HashMap::new(),
            is_mutable: true,
        }
    }

    pub fn get(&self, key: Object, default: Object) -> Object {
        match self.hash_map.get(&key) {
            Some(value) => *value,
            _ => default,
        }
    }

    pub fn contains(&self, key: Object) -> bool {
        match self.hash_map.get(&key) {
            Some(_) => true,
            _ => false,
        }
    }

    pub fn set(&mut self, key: Object, value: Object) {
        match self.hash_map.insert(key, value) {
            _ => (),
        }
    }

    pub fn size(&self) -> usize {
        self.hash_map.len()
    }

    pub fn delte(&mut self, key: Object) {
        match self.hash_map.remove(&key) {
            _ => (),
        }
    }

    pub fn is_mutable(&self) -> bool {
        return self.is_mutable;
    }

    pub fn clear(&mut self) {
        self.hash_map.clear()
    }
}

impl Display for EqHashtable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#<eq-hashtable>")
    }
}

/// InputPort
#[derive(Debug)]
pub struct InputPort {
    pub header: GcHeader,
    source: String,
    idx: usize,
}

impl InputPort {
    fn new(source: &str) -> Self {
        InputPort {
            header: GcHeader::new(ObjectType::InputPort),
            source: source.to_owned(),
            idx: 0,
        }
    }
    pub fn open(source: &str) -> std::io::Result<InputPort> {
        Ok(InputPort::new(source))
    }

    pub fn read_char(&mut self) -> Option<char> {
        let mut chars = self.source.chars();
        let ret = chars.nth(self.idx);
        self.idx = self.idx + 1;
        ret
    }
}

impl Display for InputPort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<input port>")
    }
}

pub struct Equal {
    k0: Object,
    kb: Object,
}
impl Equal {
    pub fn new() -> Self {
        Self {
            k0: Object::Number(400),
            kb: Object::Number(-40),
        }
    }
    pub fn is_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
          self.is_precheck_interleave_equal(gc, x, y)
      }

      //   (define (precheck/interleave-equal? x y)
      //     (let ([k (pre? x y k0)])
      //       (and k (or (> k 0) (interleave? x y 0)))))
      fn is_precheck_interleave_equal(&self, gc: &mut Box<Gc>, x: &Object, y: &Object) -> bool {
          let k = self.is_pre(gc, x, y, self.k0);
          if k.is_false() {
              return false;
          }
          if k.to_number() > 0 {
              return true;
          } else {
              self.is_interleave(gc, x, y, Object::Number(0))
          }
      }

      // (define (pre? x y k)
      //     (import UNSAFE)
      //     (cond
      //       [(eq? x y) k]
      //       [(pair? x)
      //        (and (pair? y)
      //             (if (<= k 0)
      //                 k
      //                 (let ([k (pre? (car x) (car y) (- k 1))])
      //                   (and k (pre? (cdr x) (cdr y) k)))))]
      //       [(vector? x)
      //        (and (vector? y)
      //             (let ([n (vector-length x)])
      //               (and (= (vector-length y) n)
      //                    (let f ([i 0] [k k])
      //                      (if (or (= i n) (<= k 0))
      //                          k
      //                          (let ([k (pre?
      //                                     (vector-ref x i)
      //                                     (vector-ref y i)
      //                                     (- k 1))])
      //                            (and k (f (+ i 1) k))))))))]
      //       [(string? x) (and (string? y) (string=? x y) k)]
      //       [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
      //       [else (and (eqv? x y) k)]))
      fn is_pre(&self, gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> Object {
          if x == y {
              return k;
          }
          match (x, y) {
              (Object::Pair(pair1), Object::Pair(pair2)) => {
                  if k.to_number() <= 0 {
                      return k;
                  } else {
                      let k2 = self.is_pre(
                          gc,
                          &pair1.car,
                          &pair2.car,
                          Object::Number(k.to_number() - 1),
                      );
                      if k2.is_false() {
                          return Object::False;
                      }
                      return self.is_pre(gc, &pair1.cdr, &pair2.cdr, k2);
                  }
              }
              (Object::Vector(v1), Object::Vector(v2)) => {
                  let n = v1.len();
                  if v2.len() != n {
                      return Object::False;
                  }
                  let mut i: usize = 0;
                  let mut k = k;
                  loop {
                      if i == n || k.to_number() <= 0 {
                          return k;
                      } else {
                          let k2 = self.is_pre(
                              gc,
                              &v1.data[i],
                              &v2.data[i],
                              Object::Number(k.to_number() - 1),
                          );
                          if k2.is_false() {
                              return Object::False;
                          }
                          i += 1;
                          k = k2;
                      }
                  }
              }
              (Object::String(s1), Object::String(s2)) => {
                  if s1.string.eq(&s2.string) {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
              (Object::Procedure(p1), Object::Procedure(p2)) => {
                  if p1.func as isize == p2.func as isize {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
              _ => {
                  if x.eqv(y) {
                      return k;
                  } else {
                      Object::False
                  }
              }
          }
      }

      //     (define (interleave? x y k)
      //       (and (e? x y k) #t))
      fn is_interleave(&self, gc: &mut Box<Gc>, x: &Object, y: &Object, k: Object) -> bool {
          let hashmap: *mut *mut HashMap<Object, Object> = null_mut();
          if self.is_e(gc, hashmap, x, y, k).is_false() {
              false
          } else {
              true
          }
      }

      //       (define (call-union-find x y)
      //         (unless ht (set! ht (make-eq-hashtable)))
      //         (union-find ht x y))
      fn call_union_find(
          &self,
          gc: &mut Box<Gc>,
          hashmap: *mut *mut HashMap<Object, Object>,
          x: &Object,
          y: &Object,
      ) -> Object {
          if unsafe { *hashmap == null_mut() } {
              unsafe { *hashmap = &mut HashMap::new().to_owned() as *mut HashMap<Object, Object> };
          }
          return self.union_find(gc, unsafe { *hashmap }, x, y);
      }

      // (define (find b)
      //       (let ([n (box-content b)])
      //         (if (box? n)
      //             (let loop ([b b] [n n])
      //               (let ([nn (box-content n)])
      //                 (if (box? nn) (begin (set-box-content! b nn) (loop n nn)) n)))
      //             b)))

      fn find(&self, b: Object) -> Object {
          let mut n = b.to_vox().value;
          if n.is_vox() {
              let mut b = b;
              loop {
                  let nn = n.to_vox().value;
                  if nn.is_vox() {
                      b.to_vox().value = nn;
                      b = n;
                      n = nn;
                  } else {
                      return n;
                  }
              }
          } else {
              return b;
          }
      }

      //   (define (union-find ht x y)
      //     (let ([bx (eq-hashtable-ref ht x #f)]
      //           [by (eq-hashtable-ref ht y #f)])
      //       (if (not bx)
      //           (if (not by)
      //               (let ([b (make-box 1)])
      //                 (eq-hashtable-set! ht x b)
      //                 (eq-hashtable-set! ht y b)
      //                 #f)
      //               (let ([ry (find by)]) (eq-hashtable-set! ht x ry) #f))
      //           (if (not by)
      //               (let ([rx (find bx)]) (eq-hashtable-set! ht y rx) #f)
      //               (let ([rx (find bx)] [ry (find by)])
      //                 (or (eq? rx ry)
      //                     (let ([nx (box-content rx)] [ny (box-content ry)])
      //                       (if (> nx ny)
      //                           (begin
      //                             (set-box-content! ry rx)
      //                             (set-box-content! rx (+ nx ny))
      //                             #f)
      //                           (begin
      //                             (set-box-content! rx ry)
      //                             (set-box-content! ry (+ ny nx))
      //                             #f)))))))))
      fn union_find(
          &self,
          gc: &mut Box<Gc>,
          hashmap: *mut HashMap<Object, Object>,
          x: &Object,
          y: &Object,
      ) -> Object {
          let hm: &mut HashMap<Object, Object> = unsafe { hashmap.as_mut().unwrap() };
          let bx = match hm.get(x) {
              Some(v) => *v,
              None => Object::False,
          };
          let by = match hm.get(y) {
              Some(v) => *v,
              None => Object::False,
          };
          if bx.is_false() {
              if by.is_false() {
                  let b = Object::Vox(gc.alloc(Vox::new(Object::Number(1))));
                  hm.insert(*x, b);
                  hm.insert(*y, b);
                  return Object::False;
              } else {
                  let ry = self.find(by);
                  hm.insert(*x, ry);
                  return Object::False;
              }
          } else if by.is_false() {
              let rx = self.find(bx);
              hm.insert(*y, rx);
              return Object::False;
          } else {
              let rx = self.find(bx);
              let ry = self.find(by);
              if rx == rx {
                  return Object::True;
              }
              let nx = rx.to_vox().value;
              let ny = ry.to_vox().value;
              if nx.to_number() > ny.to_number() {
                  ry.to_vox().value = rx;
                  rx.to_vox().value = Object::Number(nx.to_number() + ny.to_number());
                  return Object::False;
              } else {
                  rx.to_vox().value = ry;
                  ry.to_vox().value = Object::Number(ny.to_number() + nx.to_number());
                  return Object::False;
              }
          }
      }

      //       (define (e? x y k)
      //         (if (<= k 0)
      //             (if (= k kb) (fast? x y (random (* 2 k0))) (slow? x y k))
      //             (fast? x y k)))
      fn is_e(
          &self,
          gc: &mut Box<Gc>,
          hashmap: *mut *mut HashMap<Object, Object>,
          x: &Object,
          y: &Object,
          k: Object,
      ) -> Object {
          if k.to_number() <= 0 {
              if k == self.kb {
                  let mut rng = rand::thread_rng();
                  let next_k = Object::Number(rng.gen::<isize>() % (2 * self.k0.to_number()));
                  return self.is_fast(gc, hashmap, &x, &y, next_k);
              } else {
                  return self.is_slow(gc, hashmap, x, y, k);
              }
          } else {
              return self.is_fast(gc, hashmap, x, y, k);
          }
      }

      // (define (slow? x y k)
      //         (cond
      //           [(eq? x y) k]
      //           [(pair? x)
      //            (and (pair? y)
      //                 (if (call-union-find x y)
      //                     0
      //                     (let ([k (e? (car x) (car y) (- k 1))])
      //                       (and k (e? (cdr x) (cdr y) k)))))]
      //           [(vector? x)
      //            (and (vector? y)
      //                 (let ([n (vector-length x)])
      //                   (and (= (vector-length y) n)
      //                        (if (call-union-find x y)
      //                            0
      //                            (let f ([i 0] [k (- k 1)])
      //                              (if (= i n)
      //                                  k
      //                                  (let ([k (e? (vector-ref x i)
      //                                               (vector-ref y i)
      //                                               k)])
      //                                    (and k (f (+ i 1) k)))))))))]
      //           [(string? x) (and (string? y) (string=? x y) k)]
      //           [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
      //           [else (and (eqv? x y) k)]))
      fn is_slow(
          &self,
          gc: &mut Box<Gc>,
          hashmap: *mut *mut HashMap<Object, Object>,
          x: &Object,
          y: &Object,
          k: Object,
      ) -> Object {
          if x == y {
              return k;
          }
          match (x, y) {
              (Object::Pair(pair1), Object::Pair(pair2)) => {
                  if !self.call_union_find(gc, hashmap, x, y).is_false() {
                      return Object::Number(0);
                  } else {
                      let k = self.is_e(
                          gc,
                          hashmap,
                          &pair1.car,
                          &pair2.car,
                          Object::Number(k.to_number() - 1),
                      );
                      if k.is_false() {
                          return Object::False;
                      }
                      return self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k);
                  }
              }
              (Object::Vector(v1), Object::Vector(v2)) => {
                  let n = v1.len();
                  if v2.len() != n {
                      return Object::False;
                  }
                  if !self.call_union_find(gc, hashmap, x, y).is_false() {
                      return Object::Number(0);
                  }
                  let mut i: usize = 0;
                  let mut k = k;
                  loop {
                      if i == n {
                          return k;
                      }
                      k = self.is_e(gc, hashmap, &v1.data[i], &v2.data[i], k);
                      if k.is_false() {
                          return Object::False;
                      }
                      i += 1;
                  }
              }
              (Object::String(s1), Object::String(s2)) => {
                  if s1.string.eq(&s2.string) {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
              (Object::Procedure(p1), Object::Procedure(p2)) => {
                  if p1.func as isize == p2.func as isize {
                      return Object::True;
                  } else {
                      return Object::False;
                  }
              }
              _ => {
                  if x.eqv(&y) {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
          }
      }

      // (define (fast? x y k)
      //         (let ([k (- k 1)])
      //           (cond
      //             [(eq? x y) k]
      //             [(pair? x)
      //              (and (pair? y)
      //                   (let ([k (e? (car x) (car y) k)])
      //                     (and k (e? (cdr x) (cdr y) k))))]
      //             [(vector? x)
      //              (and (vector? y)
      //                   (let ([n (vector-length x)])
      //                     (and (= (vector-length y) n)
      //                          (let f ([i 0] [k k])
      //                            (if (= i n)
      //                                k
      //                                (let ([k (e? (vector-ref x i)
      //                                             (vector-ref y i)
      //                                             k)])
      //                                  (and k (f (+ i 1) k))))))))]
      //             [(string? x) (and (string? y) (string=? x y) k)]
      //             [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
      //             [else (and (eqv? x y) k)])))
      fn is_fast(
          &self,
          gc: &mut Box<Gc>,
          hashmap: *mut *mut HashMap<Object, Object>,
          x: &Object,
          y: &Object,
          k: Object,
      ) -> Object {
          let mut k = Object::Number(k.to_number() - 1);
          if x == y {
              return k;
          }
          match (x, y) {
              (Object::Pair(pair1), Object::Pair(pair2)) => {
                  k = self.is_e(gc, hashmap, &pair1.car, &pair2.car, k);
                  if k.is_false() {
                      return Object::False;
                  }
                  return self.is_e(gc, hashmap, &pair1.cdr, &pair2.cdr, k);
              }
              (Object::Vector(v1), Object::Vector(v2)) => {
                  let n = v1.len();
                  if v2.len() != n {
                      return Object::False;
                  }
                  let mut i: usize = 0;
                  let mut k = k;
                  loop {
                      if i == n {
                          return k;
                      }
                      k = self.is_e(gc, hashmap, &v1.data[i], &v2.data[i], k);
                      if k.is_false() {
                          return Object::False;
                      }
                      i += 1;
                  }
              }
              (Object::String(s1), Object::String(s2)) => {
                  if s1.string.eq(&s2.string) {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
              (Object::Procedure(p1), Object::Procedure(p2)) => {
                  if p1.func as isize == p2.func as isize {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
              _ => {
                  if x.eqv(y) {
                      return k;
                  } else {
                      return Object::False;
                  }
              }
          }
      }

      fn is_equal_fast(x: &Object, y: &Object) -> bool {
          let mut obj1 = x;
          let mut obj2 = y;
          loop {
              'inner: loop {
                  if obj1 == obj2 {
                      return true;
                  }
                  match (obj1, obj2) {
                      (Object::Pair(pair1), Object::Pair(pair2)) => {
                          if Self::is_equal_fast(&pair1.car, &pair2.car) {
                              obj1 = &pair1.cdr;
                              obj2 = &pair2.cdr;
                              break 'inner;
                          }
                      }
                      (Object::Number(n1), Object::Number(n2)) => {
                          return n1 == n2;
                      }
                      (Object::Vector(v1), Object::Vector(v2)) => {
                          if v1.len() == v2.len() {
                              for i in 0..v1.len() {
                                  if !Self::is_equal_fast(&v1.data[i], &v2.data[i]) {
                                      return false;
                                  }
                              }
                              return true;
                          } else {
                              return false;
                          }
                      }
                      (Object::String(s1), Object::String(s2)) => {
                          return s1.string.eq(&s2.string);
                      }
                      (Object::Procedure(p1), Object::Procedure(p2)) => {
                          return p1.func as isize == p2.func as isize;
                      }
                      _ => {
                          return obj1.eqv(obj2);
                      }
                  }
              }
          }
    
}
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use std::{mem, ptr::NonNull};

    use super::*;
    use crate::gc::Gc;
    use regex::Regex;

    // Helpers.
    fn procedure1(_vm: &mut Vm, args: &[Object]) -> Object {
        assert_eq!(args.len(), 1);
        args[0]
    }

    /*
    #[test]
    fn test_input_port() {
        match InputPort::open("file_not_exists") {
            Ok(_) => {}
            Err(e) => {
                println!("port error {:?}", e);
            }
        }
    }
    */

    #[test]
    fn test_symbol() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("define".to_owned()));
        let symbol = Object::Symbol(symbol);
        match symbol {
            Object::Symbol(s) => {
                assert_eq!(s.string, "define");
            }
            _ => {
                panic!("not a symbo");
            }
        }
    }

    #[test]
    fn test_procedure() {
        let mut vm = Vm::new();
        let p = vm.gc.alloc(Procedure::new(procedure1, "proc1".to_owned()));
        let stack = [Object::Number(1), Object::Number(2)];
        match (p.func)(&mut vm, &stack[0..1]) {
            Object::Number(1) => {}
            _ => {
                panic!("Wrong return value");
            }
        }
    }

    #[test]
    fn test_simple_to_string() {
        assert_eq!("101", Object::Number(101).to_string());
        assert_eq!("#t", Object::True.to_string());
        assert_eq!("#f", Object::False.to_string());
        assert_eq!("()", Object::Nil.to_string());
        assert_eq!("#<unspecified>", Object::Unspecified.to_string());
    }

    #[test]
    fn test_symbol_to_string() {
        let mut gc = Gc::new();
        let symbol = gc.alloc(Symbol::new("hello".to_owned()));
        let symbol = Object::Symbol(symbol);
        println!("hoge symbol{:?}", symbol);
        assert_eq!("hello", symbol.to_string());
    }

    #[test]
    fn test_closure_to_string() {
        let mut gc = Gc::new();
        let closure = gc.alloc(Closure::new([].as_ptr(), 0, 0, false, vec![]));
        let closure = Object::Closure(closure);

        let re = Regex::new(r"^#<closure\s[^>]+>$").unwrap();
        assert!(re.is_match(&closure.to_string()));
    }

    #[test]
    fn test_procedure_to_string() {
        let mut gc = Gc::new();
        let proc = gc.alloc(Procedure::new(procedure1, "number?".to_owned()));
        let proc = Object::Procedure(proc);
        assert_eq!("#<procedure number?>", proc.to_string());
    }

    #[test]
    fn test_stack_pointer_to_string() {
        let obj = Object::Number(10);
        let pointer: *mut Object = &obj as *const Object as *mut Object;
        let stack_pointer = Object::StackPointer(pointer);
        let re = Regex::new(r"^#<stack pointer\s[^>]+>$").unwrap();
        assert!(re.is_match(&stack_pointer.to_string()));
    }

    #[test]
    fn test_vox_to_string() {
        let mut gc = Gc::new();
        let vox = gc.alloc(Vox::new(Object::Number(101)));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox 101>", vox.to_string());

        let symbol = gc.alloc(Symbol::new("my-symbol".to_owned()));
        let symbol = Object::Symbol(symbol);
        let vox = gc.alloc(Vox::new(symbol));
        let vox = Object::Vox(vox);
        assert_eq!("#<vox my-symbol>", vox.to_string());
    }

    #[test]
    fn test_dot_pair_to_string() {
        let mut gc = Gc::new();
        let pair = gc.cons(Object::Number(1), Object::Number(2));
        assert_eq!("(1 . 2)", pair.to_string());
    }

    #[test]
    fn test_simple_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Number(2), Object::Nil);
        let pair2 = gc.cons(Object::Number(1), pair1);
        assert_eq!("(1 2)", pair2.to_string());
    }

    #[test]
    fn test_pair_to_string() {
        let mut gc = Gc::new();
        let pair1 = gc.cons(Object::Number(3), Object::Nil);
        let pair2 = gc.cons(Object::Number(2), pair1);
        let pair3 = gc.cons(Object::Number(1), pair2);
        assert_eq!("(1 2 3)", pair3.to_string());
    }

    #[test]
    fn test_quote_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let pair = gc.list2(sym_quote, sym_a);
        assert_eq!("'a", pair.to_string());
    }

    #[test]
    fn test_quote_list_to_string() {
        let mut gc = Gc::new();
        let sym_quote = gc.symbol_intern("quote");
        let sym_a = gc.symbol_intern("a");
        let sym_b = gc.symbol_intern("b");
        let list = gc.list2(sym_a, sym_b);
        let pair = gc.list2(sym_quote, list);
        assert_eq!("'(a b)", pair.to_string());
    }

    #[test]
    fn test_sstring_eq() {
        let a = SString::new("abc");
        let b = SString::new("abc");
        assert_eq!(a, b);
    }

    #[test]
    fn test_vector_to_string() {
        let mut gc = Gc::new();
        let data = vec![Object::Number(1), Object::Number(2)];
        let v = gc.new_vector(&data);
        assert_eq!("#[1, 2]", v.to_string());
    }

    trait InputPort {
        fn read(&self);
    }

    struct StringInputPort {}

    impl Drop for StringInputPort {
        fn drop(&mut self) {
            println!("StringInputPort dropped");
        }
    }

    impl InputPort for StringInputPort {
        fn read(&self) {
            println!("StringInputPort::read called");
        }
    }

    fn do_something(input_port: &mut dyn InputPort) {
        // Do something with input_port.
        // We don't care if it is StringInputPort or not.
        input_port.read();

        // Now we are free the object as InputPort.
        let pointer = input_port as *mut dyn InputPort;
        unsafe { drop(Box::from_raw(pointer)) }
    }

    #[test]
    fn test_string_input_port() {
        let boxed = Box::new(StringInputPort {});
        // The GC manages the pointer going forward.
        let pointer = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };
        let mut string_input_port: NonNull<StringInputPort> =
            unsafe { mem::transmute(pointer.as_ref()) };

        // Call read() as StringInputPort
        unsafe { string_input_port.as_ref().read() };

        // Use the object as InputPort.
        unsafe { do_something(string_input_port.as_mut()) };
    }

    #[test]
    fn test_ref_ref() {
        {
            let mut a: isize = 3;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let a_ref: &mut isize = &mut a;
            *a_ref = 4;
            assert_eq!(4, *a_ref);
        }

        {
            let a: isize = 3;
            let b: isize = 4;

            let a_ref: &isize = &a;
            assert_eq!(3, *a_ref);

            let b_ref: &isize = &b;
            assert_eq!(4, *b_ref);

            let a_ref_ref: &&isize = &&a;
            assert_eq!(3, **a_ref_ref);

            let ref_ref: &mut &isize = &mut &a;
            assert_eq!(3, **ref_ref);
            *ref_ref = &b;
            assert_eq!(4, **ref_ref);
        }

    }
}
