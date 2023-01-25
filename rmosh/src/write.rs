use std::collections::HashMap;

use crate::{
    gc::GcRef,
    objects::{Object, Pair},
};

pub struct Writer {
    accumulated: String,
    seen: HashMap<Object, Object>,
    shared_id: isize,
}

// TODO: We can probably make write and display static function.
impl Writer {
    pub fn new() -> Self {
        Self {
            accumulated: String::new(),
            seen: HashMap::new(),
            shared_id: 1,
        }
    }

    pub fn display(&mut self, obj: Object) {
        self.scan(obj);
        self.display_one(obj);
    }

    fn display_one(&mut self, obj: Object) {
        let seen_state = match self.seen.get(&obj) {
            Some(val) => *val,
            None => Object::False,
        };
        if seen_state.is_true() {
            self.seen.insert(obj, Object::Number(self.shared_id));
            self.put_string(&format!("#{}=", self.shared_id));
            self.shared_id += 1;
        } else if seen_state.is_number() {
            self.put_string(&format!("#{}#", seen_state.to_number()));
            return;
        }
        match obj {
            Object::Pair(p) => self.display_pair(p),
            Object::Vector(_) => todo!(),
            Object::SimpleStruct(_) => todo!(),
            Object::ByteVector(_)
            | Object::Closure(_)
            | Object::Vox(_)
            | Object::ProgramCounter(_)
            | Object::ObjectPointer(_)
            | Object::Unspecified
            | Object::True
            | Object::Procedure(_)
            | Object::Char(_)
            | Object::EqHashtable(_)
            | Object::False
            | Object::Float(_)
            | Object::StringInputPort(_)
            | Object::FileInputPort(_)
            | Object::Eof
            | Object::FileOutputPort(_)
            | Object::StringOutputPort(_)
            | Object::Instruction(_)
            | Object::Nil
            | Object::Symbol(_)
            | Object::String(_)
            | Object::Number(_) => self.as_display(obj),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.accumulated
    }

    fn as_display(&mut self, obj: Object) {
        self.put_string(&format!("{}", obj));
    }

    fn put_string(&mut self, s: &str) {
        self.accumulated.push_str(s);
    }

    fn display_pair(&mut self, p: GcRef<Pair>) {
        self.put_string("(");
        self.display_one(p.car);

        let mut obj = p.cdr;
        loop {
            let seen_state = match self.seen.get(&obj) {
                Some(v) => *v,
                None => Object::False,
            };
            match obj {
                Object::Pair(pair) if seen_state.is_false() => {
                    self.put_string(" ");
                    self.display_one(pair.car);
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ");
                    self.display_one(obj);
                    break;
                }
            }
        }
        self.put_string(")");
    }

    fn scan(&mut self, obj: Object) {
        let mut o = obj;
        loop {
            match o {
                Object::ByteVector(_)
                | Object::Closure(_)
                | Object::Vox(_)
                | Object::ProgramCounter(_)
                | Object::ObjectPointer(_)
                | Object::Unspecified
                | Object::True
                | Object::Procedure(_)
                | Object::Char(_)
                | Object::EqHashtable(_)
                | Object::False
                | Object::Float(_)
                | Object::StringInputPort(_)
                | Object::FileInputPort(_)
                | Object::Eof
                | Object::FileOutputPort(_)
                | Object::StringOutputPort(_)
                | Object::Instruction(_)
                | Object::Nil
                | Object::Symbol(_)
                | Object::String(_)
                | Object::Number(_) => return,
                Object::Pair(p) => {
                    let val = match self.seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        self.seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        self.seen.insert(obj, Object::False);
                    }
                    self.scan(p.car);
                    o = p.cdr;
                    continue;
                }
                Object::Vector(v) => {
                    let val = match self.seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        self.seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        self.seen.insert(obj, Object::False);
                    }
                    for i in 0..v.len() {
                        self.scan(v.data[i]);
                    }
                }
                Object::SimpleStruct(s) => {
                    let val = match self.seen.get(&o) {
                        Some(v) => *v,
                        None => Object::Unspecified,
                    };
                    if val.is_false() {
                        self.seen.insert(o, Object::True);
                        return;
                    } else if val.is_true() {
                        return;
                    } else {
                        self.seen.insert(obj, Object::False);
                    }
                    for i in 0..s.len() {
                        self.scan(s.field(i));
                    }
                }
            }
        }
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use crate::{objects::Object, vm::Vm};

    use super::Writer;

    #[test]
    fn test_number() {
        let mut w = Writer::new();
        w.display(Object::Number(123));
        assert_eq!("123", w.as_str());
    }

    #[test]
    fn test_simple_pair() {
        let mut vm = Vm::new();
        let pair = vm.gc.cons(Object::Number(123), Object::Number(456));
        let mut w = Writer::new();
        w.display(pair);
        assert_eq!("(123 . 456)", w.as_str());
    }

    #[test]
    fn test_shared_pair() {
        let mut vm = Vm::new();
        let pair = vm.gc.cons(Object::Number(123), Object::Nil);
        pair.to_pair().cdr = pair;
        let mut w = Writer::new();
        w.display(pair);
        assert_eq!("#1=(123 . #1#)", w.as_str());
    }
}
