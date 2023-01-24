use crate::{objects::{Object, Pair}, gc::GcRef};

pub struct Writer {
    accumulated: String,
}

impl Writer {
    pub fn new() -> Self {
        Self {
            accumulated: String::new(),
        }
    }

    pub fn display(&mut self, obj: Object) {
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
        self.display(p.car);

        let mut obj = p.cdr;
        loop {
            match obj {
                Object::Pair(pair) => {
                    self.put_string(" ");
                    self.display(pair.car);
                    obj = pair.cdr;
                }
                Object::Nil => {
                    break;
                }
                _ => {
                    self.put_string(" . ");
                    self.display(obj);
                    break;
                }
            }
        }
        self.put_string(")");
    }
}

/// Tests.
#[cfg(test)]
pub mod tests {

    use crate::objects::Object;

    use super::Writer;

    #[test]
    fn test_number() {
        let mut w = Writer::new();
        w.display(Object::Number(123));
        assert_eq!("123", w.as_str());
    }
}
