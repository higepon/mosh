use std::ptr::null_mut;

use crate::{
    error,
    objects::{Closure, Object},
    op::Op,
    procs::default_free_vars,
};

use super::Vm;

impl Vm {
    // Main entry point for eval.
    pub fn eval_after(&mut self, sexp: Object) -> error::Result<Object> {
        //println!("eval_after={}", sexp);
        let name = self.gc.symbol_intern("compile-w/o-halt");
        let v = self.call_by_name(name, sexp)?;
        let v = v.to_vector();
        let code_size = v.len();
        let body_size = code_size + 2;

        // We allocate new code array for every eval calls.
        // We can't share them between multiple eval calls.
        self.eval_code_array.push(vec![]);
        let eval_code = self.eval_code_array.last_mut().unwrap();
        for i in 0..code_size {
            eval_code.push(v.data[i]);
        }
        eval_code.push(Object::Instruction(Op::Return));
        eval_code.push(Object::Fixnum(0));
        // todo: Should share this!
        let free_vars = default_free_vars(&mut self.gc);
        let c = self.gc.alloc(Closure::new(
            eval_code.as_ptr(),
            body_size,
            0,
            false,
            free_vars,
            Object::False,
        ));

        return self.set_after_trigger0(Object::Closure(c));
    }

    pub fn eval_compiled(&mut self, sexp: Object) -> error::Result<Object> {
        //println!("eval={}", sexp);
        let v = sexp.to_vector();
        let code_size = v.len();
        let body_size = code_size + 2;

        // We allocate new code array for every eval calls.
        // We can't share them between multiple eval calls.
        self.eval_code_array.push(vec![]);
        let eval_code = self.eval_code_array.last_mut().unwrap();
        for i in 0..code_size {
            eval_code.push(v.data[i]);
        }
        eval_code.push(Object::Instruction(Op::Return));
        eval_code.push(Object::Fixnum(0));
        // todo: Should share this!
        let free_vars = default_free_vars(&mut self.gc);
        let c = self.gc.alloc(Closure::new(
            eval_code.as_ptr(),
            body_size,
            0,
            false,
            free_vars,
            Object::False,
        ));

        return self.set_after_trigger0(Object::Closure(c));
    }

    fn set_after_trigger0(&mut self, closure: Object) -> error::Result<Object> {
        self.make_frame(self.pc);
        self.trigger0_code[1] = closure;
        self.pc = self.trigger0_code.as_ptr();
        return Ok(self.ac);
    }

    fn set_after_trigger3(
        &mut self,
        closure: Object,
        arg1: Object,
        arg2: Object,
        arg3: Object,
    ) -> error::Result<Object> {
        self.make_frame(self.pc);
        self.trigger3_code[10] = closure;
        self.trigger3_code[7] = arg3;
        self.trigger3_code[4] = arg2;
        self.trigger3_code[1] = arg1;
        self.pc = self.trigger3_code.as_ptr();
        return Ok(self.ac);
    }

    pub(super) fn raise_after3(
        &mut self,
        closure_name: &str,
        who: Object,
        message: Object,
        irritants: Object,
    ) -> error::Result<Object> {
        let symbol = self.gc.symbol_intern(closure_name).to_symbol();
        match self.globals.get(&symbol) {
            // The exception system is ready to use.
            Some(closure) => self.set_after_trigger3(*closure, who, message, irritants),
            None => {
                println!("EEEEEEEE");
                Err(error::Error::new(who, message, irritants))
            }
        }
    }

    fn call_by_name(&mut self, name: Object, arg: Object) -> error::Result<Object> {
        self.call_by_name_code[3] = arg;
        self.call_by_name_code[6] = name;
        self.evaluate_safe(self.call_by_name_code.as_ptr())
    }

    fn evaluate_safe(&mut self, ops: *const Object) -> error::Result<Object> {
        self.save_registers();
        let ret = self.evaluate_unsafe(ops);
        self.restore_registers();
        ret
    }

    fn evaluate_unsafe(&mut self, ops: *const Object) -> error::Result<Object> {
        self.closure_for_evaluate.to_closure().ops = ops;
        self.ac = self.closure_for_evaluate;
        self.dc = self.closure_for_evaluate;
        self.fp = null_mut();
        self.run_ops(ops)
    }
}
