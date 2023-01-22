use std::ptr::null_mut;

use crate::{
    objects::{Closure, Object},
    op::Op,
    procs::default_free_vars,
};

use super::Vm;

impl Vm {
    // Main entry point for eval.
    pub fn eval_after(&mut self, sexp: Object) -> Object {
        let name = self.gc.symbol_intern("compile");
        let v = self.call_by_name(name, sexp).to_vector();
        let code_size = v.len();
        let body_size = code_size + 2;

        self.eval_code = vec![];
        for i in 0..code_size {
            self.eval_code.push(v.data[i]);
        }
        self.eval_code.push(Object::Instruction(Op::Return));
        self.eval_code.push(Object::Number(0));
        // todo: Should share this!
        let free_vars = default_free_vars(&mut self.gc);
        let c = self.gc.alloc(Closure::new(
            self.eval_code.as_ptr(),
            body_size,
            0,
            false,
            free_vars,
            Object::False,
        ));

        return self.set_after_trigger0(Object::Closure(c));
    }

    fn set_after_trigger0(&mut self, closure: Object) -> Object {
        self.make_frame(self.pc);
        self.trigger0_code[1] = closure;
        self.pc = self.trigger0_code.as_ptr();
        return self.ac;
    }

    fn call_by_name(&mut self, name: Object, arg: Object) -> Object {
        self.call_by_name_code[3] = arg;
        self.call_by_name_code[6] = name;
        self.evaluate_safe(self.call_by_name_code.as_ptr())
    }

    fn evaluate_safe(&mut self, ops: *const Object) -> Object {
        self.save_registers();
        let ret = self.evaluate_unsafe(ops);
        self.restore_registers();
        ret
    }

    fn evaluate_unsafe(&mut self, ops: *const Object) -> Object {
        self.closure_for_evaluate.to_closure().ops = ops;
        self.ac = self.closure_for_evaluate;
        self.dc = self.closure_for_evaluate;
        self.fp = null_mut();
        self.run_ops(ops)
    }
}
