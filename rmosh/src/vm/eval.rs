use std::ptr::null_mut;

use crate::{
    bug,
    error::SchemeError,
    objects::{Closure, Object},
    op::Op,
};

use super::Vm;

impl Vm {
    // Main entry point for eval.
    pub fn eval_after(&mut self, sexp: Object) -> Result<Object, SchemeError> {
        //println!("eval_after={}", sexp);
        let name = self.gc.symbol_intern("compile-w/o-halt");
        let v = self.call_by_name(name, sexp)?;
        let v = v.to_vector();
        let code_size = v.len();
        let body_size = code_size + 2;

        let mut code = v.data.clone();
        code.push(Object::Instruction(Op::Return));
        code.push(Object::Fixnum(0));

        // We allocate new code array for every eval calls.
        // We can't share them between multiple eval calls.
        let code_ptr = self.allocate_code(&code);

        let c = self.gc.alloc(Closure::new(
            code_ptr,
            body_size,
            0,
            false,
            self.default_free_vars.to_vec(),
            Object::False,
        ));

        self.set_after_trigger0(Object::Closure(c))
    }

    pub fn eval_compiled(&mut self, sexp: Object) -> Result<Object, SchemeError> {
        //println!("eval={}", sexp);
        let v = sexp.to_vector();
        let code_size = v.len();
        let body_size = code_size + 2;

        let mut code = v.data.clone();
        code.push(Object::Instruction(Op::Return));
        code.push(Object::Fixnum(0));

        // We allocate new code array for every eval calls.
        // We can't share them between multiple eval calls.
        let code_ptr = self.allocate_code(&code);
        let c = self.gc.alloc(Closure::new(
            code_ptr,
            body_size,
            0,
            false,
            self.default_free_vars.to_vec(),
            Object::False,
        ));

        self.set_after_trigger0(Object::Closure(c))
    }

    fn set_after_trigger0(&mut self, closure: Object) -> Result<Object, SchemeError> {
        self.make_frame(self.pc);

        let code = vec![
            Object::Instruction(Op::Constant),
            closure,
            Object::Instruction(Op::Call),
            Object::Fixnum(0),
            Object::Instruction(Op::Return),
            Object::Fixnum(0),
            Object::Instruction(Op::Halt),
        ];
        self.pc = self.allocate_code(&code);
        Ok(self.ac)
    }

    fn set_after_trigger2(
        &mut self,
        closure: Object,
        arg1: Object,
        arg2: Object,
    ) -> Result<Object, SchemeError> {
        self.make_frame(self.pc);

        let code = vec![
            Object::Instruction(Op::Constant),
            arg1,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg2,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            closure,
            Object::Instruction(Op::Call),
            Object::Fixnum(2),
            Object::Instruction(Op::Return),
            Object::Fixnum(0),
            Object::Instruction(Op::Halt),
        ];

        self.pc = self.allocate_code(&code);
        Ok(self.ac)
    }

    fn set_after_trigger3(
        &mut self,
        closure: Object,
        arg1: Object,
        arg2: Object,
        arg3: Object,
    ) -> Result<Object, SchemeError> {
        self.make_frame(self.pc);

        let code = vec![
            Object::Instruction(Op::Constant),
            arg1,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg2,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg3,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            closure,
            Object::Instruction(Op::Call),
            Object::Fixnum(3),
            Object::Instruction(Op::Return),
            Object::Fixnum(0),
            Object::Instruction(Op::Halt),
        ];

        self.pc = self.allocate_code(&code);
        Ok(self.ac)
    }

    fn set_after_trigger4(
        &mut self,
        closure: Object,
        arg1: Object,
        arg2: Object,
        arg3: Object,
        arg4: Object,
    ) -> Result<Object, SchemeError> {
        self.make_frame(self.pc);

        let code = vec![
            Object::Instruction(Op::Constant),
            arg1,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg2,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg3,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            arg4,
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            closure,
            Object::Instruction(Op::Call),
            Object::Fixnum(4),
            Object::Instruction(Op::Return),
            Object::Fixnum(0),
            Object::Instruction(Op::Halt),
        ];

        self.pc = self.allocate_code(&code);
        Ok(self.ac)
    }

    pub(super) fn raise_after2(
        &mut self,
        closure_name: &str,
        who: Object,
        message: Object,
    ) -> Result<Object, SchemeError> {
        let symbol = self.gc.symbol_intern(closure_name).to_symbol();
        match self.globals.get(&symbol) {
            // The exception system is ready to use.
            Some(closure) => self.set_after_trigger2(*closure, who, message),
            None => {
                eprintln!("Warning:The underlying exception is not ready to use");
                bug!()
            }
        }
    }

    pub(super) fn raise_after3(
        &mut self,
        closure_name: &str,
        who: Object,
        message: Object,
        irritants: Object,
    ) -> Result<Object, SchemeError> {
        let symbol = self.gc.symbol_intern(closure_name).to_symbol();
        match self.globals.get(&symbol) {
            // The exception system is ready to use.
            Some(closure) => self.set_after_trigger3(*closure, who, message, irritants),
            None => {
                eprintln!("Warning:The underlying exception is not ready to use");
                bug!()
            }
        }
    }

    pub(super) fn raise_after4(
        &mut self,
        closure_name: &str,
        who: Object,
        message: Object,
        arg1: Object,
        irritants: Object,
    ) -> Result<Object, SchemeError> {
        let symbol = self.gc.symbol_intern(closure_name).to_symbol();
        match self.globals.get(&symbol) {
            // The exception system is ready to use.
            Some(closure) => self.set_after_trigger4(*closure, who, message, arg1, irritants),
            None => {
                eprintln!("Warning:The underlying exception is not ready to use");
                bug!()
            }
        }
    }

    fn call_by_name(&mut self, name: Object, arg: Object) -> Result<Object, SchemeError> {
        self.call_by_name_code[3] = arg;
        self.call_by_name_code[6] = name;
        self.evaluate_safe(self.call_by_name_code.as_ptr())
    }

    pub fn call_closure0(&mut self, func: Object) -> Result<Object, SchemeError> {
        self.call_closure0_code[3] = func;
        self.evaluate_safe(self.call_closure0_code.as_ptr())
    }

    pub fn call_closure1(&mut self, func: Object, arg1: Object) -> Result<Object, SchemeError> {
        self.call_closure1_code[3] = arg1;
        self.call_closure1_code[6] = func;
        self.evaluate_safe(self.call_closure1_code.as_ptr())
    }

    pub fn call_closure3(
        &mut self,
        func: Object,
        arg1: Object,
        arg2: Object,
        arg3: Object,
    ) -> Result<Object, SchemeError> {
        self.call_closure3_code[3] = arg1;
        self.call_closure3_code[6] = arg2;
        self.call_closure3_code[9] = arg3;
        self.call_closure3_code[12] = func;
        self.evaluate_safe(self.call_closure3_code.as_ptr())
    }

    fn evaluate_safe(&mut self, ops: *const Object) -> Result<Object, SchemeError> {
        self.save_registers();
        let ret = self.evaluate_unsafe(ops);
        self.restore_registers();
        ret
    }

    fn evaluate_unsafe(&mut self, ops: *const Object) -> Result<Object, SchemeError> {
        self.closure_for_evaluate.to_closure().ops = ops;
        self.ac = self.closure_for_evaluate;
        self.dc = self.closure_for_evaluate;
        self.cl = self.closure_for_evaluate;
        self.fp = null_mut();
        self.run_ops(ops)
    }
}
