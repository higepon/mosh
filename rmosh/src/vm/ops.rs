use crate::{
    error,
    gc::GcRef,
    numbers::{add, eqv, ge, gt, le, lt, sub, ObjectExt},
    objects::{Closure, Object, Symbol},
    op::Op,
    procs::{self}, bug,
};

use super::Vm;

impl Vm {
    #[inline(always)]
    pub(super) fn push_op(&mut self) {
        self.push(self.ac);
    }

    #[inline(always)]
    pub(super) fn constant_op(&mut self) {
        let v = self.operand();
        self.set_return_value(v);
    }

    #[inline(always)]
    pub(super) fn number_add_op(&mut self) {
        let lhs = self.pop();
        let rhs = self.ac;
        match (lhs, rhs) {
            (Object::Fixnum(a), Object::Fixnum(b)) => match a.checked_add(b) {
                Some(v) => self.set_return_value(Object::Fixnum(v)),
                None => {
                    let val = add(&mut self.gc, lhs, rhs);
                    self.set_return_value(val);
                }
            },
            (a, b) => {
                let val = add(&mut self.gc, a, b);
                self.set_return_value(val);
            }
        }
    }

    #[inline(always)]
    pub(super) fn enter_op(&mut self, n: isize) {
        self.fp = self.dec(self.sp, n);
    }

    #[inline(always)]
    pub(super) fn branch_not_null_op(&mut self, skip_offset: isize) {
        if self.ac.is_nil() {
            self.set_return_value(Object::True);
        } else {
            self.set_return_value(Object::False);
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    pub(super) fn refer_free_op(&mut self, n: usize) {
        let val = self.dc.to_closure().refer_free(n);
        self.set_return_value(val);
    }

    #[inline(always)]
    pub(super) fn car_op(&mut self) -> error::Result<Object> {
        match self.ac {
            Object::Pair(pair) => {
                self.set_return_value(pair.car);
                Ok(Object::Unspecified)
            }
            obj => self.call_assertion_violation_after("car", "pair required", &[obj]),
        }
    }

    pub(super) fn call_assertion_violation_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3("assertion-violation", who, message, irritants)
    }

    pub(super) fn call_assertion_violation_obj_after(
        &mut self,
        who: Object,
        message: Object,
        irritants: Object,
    ) -> error::Result<Object> {
        self.raise_after3("assertion-violation", who, message, irritants)
    }

    pub(super) fn call_io_decoding_error_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3("raise-i/o-decoding-error", who, message, irritants)
    }

    pub(super) fn call_io_file_not_exist_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3(
            "raise-i/o-file-does-not-exist-error",
            who,
            message,
            irritants,
        )
    }

    pub(super) fn call_io_file_already_exist_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3(
            "raise-i/o-file-already-exists-error",
            who,
            message,
            irritants,
        )
    }

    pub(super) fn call_io_encoding_error_after(
        &mut self,
        who: &str,
        message: &str,
        ch: char,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let ch = Object::Char(ch);
        let irritants = self.gc.listn(irritants);
        self.raise_after4("raise-i/o-encoding-error", who, message, ch, irritants)
    }

    pub(super) fn call_error_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3("error", who, message, irritants)
    }

    pub(super) fn call_io_invalid_position_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3("raise-i/o-invalid-position-error", who, message, irritants)
    }
    pub(super) fn implementation_restriction_violation_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3(
            "implementation-restriction-violation",
            who,
            message,
            irritants,
        )
    }

    pub(super) fn call_read_error_after(
        &mut self,
        who: &str,
        message: &str,
        irritants: &[Object],
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        let irritants = self.gc.listn(irritants);
        self.raise_after3("raise-i/o-read-error", who, message, irritants)
    }

    #[inline(always)]
    pub(super) fn refer_local_op(&mut self, n: isize) {
        let obj = self.refer_local(n);
        self.set_return_value(obj);
    }

    #[inline(always)]
    pub(super) fn refer_global_op(&mut self, symbol: GcRef<Symbol>) -> error::Result<Object> {
        match self.globals.get(&symbol) {
            Some(&value) => {
                self.set_return_value(value);
            }
            None => {
                self.call_assertion_violation_after(
                    "identifier",
                    "not found",
                    &[Object::Symbol(symbol)],
                )?;
            }
        }
        Ok(Object::Unspecified)
    }

    #[inline(always)]
    pub(super) fn cdr_op(&mut self) -> error::Result<Object> {
        match self.ac {
            Object::Pair(pair) => {
                self.set_return_value(pair.cdr);
                Ok(Object::Unspecified)
            }
            _ => {
                let irritatns = self.gc.list1(self.ac);
                self.call_assertion_violation_after("cdr", "pair required", &[irritatns])
            }
        }
    }

    #[inline(always)]
    pub(super) fn call_op(&mut self, argc: isize) -> error::Result<Object> {
        let mut argc = argc;
        'call: loop {
            match self.ac {
                Object::Closure(closure) => {
                    self.dc = self.ac;
                    self.cl = self.ac;
                    self.pc = closure.ops;
                    if closure.is_optional_arg {
                        let extra_len = argc - closure.argc;
                        if -1 == extra_len {
                            let sp = self.unshift_args(self.sp, 1);
                            self.index_set(sp, 0, Object::Nil);
                            self.sp = sp;
                            self.fp = self.dec(self.sp, closure.argc);
                        } else if extra_len >= 0 {
                            let args = self.stack_to_pair(extra_len + 1);
                            self.index_set(self.sp, extra_len, args);
                            let sp = self.dec(self.sp, extra_len);
                            self.fp = self.dec(sp, closure.argc);
                            self.sp = sp;
                        } else {
                            self.call_assertion_violation_after(
                                "call",
                                &format!(
                                    "wrong number of arguments {} required bug got {}",
                                    closure.argc, argc
                                ),
                                &[self.ac],
                            )?;
                        }
                    } else if argc == closure.argc {
                        self.fp = self.dec(self.sp, argc);
                    } else {
                        self.call_assertion_violation_after(
                            "call",
                            &format!(
                                "wrong number of arguments {} required bug got {}",
                                closure.argc, argc
                            ),
                            &[self.ac],
                        )?;
                    }
                }
                Object::Procedure(procedure) => {
                    let start = unsafe { self.sp.offset_from(self.stack.as_ptr()) } - argc;
                    let start: usize = start as usize;
                    let uargc: usize = argc as usize;
                    let args = &mut self.stack[start..start + uargc];
                    self.cl = self.ac;
                    // copying args here because we can't borrow.
                    let args = &mut args.to_owned()[..];

                    // We convert apply call to Op::Call.
                    if procedure.func as usize == procs::apply as usize {
                        if argc == 1 {
                            self.call_assertion_violation_after(
                                "apply",
                                "need two or more arguments but only 1 argument",
                                &[],
                            )?;
                            return Ok(Object::Unspecified);
                        }
                        self.sp = self.dec(self.sp, argc);
                        self.ac = args[0];
                        // (apply proc arg1 arg2 ... args-as-list)
                        // We push arguments here. The last argument is flatten list.
                        for i in 1..argc {
                            if i == argc - 1 {
                                let mut last_pair = args[i as usize];
                                if !last_pair.is_list() {
                                    self.call_assertion_violation_after(
                                        "apply",
                                        "last arguments shoulbe proper list but got",
                                        &[last_pair],
                                    )?;
                                    return Ok(Object::Unspecified);
                                }
                                let mut j: isize = 0;
                                loop {
                                    if last_pair.is_nil() {
                                        argc = argc - 2 + j;
                                        continue 'call;
                                    } else {
                                        match last_pair {
                                            Object::Pair(pair) => {
                                                self.push(pair.car);
                                                last_pair = pair.cdr;
                                            }
                                            _ => {
                                                bug!("Never reached");
                                            }
                                        }
                                    }
                                    j = j + 1;
                                }
                            } else {
                                self.push(args[i as usize]);
                            }
                        }
                    } else if procedure.func as usize == procs::eval as usize {
                        self.pc = self.allocate_return_code(argc);
                        (procedure.func)(self, args)?;
                    } else {
                        self.cl = self.ac;
                        self.pc = self.allocate_return_code(argc);
                        /*
                        // debug
                        let free_vars = default_free_vars(&mut self.gc);
                        for free_var in free_vars {
                            if free_var.to_procedure().func as usize == procedure.func as usize {
                                println!("free_var={} called", free_var.to_procedure().name)
                            }
                        }
                        */
                        self.ac = (procedure.func)(self, args)?;
                    }
                }
                Object::Continuation(c) => {
                    let mut code = vec![];
                    code.push(Object::Instruction(Op::ConstantPush));
                    code.push(c.winders);
                    code.push(Object::Instruction(Op::DynamicWinders));
                    code.push(Object::Instruction(Op::BranchNotEq));
                    code.push(Object::Fixnum(3));
                    code.push(Object::Instruction(Op::LocalJmp));
                    code.push(Object::Fixnum(8));
                    code.push(Object::Instruction(Op::Frame));
                    code.push(Object::Fixnum(6));
                    code.push(Object::Instruction(Op::ConstantPush));
                    code.push(c.winders);
                    code.push(Object::Instruction(Op::ReferGlobalCall));
                    code.push(self.gc.symbol_intern("perform-dynamic-wind"));
                    code.push(Object::Fixnum(1));
                    code.push(Object::Instruction(Op::RestoreContinuation));
                    code.push(Object::Fixnum(argc));
                    code.push(c.stack);
                    code.push(Object::Fixnum(c.shift_size));
                    self.pc = self.allocate_code(&code);
                }
                _ => {
                    self.call_assertion_violation_after(
                        "call",
                        &format!(
                            "can't call {}",
                            self.ac
                        ),
                        &[self.ac],
                    )?;                    
                }
            }
            break;
        }
        Ok(Object::Unspecified)
    }

    #[inline(always)]
    pub(super) fn define_global_op(&mut self, symbol: GcRef<Symbol>) {
        self.globals.insert(symbol, self.ac);
    }

    #[inline(always)]
    pub(super) fn number_sub_op(&mut self) {
        let lhs = self.pop();
        let rhs = self.ac;
        let result = sub(&mut self.gc, lhs, rhs);
        self.set_return_value(result);
    }

    #[inline(always)]
    pub(super) fn frame_op(&mut self) {
        // Call frame in stack.
        // ======================
        //          pc*
        // ======================
        //          dc
        // ======================
        //          cl
        // ======================
        //          fp
        // ======== sp ==========
        //
        // where pc* = pc + skip_offset -1
        let skip_offset = self.operand().to_isize();
        let next_pc = self.jump(self.pc, skip_offset - 1);
        self.make_frame(next_pc);
    }

    #[inline(always)]
    pub(super) fn closure_op(&mut self) {
        let size = self.usize_operand();
        let arg_len = self.isize_operand();
        let is_optional_arg = self.bool_operand();
        let num_free_vars = self.isize_operand();
        let _max_stack = self.operand();
        let src_info = self.operand();
        let mut free_vars = vec![];
        let start = self.dec(self.sp, 1);
        for i in 0..num_free_vars {
            let var = unsafe { *start.offset(-i) };
            free_vars.push(var);
        }
        // Don't call self.alloc here.
        // Becase it can trigger gc and free the allocated object *before* it is rooted.
        let c = self.gc.alloc(Closure::new(
            self.pc,
            size - 1,
            arg_len,
            is_optional_arg,
            free_vars,
            src_info,
        ));
        self.set_return_value(Object::Closure(c));
        self.sp = self.dec(self.sp, num_free_vars);
        self.pc = self.jump(self.pc, size as isize - 6);
    }

    #[inline(always)]
    pub(super) fn branch_not_eq_op(&mut self) {
        let skip_offset = self.isize_operand();
        let op_result = eqv(self.pop(), self.ac);
        self.set_return_value(op_result.to_obj());
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    pub(super) fn branch_not_gt_op(&mut self) {
        let skip_offset = self.isize_operand();
        let op_result = gt(self.pop(), self.ac);
        self.set_return_value(op_result.to_obj());
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    pub(super) fn branch_not_lt_op(&mut self) {
        let skip_offset = self.isize_operand();
        let op_result = lt(self.pop(), self.ac);
        self.set_return_value(op_result.to_obj());
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    pub(super) fn branch_not_ge_op(&mut self) {
        let skip_offset = self.isize_operand();
        let op_result = ge(self.pop(), self.ac);
        self.set_return_value(op_result.to_obj());
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }

    #[inline(always)]
    pub(super) fn branch_not_le_op(&mut self) {
        let skip_offset = self.isize_operand();
        let op_result = le(self.pop(), self.ac);
        self.set_return_value(op_result.to_obj());
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }
}
