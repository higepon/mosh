use crate::{
    error,
    gc::GcRef,
    numbers::{add, eqv, ge, gt, le, lt, sub},
    objects::{Closure, Object, Symbol},
    op::Op,
    procs::{self},
};

use super::Vm;

#[macro_export]
macro_rules! number_cmp_op {
    ($op:tt, $self:ident) => {
        {
            match ($self.pop(), $self.ac) {
                (Object::Fixnum(l), Object::Fixnum(r)) => {
                    $self.set_return_value(Object::make_bool(l $op r))
                }
                obj => {
                    panic!("{}: numbers required but got {:?}",  stringify!($op), obj);
                }
            }
        }
    };
}

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
        match (self.pop(), self.ac) {
            (Object::Fixnum(a), Object::Fixnum(b)) => {
                self.set_return_value(Object::Fixnum(a + b));
            }
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
            obj => self.assertion_violation("car", "pair required", obj),
        }
    }

    pub(super) fn assertion_violation(
        &mut self,
        who: &str,
        message: &str,
        irritants: Object,
    ) -> error::Result<Object> {
        let who = self.gc.new_string(who);
        let message = self.gc.new_string(message);
        self.raise_after3("assertion-violation", who, message, irritants)
    }

    #[inline(always)]
    pub(super) fn refer_local_op(&mut self, n: isize) {
        let obj = self.refer_local(n);
        self.set_return_value(obj);
    }

    #[inline(always)]
    pub(super) fn refer_global_op(&mut self, symbol: GcRef<Symbol>) {
        match self.globals.get(&symbol) {
            Some(&value) => {
                self.set_return_value(value);
            }
            None => {
                panic!("identifier {} not found", symbol.string);
            }
        }
    }

    #[inline(always)]
    pub(super) fn cdr_op(&mut self) {
        match self.ac {
            Object::Pair(pair) => {
                self.set_return_value(pair.cdr);
            }
            obj => {
                self.arg_err("cdr", "pair", obj);
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
                    // TODO:
                    // self.cl = self.ac;
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
                            panic!(
                                "call: wrong number of arguments {} required bug got {}",
                                closure.argc, argc
                            );
                        }
                    } else if argc == closure.argc {
                        self.fp = self.dec(self.sp, argc);
                    } else {
                        panic!(
                            "call: wrong number of arguments {} required bug got {}",
                            closure.argc, argc
                        );
                    }
                }
                Object::Procedure(procedure) => {
                    let start = unsafe { self.sp.offset_from(self.stack.as_ptr()) } - argc;
                    let start: usize = start as usize;
                    let uargc: usize = argc as usize;
                    let args = &mut self.stack[start..start + uargc];

                    // copying args here because we can't borrow.
                    let args = &mut args.to_owned()[..];

                    // We convert apply call to Op::Call.
                    if procedure.func as usize == procs::apply as usize {
                        if argc == 1 {
                            self.assertion_violation(
                                "apply",
                                "need two or more arguments but only 1 argument",
                                Object::Nil,
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
                                    self.assertion_violation(
                                        "apply",
                                        "last arguments shoulbe proper list but got",
                                        last_pair,
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
                                                panic!("never reached");
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
                        self.eval_ret_code = vec![];
                        self.eval_ret_code.push(Object::Instruction(Op::Return));
                        self.eval_ret_code.push(Object::Fixnum(argc));

                        self.pc = self.eval_ret_code.as_ptr();
                        (procedure.func)(self, args)?;
                    } else {
                        // TODO: Take care of cl.
                        // self.cl = self.ac
                        self.ret_code = vec![];
                        self.ret_code.push(Object::Instruction(Op::Return));
                        self.ret_code.push(Object::Fixnum(argc));

                        self.pc = self.ret_code.as_ptr();
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
                    self.eval_code_array.push(vec![]);
                    let code = self.eval_code_array.last_mut().unwrap();

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
                    self.pc = code.as_ptr();
                }
                _ => {
                    panic!("can't call {:?}", self.ac);
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
        self.set_return_value(Object::make_bool(op_result));
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
        self.set_return_value(Object::make_bool(op_result));
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
        self.set_return_value(Object::make_bool(op_result));
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
        self.set_return_value(Object::make_bool(op_result));
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
        self.set_return_value(Object::make_bool(op_result));
        if op_result {
            // go to then.
        } else {
            // Branch and jump to else.
            self.pc = self.jump(self.pc, skip_offset - 1);
        }
    }
}
