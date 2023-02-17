use crate::{
    equal::Equal,
    error,
    numbers::{div, eqv, ge, gt, le, lt, mul, SchemeError},
    objects::{Closure, Continuation, ContinuationStack, Object, Pair, Vox},
    op::Op,
    ports::TextInputPort,
};

use super::{Vm, MAX_NUM_VALUES};

// Raise an exception or exit with the exception raised in Scheme world.
// In R7RS mode we raise the exception in Scheme worled.
// Otherwise print the exception and exit.
#[macro_export]
macro_rules! raise_or_exit {
    ($self:ident, $call:expr) => {{
        match $call {
            Ok(_) => Object::Unspecified,
            Err(e) => $self.assertion_violation_err(e)?,
        };
    }};
}

impl Vm {
    pub fn run(&mut self, ops: *const Object, ops_len: usize) -> error::Result<Object> {
        if !self.is_initialized {
            self.sp = self.stack.as_mut_ptr();
            self.fp = self.sp;

            // Create display closure and make free variables accessible.
            self.initialize_free_vars(ops, ops_len);

            // Load the base library.
            self.load_compiler()?;
            self.is_initialized = true;
        }
        // Run the program.
        let ret = self.run_ops(ops);

        // Clean up so that GC can sweep them.
        self.reset_roots();
        ret
    }

    pub(super) fn run_ops(&mut self, ops: *const Object) -> error::Result<Object> {
        self.pc = ops;
        loop {
            let op: Op = unsafe { *self.pc }.to_instruction();
            self.pc = self.jump(self.pc, 1);
            match op {
                Op::CompileError => todo!(),
                Op::BranchNotLe => {
                    self.branch_not_le_op();
                }
                Op::BranchNotGe => {
                    self.branch_not_ge_op();
                }
                Op::BranchNotLt => {
                    self.branch_not_lt_op();
                }
                Op::BranchNotGt => {
                    self.branch_not_gt_op();
                }
                Op::BranchNotNull => {
                    let skip_offset = self.isize_operand();
                    let op_result = self.ac.is_nil();
                    self.set_return_value(Object::make_bool(op_result));
                    if op_result {
                        // go to then.
                    } else {
                        // Branch and jump to else.
                        self.pc = self.jump(self.pc, skip_offset - 1);
                    }
                }
                Op::BranchNotNumberEqual => {
                    self.branch_not_eq_op();
                }
                Op::BranchNotEq => {
                    let skip_offset = self.isize_operand();
                    let pred = self.pop().eq(&self.ac);
                    self.set_return_value(Object::make_bool(pred));
                    if !pred {
                        // Branch and jump to else.
                        self.pc = self.jump(self.pc, skip_offset - 1);
                    }
                }
                Op::BranchNotEqv => {
                    let skip_offset = self.isize_operand();
                    if self.pop().eqv(&self.ac) {
                        self.set_return_value(Object::True);
                    } else {
                        self.pc = self.jump(self.pc, skip_offset - 1);
                        self.set_return_value(Object::False);
                    }
                }
                Op::BranchNotEqual => {
                    let skip_offset = self.isize_operand();
                    let e = Equal::new();
                    let x = self.pop();
                    if e.is_equal(&mut self.gc, &x, &self.ac) {
                        self.set_return_value(Object::True);
                    } else {
                        self.pc = self.jump(self.pc, skip_offset - 1);
                        self.set_return_value(Object::False);
                    }
                }
                Op::Append2 => {
                    let head = self.pop();
                    if Pair::is_list(head) {
                        let p = self.gc.append2(head, self.ac);
                        self.set_return_value(p);
                    } else {
                        self.arg_err("append", "pair", head);
                    }
                }
                Op::Call => {
                    let argc = self.isize_operand();
                    raise_or_exit!(self, self.call_op(argc));
                }
                Op::Apply => todo!(),
                Op::Push => {
                    self.push_op();
                }
                Op::AssignFree => {
                    let n = self.usize_operand();
                    let closure = self.dc.to_closure();
                    match closure.refer_free(n) {
                        Object::Vox(mut vox) => {
                            vox.value = self.ac;
                        }
                        _ => {
                            panic!("assign_free: vox not found")
                        }
                    }
                }
                Op::AssignGlobal => {
                    let symbol = self.symbol_operand();
                    // Same as define global op.
                    self.define_global_op(symbol);
                }
                Op::AssignLocal => {
                    let n = self.isize_operand();
                    match self.refer_local(n) {
                        Object::Vox(mut vox) => vox.value = self.ac,
                        _ => {
                            panic!("assign_local: vox not found")
                        }
                    }
                }
                Op::Box => {
                    let n = self.isize_operand();
                    let vox = self.gc.alloc(Vox::new(self.index(self.sp, n)));
                    self.index_set(self.sp, n, Object::Vox(vox));
                }
                Op::Caar => match self.ac {
                    Object::Pair(pair) => match pair.car {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.car);
                        }
                        obj => {
                            self.arg_err("caar", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("caar", "pair", obj);
                    }
                },
                Op::Cadr => match self.ac {
                    Object::Pair(pair) => match pair.cdr {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.car);
                        }
                        obj => {
                            self.arg_err("cadr", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("cadr", "pair", obj);
                    }
                },
                Op::Car => {
                    self.car_op()?;
                }
                Op::Cdar => match self.ac {
                    Object::Pair(pair) => match pair.car {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.cdr);
                        }
                        obj => {
                            self.arg_err("cdar", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("cdar", "pair", obj);
                    }
                },
                Op::Cddr => match self.ac {
                    Object::Pair(pair) => match pair.cdr {
                        Object::Pair(pair) => {
                            self.set_return_value(pair.cdr);
                        }
                        obj => {
                            self.arg_err("cddr", "pair", obj);
                        }
                    },
                    obj => {
                        self.arg_err("cddr", "pair", obj);
                    }
                },
                Op::Cdr => {
                    self.cdr_op();
                }
                Op::Closure => {
                    self.closure_op();
                    // TODO: Tentative GC here.
                    self.mark_and_sweep();
                }
                Op::Cons => {
                    let car = self.pop();
                    let cdr = self.ac;
                    let pair = self.gc.cons(car, cdr);
                    self.set_return_value(pair);
                }
                Op::Constant => {
                    self.constant_op();
                }
                Op::DefineGlobal => {
                    let symbol = self.symbol_operand();
                    self.define_global_op(symbol)
                }
                Op::Display => {
                    let num_free_vars = self.isize_operand();
                    let mut free_vars = vec![];
                    let start = self.dec(self.sp, 1);
                    for i in 0..num_free_vars {
                        let var = unsafe { *start.offset(-i) };
                        free_vars.push(var);
                    }
                    let mut display = self.gc.alloc(Closure::new(
                        [].as_ptr(),
                        0,
                        0,
                        false,
                        free_vars,
                        Object::False,
                    ));
                    display.prev = self.dc;

                    let display = Object::Closure(display);
                    self.dc = display;
                    self.sp = self.dec(self.sp, num_free_vars);
                }
                Op::Enter => {
                    let n = self.isize_operand();
                    self.enter_op(n)
                }
                Op::Eq => {
                    let ret = self.pop().eq(&self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                Op::Eqv => {
                    let ret = self.pop().eqv(&self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                Op::Equal => {
                    let e = Equal::new();
                    let val = self.pop();
                    let ret = e.is_equal(&mut self.gc, &val, &self.ac);
                    self.set_return_value(Object::make_bool(ret));
                }
                Op::Frame => {
                    self.frame_op();
                }
                Op::Indirect => match self.ac {
                    Object::Vox(vox) => {
                        self.set_return_value(vox.value);
                    }
                    obj => {
                        self.arg_err("indirect", "vox", obj);
                    }
                },
                Op::Leave => {
                    let n = self.isize_operand();
                    let sp = self.dec(self.sp, n);

                    match self.index(sp, 0) {
                        Object::ObjectPointer(fp) => {
                            self.fp = fp;
                        }
                        obj => {
                            panic!("leave: fp expected but got {:?}", obj);
                        }
                    }
                    self.dc = self.index(sp, 1);
                    self.sp = self.dec(sp, 2);
                }
                Op::LetFrame => {
                    let _unused = self.operand();
                    // TODO: expand stack.
                    self.push(self.dc);
                    self.push(Object::ObjectPointer(self.fp));
                }
                Op::List => {
                    let n = self.isize_operand();
                    let mut list = Object::Nil;
                    for i in 0..n {
                        let obj = self.index(self.sp, i);
                        list = self.gc.cons(obj, list);
                    }
                    self.set_return_value(list);
                    self.sp = self.dec(self.sp, n);
                }
                Op::LocalJmp => {
                    let jump_offset = self.isize_operand();
                    self.pc = self.jump(self.pc, jump_offset - 1);
                }
                Op::MakeContinuation => {
                    let n = self.isize_operand();
                    let source_stack = &self.stack[0..self.stack_len()];
                    let c_stack = Object::ContinuationStack(
                        self.gc.alloc(ContinuationStack::new(source_stack)),
                    );
                    let c = Object::Continuation(self.gc.alloc(Continuation::new(
                        n,
                        c_stack,
                        self.dynamic_winders,
                    )));
                    self.set_return_value(c);
                }
                Op::MakeVector => match self.pop() {
                    Object::Fixnum(size) => {
                        let v = vec![self.ac; size as usize];
                        let v = self.gc.new_vector(&v);
                        self.set_return_value(v);
                    }
                    obj => {
                        self.arg_err("make-vector", "numbers", obj);
                    }
                },
                Op::Nop => {}
                Op::Not => {
                    self.set_return_value(Object::make_bool(self.ac.is_false()));
                }
                Op::NullP => {
                    self.set_return_value(Object::make_bool(self.ac.is_nil()));
                }
                Op::NumberAdd => {
                    self.number_add_op();
                }
                Op::NumberEqual => {
                    let op_result = eqv(self.pop(), self.ac);
                    self.set_return_value(Object::make_bool(op_result));
                }
                Op::NumberGe => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    if lhs.is_number() && rhs.is_number() {
                        let op_result = ge(lhs, rhs);
                        self.set_return_value(Object::make_bool(op_result));
                    } else {
                        panic!(">=: numbers required but got {} {}", lhs, rhs);
                    }
                }
                Op::NumberGt => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    if lhs.is_number() && rhs.is_number() {
                        let op_result = gt(lhs, rhs);
                        self.set_return_value(Object::make_bool(op_result));
                    } else {
                        panic!(">: numbers required but got {} {}", lhs, rhs);
                    }
                }
                Op::NumberLe => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    if lhs.is_number() && rhs.is_number() {
                        let op_result = le(lhs, rhs);
                        self.set_return_value(Object::make_bool(op_result));
                    } else {
                        panic!("<=: numbers required but got {} {}", lhs, rhs);
                    }
                }
                Op::NumberLt => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    if lhs.is_number() && rhs.is_number() {
                        let op_result = lt(lhs, rhs);
                        self.set_return_value(Object::make_bool(op_result));
                    } else {
                        panic!("<: numbers required but got {} {}", lhs, rhs);
                    }
                }
                Op::NumberMul => {
                    let lhs = self.pop();
                    let rhs = self.ac;
                    let op_result = mul(&mut self.gc, lhs, rhs);
                    self.set_return_value(op_result);
                }
                Op::NumberDiv => {
                    let n = self.pop();
                    let d = self.ac;
                    match div(&mut self.gc, n, d) {
                        Ok(result) => self.set_return_value(result),
                        Err(SchemeError::Div0) => {
                            panic!("/: division by zero {} {}", n, d)
                        }
                        _ => panic!(),
                    }
                }
                Op::NumberSub => {
                    self.number_sub_op();
                }
                Op::PairP => self.set_return_value(Object::make_bool(self.ac.is_pair())),
                Op::Read => {
                    let port;
                    if self.ac.is_nil() {
                        port = self.current_input_port;
                    } else {
                        port = self.ac;
                    }
                    match port {
                        Object::FileInputPort(mut p) => match p.read(&mut self.gc) {
                            Ok(obj) => {
                                self.set_return_value(obj);
                            }
                            Err(err) => {
                                panic!("read: error {:?}", err)
                            }
                        },
                        Object::StringInputPort(mut p) => match p.read(&mut self.gc) {
                            Ok(obj) => {
                                self.set_return_value(obj);
                            }
                            Err(err) => {
                                self.assertion_violation("read", &format!("{:?}", err), port)?;
                            }
                        },
                        _ => {
                            self.assertion_violation("read", "input port required", port)?;
                        }
                    }
                }
                Op::ReadChar => match self.ac {
                    Object::StringInputPort(mut port) => match port.read_char() {
                        Some(c) => {
                            self.set_return_value(Object::Char(c));
                        }
                        None => {
                            self.set_return_value(Object::Eof);
                        }
                    },
                    obj => {
                        self.arg_err("read-char", "text-input-port", obj);
                    }
                },
                Op::Reduce => todo!(),
                Op::ReferFree => {
                    let n = self.usize_operand();
                    self.refer_free_op(n);
                }
                Op::ReferGlobal => {
                    let symbol = self.symbol_operand();
                    self.refer_global_op(symbol);
                    //println!("symbol={}", Object::Symbol(symbol));
                }
                Op::ReferLocal => {
                    let n = self.isize_operand();
                    self.refer_local_op(n)
                }
                Op::RestoreContinuation => {
                    self.num_values = self.usize_operand();
                    if self.num_values != 0 {
                        for i in 0..self.num_values - 1 {
                            self.values[i] =
                                self.index(self.sp, (self.num_values - i - 2) as isize);
                        }
                        self.ac = self.index(self.sp, self.num_values as isize - 1);
                    }

                    let c_stack = self.operand().to_continuation_stack();
                    let restored_size = c_stack.restore(&mut self.stack);
                    self.sp = self.stack.as_mut_ptr();
                    self.sp = self.inc(self.sp, restored_size as isize);

                    let depth = 0;
                    let diff = self.isize_operand();
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    self.return_n(0);
                }
                Op::Return => {
                    let n = self.isize_operand();
                    self.return_n(n);
                }
                Op::SetCar => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.car = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-car!", "pair", obj);
                    }
                },
                Op::SetCdr => match self.pop() {
                    Object::Pair(mut pair) => {
                        pair.cdr = self.ac;
                        self.set_return_value(Object::Unspecified);
                    }
                    obj => {
                        self.arg_err("set-cdr!", "pair", obj);
                    }
                },
                Op::Shift => todo!(),
                Op::SymbolP => {
                    self.set_return_value(Object::make_bool(self.ac.is_symbol()));
                }
                Op::Test => {
                    let jump_offset = self.isize_operand();
                    if self.ac.is_false() {
                        self.pc = self.jump(self.pc, jump_offset - 1);
                    }
                }
                Op::Values => {
                    //  values stack layout
                    //    (value 'a 'b 'c 'd)
                    //    ==>
                    //    =====
                    //      a
                    //    =====
                    //      b
                    //    =====
                    //      c    [ac_] = d
                    //    =====
                    //  values are stored in [valuez vector] and [a-reg] like following.
                    //  #(b c d)
                    //  [ac_] = a
                    let n = self.usize_operand();

                    if n > MAX_NUM_VALUES + 1 {
                        panic!("values too many values {}", n);
                    } else {
                        self.num_values = n;

                        for i in (1..n).rev() {
                            self.values[i - 1] = self.ac;
                            self.ac = self.index(self.sp, (n - i - 1) as isize);
                        }

                        if n > 1 {
                            self.sp = self.dec(self.sp, self.num_values as isize - 1);
                        } else {
                            // there's no need to push
                        }
                    }
                    if n == 0 {
                        self.ac = Object::Unspecified;
                    }
                }
                Op::Receive => {
                    let num_req_args = self.usize_operand();
                    let num_opt_args = self.usize_operand();
                    if self.num_values < num_req_args {
                        panic!(
                            "receive: received fewer valeus than expected {} {}",
                            num_req_args, self.num_values
                        );
                    } else if num_opt_args == 0 && self.num_values > num_req_args {
                        panic!(
                            "receive: received more values than expected {} {}",
                            num_req_args, self.num_values
                        );
                    }
                    // (receive (a b c) ...)
                    if num_opt_args == 0 {
                        if num_req_args > 0 {
                            self.push(self.ac);
                        }
                        for i in 0..num_req_args - 1 {
                            self.push(self.values[i]);
                        }
                    // (receive a ...)
                    } else if num_req_args == 0 {
                        let mut ret = if self.num_values == 0 {
                            Object::Nil
                        } else {
                            self.gc.list1(self.ac)
                        };
                        if self.num_values >= 1 {
                            for i in 0..self.num_values - 1 {
                                ret = Pair::append_destructive(ret, self.gc.list1(self.values[i]));
                            }
                        }
                        self.push(ret);
                    // (receive (a b . c) ...)
                    } else {
                        let mut ret = Object::Nil;
                        self.push(self.ac);
                        for i in 0..self.num_values - 1 {
                            if i < num_req_args - 1 {
                                self.push(self.values[i]);
                            } else {
                                ret = Pair::append_destructive(ret, self.gc.list1(self.values[i]));
                            }
                        }
                        self.push(ret);
                    }
                }
                Op::UnfixedJump => todo!(),
                Op::Stop => todo!(),
                Op::Shiftj => {
                    let depth = self.isize_operand();
                    let diff = self.isize_operand();
                    let display_count = self.isize_operand();

                    // SHIFT for embedded jump which appears in named let optimization.
                    //   Two things happens.
                    //   1. SHIFT the stack (same as SHIFT operation)
                    //   2. Restore fp and c registers.
                    //      This is necessary for jump which is across let or closure boundary.
                    //      new-fp => new-sp - arg-length
                    //
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    self.fp = self.dec(self.sp, depth);
                    let mut i = display_count;
                    loop {
                        if i <= 0 {
                            break;
                        }
                        self.dc = self.dc.to_closure().prev;
                        i -= 1;
                    }
                }
                Op::Undef => {
                    self.set_return_value(Object::Unspecified);
                }
                Op::VectorLength => match self.ac {
                    Object::Vector(v) => {
                        self.set_return_value(Object::Fixnum(v.len() as isize));
                    }
                    obj => {
                        self.arg_err("vector-length", "vector", obj);
                    }
                },
                Op::VectorP => match self.ac {
                    Object::Vector(_) => {
                        self.set_return_value(Object::True);
                    }
                    _ => {
                        self.set_return_value(Object::False);
                    }
                },
                Op::VectorRef => match (self.pop(), self.ac) {
                    (Object::Vector(v), Object::Fixnum(idx)) => {
                        let idx = idx as usize;
                        if idx < v.data.len() {
                            self.set_return_value(v.data[idx]);
                        } else {
                            self.arg_err("vector-ref", "valid idx to vector", self.ac);
                        }
                    }
                    (a, b) => {
                        panic!(
                            "vecto-ref: vector and number required but got {:?} {:?}",
                            a, b
                        );
                    }
                },
                Op::VectorSet => {
                    let n = self.pop();
                    let obj = self.pop();
                    match (obj, n) {
                        (Object::Vector(mut v), Object::Fixnum(idx)) => {
                            let idx = idx as usize;
                            if idx < v.data.len() {
                                v.data[idx] = self.ac;
                            } else {
                                self.arg_err("vector-set", "valid idx to vector", obj);
                            }
                        }
                        (a, b) => {
                            panic!(
                                "vecto-set: vector and number required but got {:?} {:?}",
                                a, b
                            );
                        }
                    }
                }
                Op::PushEnter => {
                    self.push_op();
                    let n = self.isize_operand();
                    self.enter_op(n);
                }
                Op::Halt => {
                    break;
                }
                Op::ConstantPush => {
                    self.constant_op();
                    self.push_op();
                }
                Op::NumberSubPush => {
                    self.number_sub_op();
                    self.push_op();
                }
                Op::NumberAddPush => {
                    self.number_add_op();
                    self.push_op();
                }
                Op::PushConstant => {
                    self.push_op();
                    self.constant_op();
                }
                Op::PushFrame => {
                    self.push_op();
                    self.frame_op();
                }
                Op::CarPush => {
                    self.car_op()?;
                    self.push_op();
                }
                Op::CdrPush => {
                    self.cdr_op();
                    self.push_op();
                }
                Op::ShiftCall => todo!(),
                Op::NotTest => {
                    let jump_offset = self.isize_operand();
                    self.ac = if self.ac.is_false() {
                        Object::True
                    } else {
                        Object::False
                    };
                    if self.ac.is_false() {
                        self.pc = self.jump(self.pc, jump_offset - 1);
                    }
                }
                Op::ReferGlobalCall => {
                    let symbol = self.symbol_operand();
                    let argc = self.isize_operand();
                    self.refer_global_op(symbol);
                    raise_or_exit!(self, self.call_op(argc));
                }
                Op::ReferFreePush => {
                    let n = self.usize_operand();
                    self.refer_free_op(n);
                    self.push_op();
                }
                Op::ReferLocalPush => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    self.push_op();
                }
                Op::ReferLocalPushConstant => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    self.push_op();
                    self.constant_op();
                }
                Op::ReferLocalPushConstantBranchNotLe => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    self.push_op();
                    self.constant_op();
                    self.branch_not_le_op();
                }
                Op::ReferLocalPushConstantBranchNotGe => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    self.push_op();
                    self.constant_op();
                    self.branch_not_ge_op();
                }
                Op::ReferLocalPushConstantBranchNotNumberEqual => todo!(),
                Op::ReferLocalBranchNotNull => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    let skip_offset = self.isize_operand();
                    self.branch_not_null_op(skip_offset);
                }
                Op::ReferLocalBranchNotLt => {
                    let n = self.isize_operand();
                    self.refer_local_op(n);
                    self.branch_not_lt_op();
                }
                Op::ReferFreeCall => {
                    let n = self.usize_operand();
                    let argc = self.isize_operand();
                    self.refer_free_op(n);
                    raise_or_exit!(self, self.call_op(argc));
                }
                Op::ReferGlobalPush => {
                    let symbol = self.symbol_operand();
                    self.refer_global_op(symbol);
                    self.push_op();
                    // println!("symbol={}", Object::Symbol(symbol));
                }
                Op::ReferLocalCall => {
                    let n = self.isize_operand();
                    let argc = self.isize_operand();
                    self.refer_local_op(n);
                    raise_or_exit!(self, self.call_op(argc));
                }
                Op::LocalCall => {
                    let argc = self.isize_operand();
                    // Locall is lighter than Call
                    // We can omit checking closure type and arguments length.
                    match self.ac {
                        Object::Closure(c) => {
                            self.dc = self.ac;
                            // todo
                            //self.cl = self.ac;
                            self.pc = c.ops;
                            self.fp = self.dec(self.sp, argc);
                        }
                        obj => {
                            panic!("LocalCall: Bug {}", obj)
                        }
                    }
                }
                Op::Vector => {
                    let n = self.usize_operand();
                    let mut v = vec![Object::Unspecified; n];
                    let mut arg = self.ac;
                    if n > 0 {
                        let mut i = n - 1;
                        loop {
                            if i == 0 {
                                break;
                            }
                            v[i] = arg;
                            arg = self.pop();
                            i -= 1;
                        }
                        v[0] = arg;
                    }
                    let vec = self.gc.new_vector(&v);
                    self.set_return_value(vec);
                }
                Op::SimpleStructRef => match (self.pop(), self.ac) {
                    (Object::SimpleStruct(s), Object::Fixnum(idx)) => {
                        self.set_return_value(s.data[idx as usize]);
                    }
                    (obj1, obj2) => {
                        panic!(
                            "simple-struct-ref: simple-struct and idx required but got {} {}",
                            obj1, obj2
                        );
                    }
                },
                Op::DynamicWinders => {
                    self.set_return_value(self.dynamic_winders);
                }
                Op::TailCall => {
                    let depth = self.isize_operand();
                    let diff = self.isize_operand();
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    let argc = depth;
                    raise_or_exit!(self, self.call_op(argc));
                }
                Op::LocalTailCall => {
                    let depth = self.isize_operand();
                    let diff = self.isize_operand();
                    self.sp = self.shift_args_to_bottom(self.sp, depth, diff);
                    let closure = self.ac.to_closure();
                    let argc = depth;
                    self.dc = self.ac;
                    self.pc = closure.ops;
                    self.fp = self.dec(self.sp, argc);
                }
            }
            self.print_vm(op);
            //self.pc = self.jump(self.pc, 1);
        }
        Ok(self.ac)
    }

    pub fn print_stack(&self) {
        for i in 0..self.stack_len() {
            println!("{}", self.stack[i].to_short_string());
        }
    }

    #[cfg(feature = "debug_log_vm")]
    fn print_vm(&mut self, op: Op) {
        println!("-----------------------------------------");
        println!("{} executed", op);
        println!("  ac={}", self.ac.to_short_string());
        println!("  dc={}", self.dc.to_short_string());
        println!("-----------------------------------------");
        let fp_idx = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
        for i in 0..self.stack_len() {
            println!(
                "  {}{}",
                self.stack[i].to_short_string(),
                if fp_idx == i.try_into().unwrap() {
                    "  <== fp"
                } else {
                    ""
                }
            );
        }
        println!("-----------------------------------------<== sp")
    }
    #[cfg(not(feature = "debug_log_vm"))]
    fn print_vm(&mut self, _: Op) {}

    pub(super) fn arg_err(&self, who: &str, expected: &str, actual: Object) {
        panic!("{}: requires {} but got {}", who, expected, actual);
    }
}
