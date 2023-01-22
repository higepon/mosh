use std::{
    collections::HashMap,
    ptr::{null, null_mut},
};

// Sub module definitions.
mod mark;
mod ops;
mod run_loop;
mod vm_helpers;

use crate::{
    compiler,
    fasl::Fasl,
    gc::{Gc, GcRef},
    objects::{Closure, Object, Pair, Symbol, Vox},
    op::Op,
    procs::{self, default_free_vars},
    psyntax,
    read::ReadError,
};

const STACK_SIZE: usize = 1024;
const MAX_NUM_VALUES: usize = 256;

#[macro_export]
macro_rules! branch_number_cmp_op {
    ($op:tt, $self:ident) => {
        {
            let skip_offset = $self.isize_operand();
            match ($self.pop(), $self.ac) {
                (Object::Number(lhs), Object::Number(rhs)) => {
                    let op_result = lhs $op rhs;
                    $self.set_return_value(Object::make_bool(op_result));
                    if op_result {
                        // go to then.
                    } else {
                        // Branch and jump to else.
                        $self.pc = $self.jump($self.pc, skip_offset - 1);
                    }
                }
                obj => {
                    panic!("{}: numbers requierd but got {:?}",  stringify!($op), obj);
                }
            }
        }
    };
}

#[macro_export]
macro_rules! number_cmp_op {
    ($op:tt, $self:ident) => {
        {
            match ($self.pop(), $self.ac) {
                (Object::Number(l), Object::Number(r)) => {
                    $self.set_return_value(Object::make_bool(l $op r))
                }
                obj => {
                    panic!("{}: numbers required but got {:?}",  stringify!($op), obj);
                }
            }
        }
    };
}

struct Registers {
    pub ac: Object,
    pub dc: Object,
    pub pc: *const Object,
    pub sp_offset: isize,
    pub fp_offset: isize,
}

impl Registers {
    fn new() -> Self {
        Self {
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            pc: null(),
            sp_offset: 0,
            fp_offset: 0,
        }
    }
}

pub struct Vm {
    pub gc: Box<Gc>,
    // Program counter
    pc: *const Object,
    // The stack.
    stack: [Object; STACK_SIZE],
    // accumulator register.
    pub ac: Object,
    // display closure register.
    dc: Object,
    // expected register to retain expected value for tests.
    pub expected: Object,
    // stack pointer.
    sp: *mut Object,
    // frame pointer.
    fp: *mut Object,
    // global variables.
    globals: HashMap<GcRef<Symbol>, Object>,
    // We keep references to operators in base libraries so that the lib_ops live longer than every call of run.
    // If we kept operators as local variable, it can/will be immediately freed after run(lib_ops).
    lib_compiler: Vec<Object>,
    lib_psyntax: Vec<Object>,
    pub dynamic_winders: Object,
    // Return values.
    values: [Object; MAX_NUM_VALUES],
    num_values: usize,
    is_initialized: bool,
    pub rtds: HashMap<Object, Object>,
    pub should_load_compiler: bool,
    pub compiled_programs: Vec<Object>,
    pub trigger0_code: Vec<Object>,
    pub eval_code: Vec<Object>,
    pub ret_code: Vec<Object>,
    pub call_by_name_code: Vec<Object>,
    pub closure_for_evaluate: Object,
    current_input_port: Object,
    saved_registers: Registers,
    // Note when we add new vars here, please make sure we take care of them in mark_roots.
    // Otherwise they can cause memory leak or double free.
}

impl Vm {
    pub fn new() -> Self {
        let mut ret = Self {
            gc: Box::new(Gc::new()),
            stack: [Object::Unspecified; STACK_SIZE],
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            expected: Object::Unspecified,
            pc: null_mut(),
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            lib_compiler: vec![],
            lib_psyntax: vec![],
            dynamic_winders: Object::Unspecified,
            num_values: 0,
            values: [Object::Unspecified; MAX_NUM_VALUES],
            rtds: HashMap::new(),
            should_load_compiler: false,
            is_initialized: false,
            compiled_programs: vec![],
            trigger0_code: vec![],
            eval_code: vec![],
            ret_code: vec![],
            call_by_name_code: vec![],
            closure_for_evaluate: Object::Unspecified,
            current_input_port: Object::Unspecified,
            saved_registers: Registers::new(),
        };
        ret.trigger0_code.push(Object::Instruction(Op::Constant));
        ret.trigger0_code.push(Object::Unspecified);
        ret.trigger0_code.push(Object::Instruction(Op::Call));
        ret.trigger0_code.push(Object::Number(0));
        ret.trigger0_code.push(Object::Instruction(Op::Return));
        ret.trigger0_code.push(Object::Number(0));
        ret.trigger0_code.push(Object::Instruction(Op::Halt));

        ret.call_by_name_code.push(Object::Instruction(Op::Frame));
        ret.call_by_name_code.push(Object::Number(8));
        ret.call_by_name_code
            .push(Object::Instruction(Op::Constant));
        ret.call_by_name_code.push(Object::Unspecified);
        ret.call_by_name_code.push(Object::Instruction(Op::Push));
        ret.call_by_name_code
            .push(Object::Instruction(Op::ReferGlobal));
        ret.call_by_name_code.push(Object::Unspecified);
        ret.call_by_name_code.push(Object::Instruction(Op::Call));
        ret.call_by_name_code.push(Object::Number(1));
        ret.call_by_name_code.push(Object::Instruction(Op::Halt));

        ret
    }

    pub fn intern(&mut self, s: &str) -> GcRef<Symbol> {
        self.gc.intern(s)
    }

    pub fn values(&mut self, values: &[Object]) -> Object {
        let n = values.len();
        self.num_values = n;
        if 0 == n {
            return Object::Unspecified;
        }
        for i in 1..n as usize {
            if i >= MAX_NUM_VALUES {
                panic!("values: too many values");
            }
            self.values[i - 1] = values[i];
        }
        // this is set to ac later.
        return values[0];
    }

    fn initialize_free_vars(&mut self, ops: *const Object, ops_len: usize) {
        let free_vars = default_free_vars(&mut self.gc);
        let mut display = self.gc.alloc(Closure::new(
            ops,
            ops_len,
            0,
            false,
            free_vars,
            Object::False,
        ));

        display.prev = self.dc;
        let free_vars = default_free_vars(&mut self.gc);
        self.closure_for_evaluate = Object::Closure(self.gc.alloc(Closure::new(
            null(),
            0,
            0,
            false,
            free_vars,
            Object::False,
        )));
        self.dc = Object::Closure(display);
    }

    pub fn mark_and_sweep(&mut self) {
        if self.gc.should_gc() {
            #[cfg(feature = "debug_log_gc")]
            println!("-- gc begin");

            self.mark_roots();
            self.gc.collect_garbage();

            #[cfg(feature = "debug_log_gc")]
            println!("-- gc end");
        }
    }

    fn load_compiler(&mut self) -> Object {
        let mut fasl = Fasl {
            bytes: compiler::U8_ARRAY,
        };
        self.lib_compiler = if self.should_load_compiler {
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };
        self.run_ops(self.lib_compiler.as_ptr())
    }

    pub fn enable_r7rs(&mut self, args: Object) -> Object {
        let mut fasl = Fasl {
            bytes: psyntax::U8_ARRAY,
        };
        self.lib_psyntax = if self.should_load_compiler {
            // Global variables.
            let sym = self.gc.symbol_intern("%verbose");
            self.set_global_value(sym.to_symbol(), Object::True);

            let sym = self.gc.symbol_intern("%clean-acc");
            self.set_global_value(sym.to_symbol(), Object::False);

            let sym = self.gc.symbol_intern("%disable-acc");
            self.set_global_value(sym.to_symbol(), Object::True);

            let sym = self.gc.symbol_intern("*command-line-args*");
            let args = args;
            self.set_global_value(sym.to_symbol(), args);

            let sym = self.gc.symbol_intern("%loadpath");
            let path = self.gc.new_string(".");
            self.set_global_value(sym.to_symbol(), path);

            let sym = self.gc.symbol_intern("%vm-import-spec");
            self.set_global_value(sym.to_symbol(), Object::False);
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };

        self.run(self.lib_psyntax.as_ptr(), self.lib_psyntax.len())
    }

    #[inline(always)]
    fn bool_operand(&mut self) -> bool {
        self.operand().to_bool()
    }

    #[inline(always)]
    fn isize_operand(&mut self) -> isize {
        self.operand().to_number()
    }

    #[inline(always)]
    fn symbol_operand(&mut self) -> GcRef<Symbol> {
        self.operand().to_symbol()
    }

    #[inline(always)]
    fn usize_operand(&mut self) -> usize {
        self.operand().to_number() as usize
    }

    #[inline(always)]
    fn make_frame(&mut self, next_pc: *const Object) {
        self.push(Object::ProgramCounter(next_pc));
        self.push(self.dc);
        // TODO: This should be cl register.
        self.push(self.dc);
        self.push(Object::ObjectPointer(self.fp));
    }

    pub fn set_symbol_value(&mut self, symbol: GcRef<Symbol>, value: Object) {
        self.globals.insert(symbol, value);
    }

    // Helpers.
    fn pop(&mut self) -> Object {
        unsafe {
            self.sp = self.dec(self.sp, 1);
            *self.sp
        }
    }

    fn push(&mut self, value: Object) {
        unsafe {
            *self.sp = value;
            self.sp = self.inc(self.sp, 1);
        }
    }

    fn index(&self, sp: *mut Object, n: isize) -> Object {
        unsafe { *self.dec(sp, n + 1) }
    }

    fn index_set(&mut self, sp: *mut Object, n: isize, obj: Object) {
        unsafe { *self.dec(sp, n + 1) = obj }
    }

    fn stack_len(&self) -> usize {
        unsafe { self.sp.offset_from(self.stack.as_ptr()) as usize }
    }

    #[cfg(feature = "debug_log_vm")]
    fn print_vm(&mut self, op: Op) {
        println!("-----------------------------------------");
        println!("{} executed", op);
        println!("  ac={}", self.ac);
        println!("  dc={}", self.dc);
        println!("-----------------------------------------");
        let fp_idx = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
        for i in 0..self.stack_len() {
            println!(
                "  {}{}",
                self.stack[i],
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

    #[inline(always)]
    fn set_return_value(&mut self, obj: Object) {
        self.ac = obj;
        self.num_values = 1;
    }

    #[inline(always)]
    fn refer_local(&mut self, n: isize) -> Object {
        unsafe { *self.fp.offset(n) }
    }

    #[inline(always)]
    fn jump(&self, pc: *const Object, offset: isize) -> *const Object {
        unsafe { pc.offset(offset) }
    }

    #[inline(always)]
    fn operand(&mut self) -> Object {
        let obj = unsafe { *self.pc };
        let next_pc = self.jump(self.pc, 1);
        self.pc = next_pc;
        //unsafe { *next_pc }
        return obj;
    }

    #[inline(always)]
    fn inc(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(offset) }
    }

    #[inline(always)]
    fn dec(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(-offset) }
    }

    fn arg_err(&self, who: &str, expected: &str, actual: Object) {
        panic!("{}: requires {} but got {}", who, expected, actual);
    }

    fn return_n(&mut self, n: isize) {
        #[cfg(feature = "debug_log_vm")]
        println!("  return {}", n);
        let sp = self.dec(self.sp, n);
        match self.index(sp, 0) {
            Object::ObjectPointer(fp) => {
                self.fp = fp;
            }
            obj => {
                panic!("not fp pointer but {}", obj)
            }
        }
        // TODO: Take care of cl register.
        // self.cl = index(sp, 1);
        self.dc = self.index(sp, 2);
        match self.index(sp, 3) {
            Object::ProgramCounter(next_pc) => {
                self.pc = next_pc;
            }
            _ => {
                panic!("not a pc");
            }
        }
        self.sp = self.dec(sp, 4);
    }

    fn shift_args_to_bottom(&mut self, sp: *mut Object, depth: isize, diff: isize) -> *mut Object {
        let mut i = depth - 1;
        while i >= 0 {
            self.index_set(sp, i + diff, self.index(self.sp, i));
            i = i - 1;
        }
        self.dec(sp, diff)
    }

    fn unshift_args(&mut self, sp: *mut Object, diff: isize) -> *mut Object {
        for i in 0..diff {
            self.index_set(self.inc(sp, diff - i), 0, self.index(sp, 1));
        }
        self.inc(sp, diff)
    }

    fn stack_to_pair(&mut self, n: isize) -> Object {
        let mut args = Object::Nil;
        for i in 0..n {
            args = self.gc.cons(self.index(self.sp, i), args);
        }
        args
    }

    pub fn set_rtd(&mut self, key: Object, rtd: Object) {
        self.rtds.insert(key, rtd);
    }

    pub fn lookup_rtd(&self, key: Object) -> Object {
        match self.rtds.get(&key) {
            Some(&value) => value,
            _ => Object::False,
        }
    }

    pub fn set_global_value(&mut self, key: GcRef<Symbol>, value: Object) {
        self.globals.insert(key, value);
    }

    pub fn global_value(&mut self, key: GcRef<Symbol>) -> Option<&Object> {
        self.globals.get(&key)
    }

    pub fn current_input_port(&self) -> Object {
        self.current_input_port
    }

    pub fn set_current_input_port(&mut self, port: Object) {
        self.current_input_port = port;
    }

    pub fn read(&mut self) -> Result<Object, ReadError> {
        match self.current_input_port {
            Object::FileInputPort(mut port) => port.read(&mut self.gc),
            _ => {
                panic!(
                    "read: input-port required but got {}",
                    self.current_input_port
                )
            }
        }
    }
    pub fn eval_after(&mut self, sexp: Object) -> Object {
        let name = self.gc.symbol_intern("compile");
        let v = self.call_by_name(name, sexp).to_vector(); //self.compile(sexp).to_vector();
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

    pub fn set_after_trigger0(&mut self, closure: Object) -> Object {
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
