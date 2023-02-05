use std::{
    collections::HashMap,
    env,
    ptr::{null, null_mut},
};

// Sub module definitions.
mod eval;
mod helpers;
mod mark;
mod ops;
mod run;

use crate::{
    compiler,
    fasl::FaslReader,
    gc::{Gc, GcRef},
    objects::{Closure, Object, Symbol},
    op::Op,
    ports::{ReadError, StdErrorPort, StdOutputPort, TextInputPort},
    procs::default_free_vars,
    psyntax,
};

const STACK_SIZE: usize = 65536;
const MAX_NUM_VALUES: usize = 256;

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
    pub eval_code_array: Vec<Vec<Object>>,
    pub eval_ret_code: Vec<Object>,
    pub ret_code: Vec<Object>,
    pub call_by_name_code: Vec<Object>,
    pub closure_for_evaluate: Object,
    current_input_port: Object,
    current_output_port: Object,
    current_error_port: Object,
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
            eval_code_array: vec![],
            eval_ret_code: vec![],
            ret_code: vec![],
            call_by_name_code: vec![],
            closure_for_evaluate: Object::Unspecified,
            current_input_port: Object::Unspecified,
            current_output_port: Object::Unspecified,
            current_error_port: Object::Unspecified,
            saved_registers: Registers::new(),
        };
        ret.current_output_port = Object::StdOutputPort(ret.gc.alloc(StdOutputPort::new()));
        ret.current_error_port = Object::StdErrorPort(ret.gc.alloc(StdErrorPort::new()));
        ret.trigger0_code.push(Object::Instruction(Op::Constant));
        ret.trigger0_code.push(Object::Unspecified);
        ret.trigger0_code.push(Object::Instruction(Op::Call));
        ret.trigger0_code.push(Object::Fixnum(0));
        ret.trigger0_code.push(Object::Instruction(Op::Return));
        ret.trigger0_code.push(Object::Fixnum(0));
        ret.trigger0_code.push(Object::Instruction(Op::Halt));

        ret.call_by_name_code.push(Object::Instruction(Op::Frame));
        ret.call_by_name_code.push(Object::Fixnum(8));
        ret.call_by_name_code
            .push(Object::Instruction(Op::Constant));
        ret.call_by_name_code.push(Object::Unspecified);
        ret.call_by_name_code.push(Object::Instruction(Op::Push));
        ret.call_by_name_code
            .push(Object::Instruction(Op::ReferGlobal));
        ret.call_by_name_code.push(Object::Unspecified);
        ret.call_by_name_code.push(Object::Instruction(Op::Call));
        ret.call_by_name_code.push(Object::Fixnum(1));
        ret.call_by_name_code.push(Object::Instruction(Op::Halt));

        ret
    }

    pub fn enable_r7rs(&mut self, args: Object) -> Object {
        let mut fasl = FaslReader {
            bytes: psyntax::U8_ARRAY,
            shared_objects: &mut HashMap::new(),
        };
        self.lib_psyntax = if self.should_load_compiler {
            env::set_var("MOSH_CACHE_DIR", "/.rmosh");
            // Global variables.
            let sym = self.gc.symbol_intern("%verbose");
            self.set_global_value(sym.to_symbol(), Object::True);

            let sym = self.gc.symbol_intern("%optimize?");
            self.set_global_value(sym.to_symbol(), Object::False);

            let sym = self.gc.symbol_intern("%clean-acc");
            self.set_global_value(sym.to_symbol(), Object::False);

            let sym = self.gc.symbol_intern("%disable-acc");
            self.set_global_value(sym.to_symbol(), Object::False);

            let sym = self.gc.symbol_intern("*command-line-args*");
            let args = args;
            self.set_global_value(sym.to_symbol(), args);

            let sym = self.gc.symbol_intern("%loadpath");
            // TODO: tentative.
            let path = self.gc.new_string("/root/mosh.git/lib");
            self.set_global_value(sym.to_symbol(), path);

            let sym = self.gc.symbol_intern("%vm-import-spec");
            self.set_global_value(sym.to_symbol(), Object::False);
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };

        self.run(self.lib_psyntax.as_ptr(), self.lib_psyntax.len())
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

    pub fn set_symbol_value(&mut self, symbol: GcRef<Symbol>, value: Object) {
        self.globals.insert(symbol, value);
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

    pub fn current_output_port(&self) -> Object {
        self.current_output_port
    }

    pub fn current_error_port(&self) -> Object {
        self.current_output_port
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

    fn load_compiler(&mut self) -> Object {
        let mut fasl = FaslReader {
            bytes: compiler::U8_ARRAY,
            shared_objects: &mut HashMap::new(),
        };
        self.lib_compiler = if self.should_load_compiler {
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };
        self.run_ops(self.lib_compiler.as_ptr())
    }
}
