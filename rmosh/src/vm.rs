use once_cell::sync::Lazy;
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
    bug, compiler,
    error::SchemeError,
    fasl::FaslReader,
    gc::{Gc, GcRef},
    obj_as_text_input_port_mut_or_panic,
    objects::{Closure, Object, Symbol},
    op::Op,
    ports::{
        EolStyle, ErrorHandlingMode, StdErrorPort, StdInputPort, StdOutputPort, TextInputPort,
        TranscodedInputPort, TranscodedOutputPort, Transcoder, UTF8Codec,
    },
    procs::default_free_vars,
    psyntax,
};

// This is introduced to support custom binary output port where we need vm.call_closure3().
// We tried to pass Vm to the methods, but it turned out it breaks to_string() family badly.
// So we decided to have this kind of global accessible Vm. We use this as less as possible to keep this code clean.
pub static mut CURRENT_VM: Lazy<Vm> = Lazy::new(Vm::new);

const STACK_SIZE: usize = 65536;
const MAX_NUM_VALUES: usize = 256;

struct Registers {
    pub ac: Object,
    pub dc: Object,
    pub cl: Object,
    pub pc: *const Object,
    pub sp_offset: isize,
    pub fp_offset: isize,
}

impl Registers {
    fn new() -> Self {
        Self {
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            cl: Object::Unspecified,
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
    stack: Vec<Object>,
    // accumulator register.
    pub ac: Object,
    // current closure register, used for profiler.
    cl: Object,
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
    pub dynamic_code_array: Vec<Vec<Object>>,
    pub call_by_name_code: Vec<Object>,
    pub call_closure0_code: Vec<Object>,
    pub call_closure1_code: Vec<Object>,
    pub call_closure3_code: Vec<Object>,
    pub closure_for_evaluate: Object,
    current_input_port: Object,
    current_output_port: Object,
    current_error_port: Object,
    saved_registers: Registers,
    // Note when we add new vars here, please make sure we take care of them in mark_roots.
    // Otherwise they can cause memory leak or double free.
    default_free_vars: Vec<Object>,
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}

impl Vm {
    pub fn new() -> Self {
        let mut gc = Box::new(Gc::new());
        let free_vars = default_free_vars(&mut gc);
        let mut ret = Self {
            gc,
            stack: vec![Object::Unspecified; STACK_SIZE],
            ac: Object::Unspecified,
            dc: Object::Unspecified,
            cl: Object::Unspecified,
            expected: Object::Unspecified,
            pc: null_mut(),
            sp: null_mut(),
            fp: null_mut(),
            globals: HashMap::new(),
            lib_compiler: vec![],
            lib_psyntax: vec![],
            dynamic_winders: Object::Nil,
            num_values: 0,
            values: [Object::Unspecified; MAX_NUM_VALUES],
            rtds: HashMap::new(),
            should_load_compiler: false,
            is_initialized: false,
            dynamic_code_array: vec![],
            call_by_name_code: vec![],
            call_closure0_code: vec![],
            call_closure1_code: vec![],
            call_closure3_code: vec![],
            closure_for_evaluate: Object::Unspecified,
            current_input_port: Object::Unspecified,
            current_output_port: Object::Unspecified,
            current_error_port: Object::Unspecified,
            saved_registers: Registers::new(),
            default_free_vars: free_vars,
        };
        let raw_stdport = Object::StdOutputPort(ret.gc.alloc(StdOutputPort::new()));
        let codec = Object::UTF8Codec(ret.gc.alloc(UTF8Codec::new()));
        let eol_style = if std::env::consts::OS == "windows" {
            EolStyle::CrLf
        } else {
            EolStyle::Lf
        };
        let transcoder = Object::Transcoder(ret.gc.alloc(Transcoder::new(
            codec,
            eol_style,
            ErrorHandlingMode::RaiseError,
        )));
        ret.current_output_port = Object::TranscodedOutputPort(
            ret.gc
                .alloc(TranscodedOutputPort::new(raw_stdport, transcoder)),
        );

        let raw_errport = Object::StdErrorPort(ret.gc.alloc(StdErrorPort::new()));
        let codec = Object::UTF8Codec(ret.gc.alloc(UTF8Codec::new()));
        let eol_style = if std::env::consts::OS == "windows" {
            EolStyle::CrLf
        } else {
            EolStyle::Lf
        };
        let transcoder = Object::Transcoder(ret.gc.alloc(Transcoder::new(
            codec,
            eol_style,
            ErrorHandlingMode::RaiseError,
        )));
        ret.current_error_port = Object::TranscodedOutputPort(
            ret.gc
                .alloc(TranscodedOutputPort::new(raw_errport, transcoder)),
        );

        let raw_stdinport = Object::StdInputPort(ret.gc.alloc(StdInputPort::new()));
        let codec = Object::UTF8Codec(ret.gc.alloc(UTF8Codec::new()));
        let eol_style = if std::env::consts::OS == "windows" {
            EolStyle::CrLf
        } else {
            EolStyle::Lf
        };
        let transcoder = Object::Transcoder(ret.gc.alloc(Transcoder::new(
            codec,
            eol_style,
            ErrorHandlingMode::RaiseError,
        )));
        ret.current_input_port = Object::TranscodedInputPort(
            ret.gc
                .alloc(TranscodedInputPort::new(raw_stdinport, transcoder)),
        );

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

        ret.call_closure0_code.push(Object::Instruction(Op::Frame));
        ret.call_closure0_code.push(Object::Fixnum(5));
        ret.call_closure0_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure0_code.push(Object::Unspecified);
        ret.call_closure0_code.push(Object::Instruction(Op::Call));
        ret.call_closure0_code.push(Object::Fixnum(0));
        ret.call_closure0_code.push(Object::Instruction(Op::Halt));

        ret.call_closure1_code.push(Object::Instruction(Op::Frame));
        ret.call_closure1_code.push(Object::Fixnum(8));
        ret.call_closure1_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure1_code.push(Object::Unspecified);
        ret.call_closure1_code.push(Object::Instruction(Op::Push));
        ret.call_closure1_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure1_code.push(Object::Unspecified);
        ret.call_closure1_code.push(Object::Instruction(Op::Call));
        ret.call_closure1_code.push(Object::Fixnum(1));
        ret.call_closure1_code.push(Object::Instruction(Op::Halt));

        ret.call_closure3_code.push(Object::Instruction(Op::Frame));
        ret.call_closure3_code.push(Object::Fixnum(14));
        ret.call_closure3_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure3_code.push(Object::Unspecified);
        ret.call_closure3_code.push(Object::Instruction(Op::Push));
        ret.call_closure3_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure3_code.push(Object::Unspecified);
        ret.call_closure3_code.push(Object::Instruction(Op::Push));
        ret.call_closure3_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure3_code.push(Object::Unspecified);
        ret.call_closure3_code.push(Object::Instruction(Op::Push));
        ret.call_closure3_code
            .push(Object::Instruction(Op::Constant));
        ret.call_closure3_code.push(Object::Unspecified);
        ret.call_closure3_code.push(Object::Instruction(Op::Call));
        ret.call_closure3_code.push(Object::Fixnum(3));
        ret.call_closure3_code.push(Object::Instruction(Op::Halt));
        ret
    }

    pub fn enable_r7rs(
        &mut self,
        args: Object,
        loadpath: Option<String>,
    ) -> Result<Object, SchemeError> {
        let mut fasl = FaslReader::new(psyntax::U8_ARRAY);
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
            let mut paths = String::new();
            paths.push_str("/embed/stdlib");
            if let Some(loadpath) = loadpath {
                paths.push(':');
                paths.push_str(&loadpath);
            }
            let paths = self.gc.new_string(&paths);
            self.set_global_value(sym.to_symbol(), paths);

            let sym = self.gc.symbol_intern("%vm-import-spec");
            self.set_global_value(sym.to_symbol(), Object::False);
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };

        self.run(self.lib_psyntax.as_ptr(), self.lib_psyntax.len())
    }

    pub fn values(&mut self, values: &[Object]) -> Result<Object, SchemeError> {
        let n = values.len();
        self.num_values = n;
        if 0 == n {
            return Ok(Object::Unspecified);
        }
        for (i, value) in values.iter().enumerate().take(n).skip(1) {
            if i >= MAX_NUM_VALUES {
                return Err(SchemeError::assertion_violation(
                    "values",
                    "Too many values",
                    &[],
                ));
            }
            self.values[i - 1] = *value;
        }
        // this is set to ac later.
        Ok(values[0])
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
        self.current_error_port
    }

    pub fn current_input_port(&self) -> Object {
        self.current_input_port
    }

    pub fn set_current_input_port(&mut self, port: Object) {
        self.current_input_port = port;
    }

    pub fn set_current_output_port(&mut self, port: Object) {
        self.current_output_port = port;
    }

    pub fn read(&mut self) -> Result<Object, SchemeError> {
        let port = obj_as_text_input_port_mut_or_panic!(self.current_input_port);
        port.read(self)
    }
    fn initialize_free_vars(&mut self, ops: *const Object, ops_len: usize) {
        let mut display = self.gc.alloc(Closure::new(
            ops,
            ops_len,
            0,
            false,
            self.default_free_vars.to_vec(),
            Object::False,
        ));

        display.prev = self.dc;

        let top_level = self.gc.symbol_intern("<top-level>");
        let src = self.gc.list2(Object::False, top_level);
        self.closure_for_evaluate = Object::Closure(self.gc.alloc(Closure::new(
            null(),
            0,
            0,
            false,
            self.default_free_vars.to_vec(),
            src,
        )));
        self.dc = Object::Closure(display);
    }

    fn load_compiler(&mut self) -> Result<Object, SchemeError> {
        let mut fasl = FaslReader::new(compiler::U8_ARRAY);
        self.lib_compiler = if self.should_load_compiler {
            fasl.read_all_sexp(&mut self.gc)
        } else {
            vec![Object::Instruction(Op::Halt)]
        };
        self.run_ops(self.lib_compiler.as_ptr())
    }
}
