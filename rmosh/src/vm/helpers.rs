use crate::{
    bug,
    gc::GcRef,
    objects::{Object, Symbol},
    op::Op,
    vm::Vm,
};

use super::STACK_SIZE;

impl Vm {
    #[inline(always)]
    pub(super) fn return_n(&mut self, n: isize) {
        #[cfg(feature = "debug_log_vm")]
        println!("  return {}", n);
        let sp = self.dec(self.sp, n);
        match self.index(sp, 0) {
            Object::ObjectPointer(fp) => {
                self.fp = fp;
            }
            obj => {
                bug!("not fp pointer but {}", obj)
            }
        }

        self.cl = self.index(sp, 1);
        self.dc = self.index(sp, 2);
        match self.index(sp, 3) {
            Object::ProgramCounter(next_pc) => {
                self.pc = next_pc;
            }
            _ => {
                bug!("not a pc");
            }
        }
        self.sp = self.dec(sp, 4);
    }

    #[inline(always)]
    pub(super) fn stack_to_pair(&mut self, n: isize) -> Object {
        let mut args = Object::Nil;
        for i in 0..n {
            args = self.gc.cons(self.index(self.sp, i), args);
        }
        args
    }

    #[inline(always)]
    pub(super) fn bool_operand(&mut self) -> bool {
        self.operand().to_bool()
    }

    #[inline(always)]
    pub(super) fn isize_operand(&mut self) -> isize {
        self.operand().to_isize()
    }

    #[inline(always)]
    pub(super) fn symbol_operand(&mut self) -> GcRef<Symbol> {
        self.operand().to_symbol()
    }

    #[inline(always)]
    pub(super) fn usize_operand(&mut self) -> usize {
        self.operand().to_isize() as usize
    }

    #[inline(always)]
    pub(super) fn make_frame(&mut self, next_pc: *const Object) {
        self.push(Object::ProgramCounter(next_pc));
        self.push(self.dc);
        self.push(self.cl);
        self.push(Object::ObjectPointer(self.fp));
    }

    #[inline(always)]
    pub(super) fn pop(&mut self) -> Object {
        unsafe {
            self.sp = self.dec(self.sp, 1);
            *self.sp
        }
    }

    #[inline(always)]
    pub(super) fn push(&mut self, value: Object) {
        unsafe {
            *self.sp = value;
            self.sp = self.inc(self.sp, 1);
            if self.stack_len() >= STACK_SIZE {
                bug!("Stack overflow")
            }
        }
    }

    #[inline(always)]
    pub(super) fn index(&self, sp: *mut Object, n: isize) -> Object {
        unsafe { *self.dec(sp, n + 1) }
    }

    #[inline(always)]
    pub(super) fn index_set(&mut self, sp: *mut Object, n: isize, obj: Object) {
        unsafe { *self.dec(sp, n + 1) = obj }
    }

    #[inline(always)]
    pub(super) fn stack_len(&self) -> usize {
        unsafe { self.sp.offset_from(self.stack.as_ptr()) as usize }
    }

    #[inline(always)]
    pub(super) fn shift_args_to_bottom(
        &mut self,
        sp: *mut Object,
        depth: isize,
        diff: isize,
    ) -> *mut Object {
        let mut i = depth - 1;
        while i >= 0 {
            self.index_set(sp, i + diff, self.index(self.sp, i));
            i = i - 1;
        }
        self.dec(sp, diff)
    }

    #[inline(always)]
    pub(super) fn unshift_args(&mut self, sp: *mut Object, diff: isize) -> *mut Object {
        for i in 0..diff {
            self.index_set(self.inc(sp, diff - i), 0, self.index(sp, 1));
        }
        self.inc(sp, diff)
    }

    #[inline(always)]
    pub(super) fn set_return_value(&mut self, obj: Object) {
        self.ac = obj;
        self.num_values = 1;
    }

    #[inline(always)]
    pub(super) fn refer_local(&mut self, n: isize) -> Object {
        unsafe { *self.fp.offset(n) }
    }

    #[inline(always)]
    pub(super) fn jump(&self, pc: *const Object, offset: isize) -> *const Object {
        unsafe { pc.offset(offset) }
    }

    #[inline(always)]
    pub(super) fn operand(&mut self) -> Object {
        let obj = unsafe { *self.pc };
        let next_pc = self.jump(self.pc, 1);
        self.pc = next_pc;
        //unsafe { *next_pc }
        return obj;
    }

    #[inline(always)]
    pub(super) fn inc(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(offset) }
    }

    #[inline(always)]
    pub(super) fn dec(&self, pointer: *mut Object, offset: isize) -> *mut Object {
        unsafe { pointer.offset(-offset) }
    }
    pub(super) fn save_registers(&mut self) {
        // Found we already stored something.
        assert!(self.saved_registers.ac.is_unspecified());

        self.saved_registers.ac = self.ac;
        self.saved_registers.dc = self.dc;
        self.saved_registers.cl = self.cl;
        self.saved_registers.pc = self.pc;
        self.saved_registers.sp_offset = unsafe { self.sp.offset_from(self.stack.as_ptr()) };
        self.saved_registers.fp_offset = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
    }

    pub(super) fn restore_registers(&mut self) {
        self.ac = self.saved_registers.ac;
        self.dc = self.saved_registers.dc;
        self.cl = self.saved_registers.cl;
        self.pc = self.saved_registers.pc;
        self.sp = unsafe {
            self.stack
                .as_mut_ptr()
                .offset(self.saved_registers.sp_offset)
        };
        self.fp = unsafe {
            self.stack
                .as_mut_ptr()
                .offset(self.saved_registers.fp_offset)
        };
        self.saved_registers.ac = Object::Unspecified;
    }

    // We have to alocate (return n) code dymaically because recursive calls can happen.
    pub(super) fn allocate_return_code(&mut self, argc: isize) -> *const Object {
        self.dynamic_code_array.push(vec![]);
        let code = self.dynamic_code_array.last_mut().unwrap();
        code.push(Object::Instruction(Op::Return));
        code.push(Object::Fixnum(argc));
        code.as_ptr()
    }

    pub(super) fn allocate_code(&mut self, src: &Vec<Object>) -> *const Object {
        self.dynamic_code_array.push(vec![]);
        let code = self.dynamic_code_array.last_mut().unwrap();
        for obj in src.iter() {
            code.push(*obj);
        }
        code.as_ptr()
    }

    pub fn show_stack_trace(&mut self) {
        eprintln!(" Stack trace:");
        let mut i = 1;
        let mut fp = self.fp;
        let mut cl = self.cl;
        // ======================
        //          pc*
        // ======================
        //          dc
        // ======================
        //          cl
        // ======================
        //          fp
        // ======== sp ==========
        loop {
            match cl {
                Object::Procedure(proc) => {
                    eprintln!("    {}. {}: <subr>", i, proc.name);
                    i += 1;
                }
                Object::Closure(closure) => {
                    if closure.src.is_pair() {
                        eprint!("    {}. ", i);
                        let proc = closure.src.cdr_unchecked();
                        let location = closure.src.car_unchecked();
                        if location.is_false() {
                            eprintln!("{}: <unknown location>", proc)
                        } else {
                            let file = location.car_unchecked();
                            let lineno = location.cdr_unchecked().car_unchecked();
                            // anonymous procedure
                            let proc_name = proc.car_unchecked();
                            if proc_name == self.gc.symbol_intern("lambda") {
                                // format source information to follwing style
                                // (lambda (arg1 arg2 arg3) ...)
                                let args = proc.cdr_unchecked();
                                let body = self.gc.symbol_intern("...");
                                let proc_src = self.gc.listn(&[proc_name, args, body]);
                                eprintln!("{}:  {}:{}", proc_src, file, lineno);
                            } else {
                                eprintln!("{}:  {}:{}", proc_name, file, lineno);
                            }
                        }
                        i += 1;
                    }
                }
                _ => (),
            }
            if fp > self.stack.as_mut_ptr() {
                const CL_OFFSET: isize = 2;
                const FP_OFFSET: isize = 1;
                cl = unsafe { *self.dec(fp, CL_OFFSET) };

                if !cl.is_closure() && !cl.is_procedure() {
                    break;
                }

                let next_fp = self.dec(fp, FP_OFFSET);
                let next_fp_obj = unsafe { *next_fp };
                match next_fp_obj {
                    Object::ObjectPointer(pointer) => fp = pointer,
                    _ => {
                        bug!("object pointer expected but got {}", next_fp_obj)
                    }
                };
            } else {
                break;
            }
            if i > 20 {
                break;
            }
        }
    }
}
