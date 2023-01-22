use crate::{gc::GcRef, objects::{Object, Symbol}, vm::Vm};

impl Vm {
    #[inline(always)]
    pub(super) fn bool_operand(&mut self) -> bool {
        self.operand().to_bool()
    }

    #[inline(always)]
    pub(super) fn isize_operand(&mut self) -> isize {
        self.operand().to_number()
    }

    #[inline(always)]
    pub(super) fn symbol_operand(&mut self) -> GcRef<Symbol> {
        self.operand().to_symbol()
    }

    #[inline(always)]
    pub(super) fn usize_operand(&mut self) -> usize {
        self.operand().to_number() as usize
    }

    #[inline(always)]
    pub(super) fn make_frame(&mut self, next_pc: *const Object) {
        self.push(Object::ProgramCounter(next_pc));
        self.push(self.dc);
        // TODO: This should be cl register.
        self.push(self.dc);
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
        self.saved_registers.pc = self.pc;
        self.saved_registers.sp_offset = unsafe { self.sp.offset_from(self.stack.as_ptr()) };
        self.saved_registers.fp_offset = unsafe { self.fp.offset_from(self.stack.as_ptr()) };
    }

    pub(super) fn restore_registers(&mut self) {
        self.ac = self.saved_registers.ac;
        self.dc = self.saved_registers.dc;
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
}