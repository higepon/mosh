use crate::{objects::Object, vm::Vm};

impl Vm {
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
