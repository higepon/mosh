use crate::{vm::Vm, objects::Object};

impl Vm {
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