use std::ptr::null;

use crate::{
    gc::GcRef,
    objects::{Object, Symbol},
};

use super::Vm;

impl Vm {
    pub(super) fn reset_roots(&mut self) {
        // Clean up display closure so that Objects in ops can be freed.
        let mut closure = self.dc.to_closure();
        closure.ops = null();
        closure.ops_len = 0;
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

    pub(super) fn mark_roots(&mut self) {
        // Ports.
        self.gc.mark_object(self.current_input_port);
        self.gc.mark_object(self.current_error_port);
        self.gc.mark_object(self.current_output_port);

        for eval_code in &self.dynamic_code_array {
            for obj in eval_code {
                self.gc.mark_object(*obj);
            }
        }

        for &obj in &self.call_by_name_code {
            self.gc.mark_object(obj);
        }

        for &obj in &self.call_closure1_code {
            self.gc.mark_object(obj);
        }
        for &obj in &self.call_closure3_code {
            self.gc.mark_object(obj);
        }


        self.gc.mark_object(self.saved_registers.ac);
        self.gc.mark_object(self.saved_registers.dc);
        self.gc.mark_object(self.saved_registers.cl);

        self.gc.mark_object(self.closure_for_evaluate);

        // Base library ops.
        for op in &self.lib_compiler {
            self.gc.mark_object(*op);
        }
        for op in &self.lib_psyntax {
            self.gc.mark_object(*op);
        }

        self.gc.mark_object(self.dynamic_winders);

        // Stack.
        for &obj in &self.stack[0..self.stack_len()] {
            self.gc.mark_object(obj);
        }

        // Values.
        for &obj in &self.values[0..self.num_values] {
            self.gc.mark_object(obj);
        }

        // Symbols.
        let symbols = self
            .gc
            .symbols
            .values()
            .cloned()
            .collect::<Vec<GcRef<Symbol>>>();
        for symbol in symbols {
            self.gc.mark_object(Object::Symbol(symbol));
        }

        // Global variables.
        for &obj in self.globals.values() {
            self.gc.mark_object(obj);
        }

        // RTDs.
        for (k, v) in self.rtds.iter() {
            self.gc.mark_object(*k);
            self.gc.mark_object(*v);
        }

        // Registers.
        self.gc.mark_object(self.ac);
        self.gc.mark_object(self.cl);
        self.gc.mark_object(self.dc);
        self.gc.mark_object(self.expected);
    }
}
