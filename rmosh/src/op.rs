use crate::{
    gc::GcRef,
    objects::{Object, Symbol},
};

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Add,
    AddPair,
    Call(isize),
    Cons,
    Constant(Object),
    Closure {
        size: usize,
        arg_len: isize,
        is_optional_arg: bool,
        num_free_vars: isize,
    },
    DefineGlobal(GcRef<Symbol>),
    Display(isize),
    Enter(isize),
    Frame(usize),
    Leave(isize),
    LetFrame(isize),
    LocalJump(usize),
    Push,
    ReferFree(usize),
    ReferGlobal(GcRef<Symbol>),
    ReferLocal(isize),
    Return(isize),
    Test(usize),
}
