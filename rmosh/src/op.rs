use crate::{
    gc::GcRef,
    objects::{Object, Symbol},
};

#[derive(Copy, Clone, Debug)]
pub enum Op {
    NumberAdd,
    AddPair,
    AssignLocal(isize),
    Box(isize),
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
    Halt,
    Indirect,
    Leave(isize),
    LetFrame(isize),
    LocalJmp(usize),
    Nop,
    Push,
    ReferFree(usize),
    ReferGlobal(GcRef<Symbol>),
    ReferLocal(isize),
    Return(isize),
    Test(usize),
    Undef,
}
