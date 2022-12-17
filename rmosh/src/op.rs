use crate::{
    gc::GcRef,
    objects::{Object, Symbol},
};

#[derive(Copy, Clone, Debug)]
pub enum Op {
    NumberAdd,
    AddPair,
    AssignFree(usize),
    AssignLocal(isize),
    Box(isize),
    Call(isize),
    Car,
    Cdr,
    Cadr,
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
    NumberEqual,
    Push,
    ReferFree(usize),
    ReferGlobal(GcRef<Symbol>),
    ReferLocal(isize),
    Return(isize),
    TailCall(isize, isize),
    Test(usize),
    Undef,
}
