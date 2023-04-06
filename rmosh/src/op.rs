use std::fmt::{self, Display};

use num_derive::FromPrimitive;

#[repr(u8)]
#[derive(Clone, Copy, Debug, FromPrimitive, Hash, PartialEq)]
pub enum Op {
    CompileError = 0,
    BranchNotLe = 1,
    BranchNotGe = 2,
    BranchNotLt = 3,
    BranchNotGt = 4,
    BranchNotNull = 5,
    BranchNotNumberEqual = 6,
    BranchNotEq = 7,
    BranchNotEqv = 8,
    BranchNotEqual = 9,
    Append2 = 10,
    Call = 11,
    Apply = 12,
    Push = 13,
    AssignFree = 14,
    AssignGlobal = 15,
    AssignLocal = 16,
    Box = 17,
    Caar = 18,
    Cadr = 19,
    Car = 20,
    Cdar = 21,
    Cddr = 22,
    Cdr = 23,
    Closure = 24,
    Cons = 25,
    Constant = 26,
    DefineGlobal = 27,
    Display = 28,
    Enter = 29,
    Eq = 30,
    Eqv = 31,
    Equal = 32,
    Frame = 33,
    Indirect = 34,
    Leave = 35,
    LetFrame = 36,
    List = 37,
    LocalJmp = 38,
    MakeContinuation = 39,
    MakeVector = 40,
    Nop = 41,
    Not = 42,
    NullP = 43,
    NumberAdd = 44,
    NumberEqual = 45,
    NumberGe = 46,
    NumberGt = 47,
    NumberLe = 48,
    NumberLt = 49,
    NumberMul = 50,
    NumberDiv = 51,
    NumberSub = 52,
    PairP = 53,
    Read = 54,
    ReadChar = 55,
    Reduce = 56,
    ReferFree = 57,
    ReferGlobal = 58,
    ReferLocal = 59,
    RestoreContinuation = 60,
    Return = 61,
    SetCar = 62,
    SetCdr = 63,
    Shift = 64,
    SymbolP = 65,
    Test = 66,
    Values = 67,
    Receive = 68,
    UnfixedJump = 69,
    Stop = 70,
    Shiftj = 71,
    Undef = 72,
    VectorLength = 73,
    VectorP = 74,
    VectorRef = 75,
    VectorSet = 76,
    PushEnter = 77,
    Halt = 78,
    ConstantPush = 79,
    NumberSubPush = 80,
    NumberAddPush = 81,
    PushConstant = 82,
    PushFrame = 83,
    CarPush = 84,
    CdrPush = 85,
    ShiftCall = 86,
    NotTest = 87,
    ReferGlobalCall = 88,
    ReferFreePush = 89,
    ReferLocalPush = 90,
    ReferLocalPushConstant = 91,
    ReferLocalPushConstantBranchNotLe = 92,
    ReferLocalPushConstantBranchNotGe = 93,
    ReferLocalPushConstantBranchNotNumberEqual = 94,
    ReferLocalBranchNotNull = 95,
    ReferLocalBranchNotLt = 96,
    ReferFreeCall = 97,
    ReferGlobalPush = 98,
    ReferLocalCall = 99,
    LocalCall = 100,
    Vector = 101,
    SimpleStructRef = 102,
    DynamicWinders = 103,
    TailCall = 104,
    LocalTailCall = 105,
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
