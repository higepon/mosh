use rmosh::{
    self,
    gc::Gc,
    objects::{Closure, Object, Pair, Procedure, SString, Symbol, Vector},
    op::Op,
    vm::Vm,
};

// Custom hand written tests.
#[test]
fn test_symbol_intern() {
    let mut gc = Gc::new();
    let symbol = gc.intern("foo");
    let symbol2 = gc.intern("foo");
    assert_eq!(symbol.pointer, symbol2.pointer);
}

#[test]
fn test_vm_define() {
    let mut vm = Vm::new();
    let ops = [
        Op::Constant(Object::Number(9)),
        Op::DefineGlobal(vm.gc.intern("a")),
        Op::ReferGlobal(vm.gc.intern("a")),
        Op::Halt,
    ];
    let before_size = vm.gc.bytes_allocated();
    let ret = vm.run(ops.as_ptr(), ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
    match ret {
        Object::Number(a) => {
            assert_eq!(a, 9);
        }
        _ => panic!("{:?}", "todo"),
    }
}

pub static SIZE_OF_CLOSURE: usize = std::mem::size_of::<Closure>();
pub static SIZE_OF_PAIR: usize = std::mem::size_of::<Pair>();
pub static SIZE_OF_PROCEDURE: usize = std::mem::size_of::<Procedure>();
pub static SIZE_OF_STRING: usize = std::mem::size_of::<SString>();
pub static SIZE_OF_SYMBOL: usize = std::mem::size_of::<Symbol>();
pub static SIZE_OF_VECTOR: usize = std::mem::size_of::<Vector>();

/*
   Base display closure
   free variables
   baselib name and closure of map1
*/
static SIZE_OF_MIN_VM: usize =
    SIZE_OF_CLOSURE + (SIZE_OF_PROCEDURE * 623) + SIZE_OF_CLOSURE + SIZE_OF_SYMBOL;

fn test_ops_with_size(vm: &mut Vm, ops: Vec<Op>, expected: Object, expected_heap_diff: usize) {
    let ret = vm.run(ops.as_ptr(), ops.len());
    // Remove reference to ret.
    vm.ac = Object::Unspecified;
    vm.mark_and_sweep();
    assert_eq!(vm.gc.bytes_allocated(), SIZE_OF_MIN_VM + expected_heap_diff);
    assert_eq!(ret, expected);
}

fn test_ops_with_size_as_str(vm: &mut Vm, ops: Vec<Op>, expected: &str, expected_heap_diff: usize) {
    let ret = vm.run(ops.as_ptr(), ops.len());
    vm.ac = Object::Unspecified;
    vm.mark_and_sweep();
    assert_eq!(ret.to_string(), expected);
    assert_eq!(vm.gc.bytes_allocated(), SIZE_OF_MIN_VM + expected_heap_diff);
}

#[test]
fn test_vm_alloc_many_pairs() {
    let mut vm = Vm::new();
    let mut ops = vec![];

    for _ in 0..100 {
        ops.push(Op::Constant(Object::Number(99)));
        ops.push(Op::Push);
        ops.push(Op::Constant(Object::Number(101)));
        ops.push(Op::Cons);
    }
    ops.push(Op::Halt);
    let before_size = vm.gc.bytes_allocated();
    vm.run(&ops[..][0] as *const Op, ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM + SIZE_OF_PAIR);
}

// All ops in the following tests are generated in data/.

#[test]
fn test_call0() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_call1() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(10),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_call2() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(12),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_if0() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Test(3),
        Op::Constant(Object::Number(2)),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}
#[test]
fn test_if1() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(2)),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_let0() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(0), 0);
}

#[test]
fn test_let1() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_nested_let0() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_nested_let1() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(5),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(4),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
}

#[test]
fn test_and() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::True), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_if2() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::False),
        Op::LocalJmp(2),
        Op::Constant(Object::True),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test0() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::True), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test2() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::True), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test3() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test4() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test5() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::False),
        Op::LocalJmp(2),
        Op::Constant(Object::True),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test6() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test7() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(6)),
        Op::Push,
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Test(3),
        Op::Constant(Object::Number(7)),
        Op::Return(1),
        Op::Constant(Object::Number(5)),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test8() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test9() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(101)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

#[test]
fn test_test10() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        Op::Frame(7),
        Op::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(102)),
        Op::Return(0),
        Op::Return(0),
        Op::Call(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

#[test]
fn test_test11() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(101)),
        Op::Push,
        Op::Frame(7),
        Op::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(102)),
        Op::Return(1),
        Op::Return(0),
        Op::Call(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

#[test]
fn test_test12() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(103)),
        Op::Push,
        Op::Frame(7),
        Op::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Return(0),
        Op::Call(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(103), 0);
}

#[test]
fn test_test13() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Frame(11),
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Return(1),
        Op::Call(1),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

#[test]
fn test_test14() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Box(0),
        Op::Constant(Object::Number(12)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test15() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(14),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Box(0),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 4,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Constant(Object::Number(101)),
        Op::AssignFree(0),
        Op::Return(0),
        Op::TailCall(0, 1),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

#[test]
fn test_test16() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(24),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Push,
        Op::Closure {
            size: 18,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferLocal(0),
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test17() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::Constant(Object::Number(5)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test18() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test19() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
}

#[test]
fn test_test20() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
}

#[test]
fn test_test21() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test22() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberAdd,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test23() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::NumberAdd,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(17), 0);
}

#[test]
fn test_test24() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test25() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::Constant(Object::Number(-5)),
        Op::NumberAdd,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test26() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::Constant(Object::Number(-5)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(-2)),
        Op::NumberAdd,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test27_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::Constant(Object::Symbol(vm.gc.intern("b"))),
        Op::Cons,
        Op::Halt,
    ];
    let a = Object::Symbol(vm.gc.intern("a"));
    let b = Object::Symbol(vm.gc.intern("b"));
    let pair = vm.gc.alloc(Pair::new(a, b));

    let before_size = vm.gc.bytes_allocated();
    let ret = vm.run(&ops[0], ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
    match ret {
        Object::Pair(pair2) => {
            assert_eq!(pair.car, pair2.car);
            assert_eq!(pair.cdr, pair2.cdr);
        }
        _ => {
            panic!("not a pair");
        }
    }
}

#[test]
fn test_test28() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Car,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test29() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Cdr,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test30() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cadr,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test31() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberEqual,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test32() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberEqual,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

#[test]
fn test_test33() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test34() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(1),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test35() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test36() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::ReferLocal(1),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test37() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test38() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test39() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test40() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test41() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(4),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test42() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(2),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test43() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(6),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(2),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(13), 0);
}

#[test]
fn test_test44() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test45() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test46() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferFree(0),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test47() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test48() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test49() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test50() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(3),
        Op::ReferLocal(2),
        Op::Leave(3),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test51() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(5),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(4),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Push,
        Op::Display(3),
        Op::ReferFree(0),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferFree(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test52() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test53() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test54() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test55() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::AssignFree(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Frame(4),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(0),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test56() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(2),
        Op::LetFrame(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test57() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(2),
        Op::Frame(7),
        Op::ReferLocal(1),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Call(0),
        Op::Leave(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test58() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Box(0),
        Op::Enter(2),
        Op::LetFrame(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Constant(Object::Number(3)),
        Op::AssignFree(0),
        Op::ReferFree(0),
        Op::Indirect,
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test59() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Box(0),
        Op::Enter(2),
        Op::LetFrame(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(1),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Constant(Object::Number(3)),
        Op::AssignFree(0),
        Op::ReferFree(0),
        Op::Indirect,
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test60() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(100)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::LetFrame(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Push,
        Op::Enter(1),
        Op::Frame(3),
        Op::ReferLocal(0),
        Op::Call(0),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(100), 0);
}

// (letrec ((a 1) (b (lambda () a))) (b)) => 1
#[test]
fn test_test61() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(0),
        Op::Undef,
        Op::Push,
        Op::Undef,
        Op::Push,
        Op::Box(1),
        Op::Box(0),
        Op::Enter(2),
        Op::Constant(Object::Number(1)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 4,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Indirect,
        Op::Return(0),
        Op::AssignLocal(1),
        Op::Frame(4),
        Op::ReferLocal(1),
        Op::Indirect,
        Op::Call(0),
        Op::Leave(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0)) => 10
#[test]
fn test_test62() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// (let ((a '())) (let ((G68 (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (set! a G68) (a 0))) => 1000
#[test]
fn test_test63() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1000)),
        Op::BranchNotGe(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::AssignFree(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// (letrec ((a (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (a 0)) => 1000
#[test]
fn test_test64() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1000)),
        Op::BranchNotGe(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// ((lambda (a) (set! a 1000) a) '()) => 1000
#[test]
fn test_test65() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Box(0),
        Op::Constant(Object::Number(1000)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// ((lambda (a) (set! a (lambda (i) (if (= i 20) i (a (+ i 1))))) (a 0)) '()) => 20
#[test]
fn test_test66() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(31),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Closure {
            size: 27,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Box(0),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(20)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(20), 0);
}

// (define a 3) => 3
#[test]
fn test_test68() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::DefineGlobal(vm.gc.intern("a")),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), SIZE_OF_SYMBOL);
}
// (= 3 4) => #f
#[test]
fn test_test70() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberEqual,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (= 3 3 3) => #t
#[test]
fn test_test71() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotNumberEqual(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (= 3 4 5) => #f
#[test]
fn test_test72() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::BranchNotNumberEqual(5),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (((lambda (a) (lambda () a)) 101)) => 101
#[test]
fn test_test73() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Frame(11),
        Op::Constant(Object::Number(101)),
        Op::Push,
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Return(0),
        Op::Return(1),
        Op::Call(1),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

// (((lambda (a) (lambda (b) (+ a b))) 101) 1) => 102
#[test]
fn test_test74() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(18),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Frame(14),
        Op::Constant(Object::Number(101)),
        Op::Push,
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Return(1),
        Op::Return(1),
        Op::Call(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

// (null? '()) => #t
#[test]
fn test_test75() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Nil), Op::NullP, Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (null? 3) => #f
#[test]
fn test_test76() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(3)), Op::NullP, Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (cons 1 2) => (1 . 2)
#[test]
fn test_test77_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::Halt,
    ];
    let pair = vm.gc.alloc(Pair::new(Object::Number(1), Object::Number(2)));
    let before_size = vm.gc.bytes_allocated();
    let ret = vm.run(ops.as_ptr(), ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
    match ret {
        Object::Pair(pair2) => {
            assert_eq!(pair.car, pair2.car);
            assert_eq!(pair.cdr, pair2.cdr);
        }
        _ => {
            panic!("not a pair");
        }
    }
}

// (cons 1 (cons 2 '())) => (1 2)
#[test]
fn test_test78() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    // (2 . nil)
    let pair1 = Object::Pair(vm.gc.alloc(Pair::new(Object::Number(2), Object::Nil)));
    // (1 2)
    let pair2 = vm.gc.alloc(Pair::new(Object::Number(1), pair1));
    let before_size = vm.gc.bytes_allocated();
    let ret = vm.run(&ops[0], ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM);
    match ret {
        Object::Pair(pair3) => {
            assert_eq!(pair2.car, pair3.car);
        }
        _ => {
            panic!("not a pair");
        }
    }
}

// (begin 1 2 3) => 3
#[test]
fn test_test79() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (set! a 4) a)) => 4
#[test]
fn test_test80() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(4)),
        Op::AssignGlobal(vm.gc.intern("a")),
        Op::ReferGlobal(vm.gc.intern("a")),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), SIZE_OF_SYMBOL);
}

// ((lambda () ((lambda () 3)))) => 3
#[test]
fn test_test81() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(8),
        Op::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::TailCall(0, 0),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () ((lambda (x) x) 3))) => 3
#[test]
fn test_test82() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(10),
        Op::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::TailCall(1, 0),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda (y) ((lambda (x) x) 3)) 4) => 3
#[test]
fn test_test83() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(12),
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 8,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (let1 a 1 ((lambda () 3))))) => 3
#[test]
fn test_test84() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Closure {
            size: 11,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::TailCall(0, 3),
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))) => 3
#[test]
fn test_test85() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(18),
        Op::Closure {
            size: 16,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(2),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::TailCall(0, 6),
        Op::Leave(1),
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (if 3 ((lambda () 3))))) => 3
#[test]
fn test_test86() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(12),
        Op::Closure {
            size: 10,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Test(6),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::TailCall(0, 0),
        Op::Return(0),
        Op::Undef,
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (if ((lambda () 3)) 4 5))) => 4
#[test]
fn test_test87() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Closure {
            size: 11,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::Call(0),
        Op::Test(3),
        Op::Constant(Object::Number(4)),
        Op::Return(0),
        Op::Constant(Object::Number(5)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
#[test]
fn test_test88() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 41,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::LetFrame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(4),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Push,
        Op::Display(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 7),
        Op::Leave(1),
        Op::Leave(1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
#[test]
fn test_test89() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 41,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::LetFrame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(4),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Push,
        Op::Display(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Enter(1),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 7),
        Op::Leave(1),
        Op::Leave(1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// ((lambda () (define d (lambda (x y z) (+ x y z))) (d 1 2 3))) => 6
#[test]
fn test_test90() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(29),
        Op::Closure {
            size: 27,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(3),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::AssignLocal(0),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::TailCall(3, 3),
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
}

// ((lambda () (define b (lambda () 3)) (b))) => 3
#[test]
fn test_test91() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(17),
        Op::Closure {
            size: 15,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(0),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::TailCall(0, 3),
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda a a) 1 2 3) => (1 2 3)
#[test]
fn test_test92() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// ((lambda (a . b) b) 1 2 3) => (2 3)
#[test]
fn test_test93() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
}

// ((lambda (a . b) b) 1 2 3 4) => (2 3 4)
#[test]
fn test_test94() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3 4)", 0);
}

// ((lambda (a b . c) c) 1 2 3 4) => (3 4)
#[test]
fn test_test95() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 3,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(2),
        Op::Return(3),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 4)", 0);
}

// ((lambda (a b c . d) d) 1 2 3 4) => (4)
#[test]
fn test_test96() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 4,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(3),
        Op::Return(4),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(4)", 0);
}

// ((lambda (a b c . d) d) 1 2 3) => ()
#[test]
fn test_test97() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 4,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(3),
        Op::Return(4),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Nil, 0);
}

// ((lambda a a)) => ()
#[test]
fn test_test98() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Nil, 0);
}

// ((lambda a a) 1) => (1)
#[test]
fn test_test99() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", 0);
}

// (when #t 1 2 34) => 34
#[test]
fn test_test100() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::True),
        Op::Test(5),
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(34)),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(34), 0);
}

// (not 3) => #f
#[test]
fn test_test101() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(3)), Op::Not, Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (unless #f 1 2 48) => 48
#[test]
fn test_test102() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Undef,
        Op::LocalJmp(4),
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(48)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(48), 0);
}

// (and 3 4 5) => 5
#[test]
fn test_test103() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Test(4),
        Op::Constant(Object::Number(4)),
        Op::Test(2),
        Op::Constant(Object::Number(5)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

// (let1 a 0 (and (set! a (+ a 1))) a) => 1
#[test]
fn test_test104() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (let1 a 0 (or (set! a (+ a 1))) a) => 1
#[test]
fn test_test105() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (and 3 #f 5) => #f
#[test]
fn test_test106() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Test(4),
        Op::Constant(Object::False),
        Op::Test(2),
        Op::Constant(Object::Number(5)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (or 3 4 5) => 3
#[test]
fn test_test107() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Test(2),
        Op::LocalJmp(5),
        Op::Constant(Object::Number(4)),
        Op::Test(2),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(5)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (or #f #f #f) => #f
#[test]
fn test_test108() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(2),
        Op::LocalJmp(3),
        Op::Constant(Object::False),
        Op::Test(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (> 4 3) => #t
#[test]
fn test_test109() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberGt,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (> 4 3 2) => #t
#[test]
fn test_test110() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (> 4 3 1 2) => #f
#[test]
fn test_test111() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(9),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::BranchNotGt(5),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (>= 3 3 3) => #t
#[test]
fn test_test112() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotGe(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (>= 4 3 3) => #t
#[test]
fn test_test113() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotGe(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (>= 4 3) => #t
#[test]
fn test_test114() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 2) => #t
#[test]
fn test_test115() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberLt,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 2 3) => #t
#[test]
fn test_test116() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotLt(5),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 5 3) => #f
#[test]
fn test_test117() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::BranchNotLt(5),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (<= 1 2) => #t
#[test]
fn test_test118() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberLe,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 2 3) => #t
#[test]
fn test_test119() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotLe(5),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 3 3) => #t
#[test]
fn test_test120() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotLe(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 5 3) => #f
#[test]
fn test_test121() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::BranchNotLe(5),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (eq? #t #t) => #t
#[test]
fn test_test122() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::True),
        Op::Push,
        Op::Constant(Object::True),
        Op::Eq,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (eq? #t #f) => #f
#[test]
fn test_test123() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::True),
        Op::Push,
        Op::Constant(Object::False),
        Op::Eq,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (eq? 'a 'a) => #t
#[test]
fn test_test124() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Eq,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
}

// (eq? 'a 'b) => #f
#[test]
fn test_test125() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::Constant(Object::Symbol(vm.gc.intern("b"))),
        Op::Eq,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL * 2);
}

// (pair? (cons 1 2)) => #t
#[test]
fn test_test126() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::PairP,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (pair? 3) => #f
#[test]
fn test_test127() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(3)), Op::PairP, Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (symbol? 'a) => #t
#[test]
fn test_test128() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::SymbolP,
        Op::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
}

// (symbol? 3) => #f
#[test]
fn test_test129() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(3)), Op::SymbolP, Op::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (cond (#f 1) (#t 3)) => 3
#[test]
fn test_test130() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(1)),
        Op::LocalJmp(6),
        Op::Constant(Object::True),
        Op::Test(3),
        Op::Constant(Object::Number(3)),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond (#f 1) (#f 2) (else 3)) => 3
#[test]
fn test_test131() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(1)),
        Op::LocalJmp(6),
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(2)),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond (#t 3) (#f 2) (else 1)) => 3
#[test]
fn test_test132() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::True),
        Op::Test(3),
        Op::Constant(Object::Number(3)),
        Op::LocalJmp(6),
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(2)),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(1)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond ((cons 1 2) => car) (#f 2) (else 3)) => 1
#[test]
fn test_test133() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFree(3),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Test(7),
        Op::Frame(11),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::LocalJmp(6),
        Op::Constant(Object::False),
        Op::Test(3),
        Op::Constant(Object::Number(2)),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (let ((a 0)) `(,a 4 5)) => (0 4 5)
#[test]
fn test_test134() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 4 5)", 0);
}

// (let ((a '(1 2 3))) `(,a 4 5)) => ((1 2 3) 4 5)
#[test]
fn test_test135() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4 5)", 0);
}

// (let ((a '(1 2 3))) `(,@a 4 5)) => (1 2 3 4 5)
#[test]
fn test_test136() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        Op::Append2,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4 5)", 0);
}

// (let ((name 'a)) `(list ,name ',name)) => (list a 'a)
#[test]
fn test_test137() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(6),
        Op::Constant(Object::Symbol(vm.gc.intern("a"))),
        Op::Push,
        Op::Enter(1),
        Op::Constant(Object::Symbol(vm.gc.intern("list"))),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Symbol(vm.gc.intern("quote"))),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list a 'a)", SIZE_OF_SYMBOL * 3);
}

// `(list ,(+ 1 2) 4) => (list 3 4)
#[test]
fn test_test138() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Symbol(vm.gc.intern("list"))),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list 3 4)", SIZE_OF_SYMBOL);
}

// (let ((a '(1 2 3))) `(1 . ,a)) => (1 1 2 3)
#[test]
fn test_test139() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 1 2 3)", 0);
}

// (let ((a '(1 2 3))) `,a) => (1 2 3)
#[test]
fn test_test140() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(,@a)) => (1 2 3)
#[test]
fn test_test141() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(0 ,@a)) => (0 1 2 3)
#[test]
fn test_test142() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(0 ,a 4)) => (0 (1 2 3) 4)
#[test]
fn test_test143() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Cons,
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 (1 2 3) 4)", 0);
}

// (let ((a '(1 2 3))) `(,@a 4)) => (1 2 3 4)
#[test]
fn test_test144() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Append2,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
}

// (let ((a '(1 2 3))) `((,@a) 4)) => ((1 2 3) 4)
#[test]
fn test_test145() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4)", 0);
}

// (let ((a '(1 2 3))) `((,a) 4)) => (((1 2 3)) 4)
#[test]
fn test_test146() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(((1 2 3)) 4)", 0);
}

// `b => b
#[test]
fn test_test147_modified() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Symbol(vm.gc.intern("b"))), Op::Halt];
    let obj = vm.gc.symbol_intern("b");
    test_ops_with_size(&mut vm, ops, obj, SIZE_OF_SYMBOL);
}

// (list 1 2 3) => (1 2 3)
#[test]
fn test_test148() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (aif (+ 1 2) it #f) => 3
#[test]
fn test_test149() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberAdd,
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Test(2),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string-length abc) => 3
#[test]
fn test_test150() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("abc")),
        Op::Push,
        Op::ReferFree(19),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string-length あいう) => 3
#[test]
fn test_test151() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("あいう")),
        Op::Push,
        Op::ReferFree(19),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string->symbol abc) => abc
#[test]
fn test_test152_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("abc")),
        Op::Push,
        Op::ReferFree(20),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let s = vm.gc.symbol_intern("abc");
    test_ops_with_size(&mut vm, ops, s, SIZE_OF_SYMBOL);
}

// (number->string 123) => 123
#[test]
fn test_test153() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::ReferFree(25),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123\"", 0);
}

// (begin (define (proc1 . a) a) (proc1 1 2 3 4)) => (1 2 3 4)
#[test]
fn test_test154_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::DefineGlobal(vm.gc.intern("proc1")),
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("proc1")),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// ((lambda (a . b) b) 1 2 3) => (2 3)
#[test]
fn test_test155() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
}

// ((lambda (a . b) a) 1 2 3 4 5) => 1
#[test]
fn test_test156() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(15),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(2),
        Op::Call(5),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a . b) b) 1 2 3 4 5) => (2 3 4 5)
#[test]
fn test_test157() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(15),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Call(5),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3 4 5)", 0);
}

// ((lambda (a b c d . e) e) 1 2 3 4) => ()
#[test]
fn test_test158() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(4),
        Op::Return(5),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Nil;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) a) 1 2 3 4) => 1
#[test]
fn test_test159() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(5),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) b) 1 2 3 4) => 2
#[test]
fn test_test160() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(5),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) c) 1 2 3 4) => 3
#[test]
fn test_test161() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(2),
        Op::Return(5),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (append '(1 2) '(3 4)) => (1 2 3 4)
#[test]
fn test_test163() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(3), Object::Number(4))),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
}

// (append) => ()
#[test]
fn test_test164() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Nil), Op::Halt];
    let expected = Object::Nil;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (define x 3) x) => 3
#[test]
fn test_test165_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::DefineGlobal(vm.gc.intern("x")),
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (begin (define (hoge . a) a) (hoge 1 2 3)) => (1 2 3)
#[test]
fn test_test166() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::DefineGlobal(vm.gc.intern("hoge")),
        Op::Frame(9),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("hoge")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// (begin (define (hige a . b) b) (hige 1 2 3)) => (2 3)
#[test]
fn test_test167() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::DefineGlobal(vm.gc.intern("hige")),
        Op::Frame(9),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("hige")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// (apply (lambda a a) '(3 2)) => (3 2)
#[test]
fn test_test168() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(3), Object::Number(2))),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 2)", 0);
}

// (let ((a 3)) 3 2 1) => 1
#[test]
fn test_test170() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(1),
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(1)),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (make-string 3) =>
#[test]
fn test_test171() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(17),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"   \"", 0);
}

// (make-string 3 #\c) => "ccc"
#[test]
fn test_test172() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::ReferFree(17),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"ccc\"", 0);
}

// (apply car '((3))) => 3
#[test]
fn test_test173_modified() {
    let mut vm = Vm::new();
    let list = vm.gc.list1(Object::Number(3));
    let ops = vec![
        Op::Frame(7),
        Op::ReferFree(3),
        Op::Push,
        Op::Constant(vm.gc.list1(list)),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a) a) '(3)) => 3
#[test]
fn test_test174() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a b) (+ a b)) '(5 2)) => 7
#[test]
fn test_test175() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(12),
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(5), Object::Number(2))),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a b c) (+ a b c)) '(5 2 1)) => 8
#[test]
fn test_test176() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(15),
        Op::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(5), Object::Number(2), Object::Number(1)),
        ),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(8);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a) (car a)) '((3))) => 3
#[test]
fn test_test177_modified() {
    let mut vm = Vm::new();
    let list = vm.gc.list1(Object::Number(3));
    let ops = vec![
        Op::Frame(10),
        Op::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Car,
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.list1(list)),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a . b) (+ a (car b))) '(1 2)) => 3
#[test]
fn test_test178() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(13),
        Op::Closure {
            size: 7,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::Car,
        Op::NumberAdd,
        Op::Return(2),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (string-append "12" "345" "6") => "123456"
#[test]
fn test_test179() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        Op::Constant(vm.gc.new_string("12")),
        Op::Push,
        Op::Constant(vm.gc.new_string("345")),
        Op::Push,
        Op::Constant(vm.gc.new_string("6")),
        Op::Push,
        Op::ReferFree(22),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123456\"", 0);
}

// (string? "hige") => #t
#[test]
fn test_test181() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("hige")),
        Op::Push,
        Op::ReferFree(31),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p)) => (1 . 3)
#[test]
fn test_test184() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(22),
        Op::Closure {
            size: 20,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(2),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::SetCdr,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 3)", 0);
}

// ((lambda () (define q (cons 1 2)) (set-car! q 3) q)) => (3 . 2)
#[test]
fn test_test185() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(22),
        Op::Closure {
            size: 20,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(2),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::SetCar,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 . 2)", 0);
}

// (begin #f #t) => #t
#[test]
fn test_test186() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::True),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (vector-length (make-vector 3)) => 3
#[test]
fn test_test187() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::VectorLength,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let loop ((i 0)) (if (= i 100) (+ i 1) (loop (+ i 1)))) => 101
#[test]
fn test_test188() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 19,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(100)),
        Op::BranchNotNumberEqual(6),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(101);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((a 0)) (cond (#t (set! a (+ a 1)) (set! a (+ a 1)) a))) => 2
#[test]
fn test_test189() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::True),
        Op::Test(16),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::LocalJmp(2),
        Op::Undef,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (char? #\あ) => #t
#[test]
fn test_test190() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Char('あ')),
        Op::Push,
        Op::ReferFree(53),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eq? (list 'a) (list 'a)) => #f
#[test]
fn test_test191() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(1),
        Op::Eq,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (let ((x (list 'a))) (eq? x x)) => #t
#[test]
fn test_test192() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFree(89),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (map1 (lambda (x) 2) '(1)) => (2)
#[test]
fn test_test193_modified0() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(9),
        // size was originally 3
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(2)),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("map1")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];

    test_ops_with_size_as_str(&mut vm, ops, "(2)", 0);
}

// (map1 (lambda (s) (string-append s "123")) '("ABC" "DEF")) => ("ABC123" "DEF123")
#[test]
fn test_test193_modified() {
    let mut vm = Vm::new();
    let abc = vm.gc.new_string("ABC");
    let def = vm.gc.new_string("DEF");
    let ops = vec![
        Op::Frame(16),
        Op::ReferFree(22),
        Op::Push,
        Op::Closure {
            size: 8,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("123")),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(2, 1),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.list2(abc, def)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("map1")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"ABC123\" \"DEF123\")", 0);
}

// (let1 a '() (let1 G68 (lambda (i) (if (>= i 10000) i (a (+ i 1)))) (set! a G68) (a 0))) => 10000
#[test]
fn test_test194() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(10000)),
        Op::BranchNotGe(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::AssignFree(0),
        Op::Frame(6),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferFree(0),
        Op::Indirect,
        Op::Call(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(10000);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((p (open-string-input-port "12345"))) (read-char p) (read-char p)) => #\2
#[test]
fn test_test195() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(35),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("12345")),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('2');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eof-object? (let ((p (open-string-input-port "1"))) (read-char p) (read-char p))) => #t
#[test]
fn test_test196() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(20),
        Op::LetFrame(2),
        Op::ReferFree(35),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("1")),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Push,
        Op::ReferFree(27),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a)))) => a
#[test]
fn test_test197_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(4),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::Enter(1),
        Op::Constant(vm.gc.symbol_intern("b")),
        Op::Push,
        Op::ReferLocal(0),
        Op::BranchNotEqv(3),
        Op::Constant(vm.gc.symbol_intern("b")),
        Op::LocalJmp(8),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::ReferLocal(0),
        Op::BranchNotEqv(3),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = vm.gc.symbol_intern("a");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (begin (let ((xxy 'a)) (case xxy ((b) 'b) ((c) 'c) (else 3)))) => 3
#[test]
fn test_test198_mofidified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(4),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::Enter(1),
        Op::Constant(vm.gc.symbol_intern("b")),
        Op::Push,
        Op::ReferLocal(0),
        Op::BranchNotEqv(3),
        Op::Constant(vm.gc.symbol_intern("b")),
        Op::LocalJmp(8),
        Op::Constant(vm.gc.symbol_intern("c")),
        Op::Push,
        Op::ReferLocal(0),
        Op::BranchNotEqv(3),
        Op::Constant(vm.gc.symbol_intern("c")),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (* 2 3 4) => 24
#[test]
fn test_test200() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberMul,
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberMul,
        Op::Halt,
    ];
    let expected = Object::Number(24);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (string->number "123") => 123
#[test]
fn test_test201() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("123")),
        Op::Push,
        Op::ReferFree(21),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(123);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((p (open-string-input-port "123 456"))) (read-char p)) => #\1
#[test]
fn test_test202() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(35),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("123 456")),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('1');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (reverse '(1 2 3 4)) => (4 3 2 1)
#[test]
fn test_test203_modified() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.list4(
            Object::Number(1),
            Object::Number(2),
            Object::Number(3),
            Object::Number(4),
        )),
        Op::Push,
        Op::ReferFree(26),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(4 3 2 1)", 0);
}

// (string-split "wiki&cmd" #\&) => ("wiki" "cmd")
#[test]
fn test_test204() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.new_string("wiki&cmd")),
        Op::Push,
        Op::Constant(Object::Char('&')),
        Op::Push,
        Op::ReferFree(23),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"wiki\" \"cmd\")", 0);
}

// (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1) => "cbc"
#[test]
fn test_test205() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::ReferFree(17),
        Op::Call(2),
        Op::DefineGlobal(vm.gc.intern("str1")),
        Op::Frame(9),
        Op::ReferGlobal(vm.gc.intern("str1")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(18),
        Op::Call(3),
        Op::ReferGlobal(vm.gc.intern("str1")),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    // The string is kept as global variable.
    test_ops_with_size_as_str(&mut vm, ops, "\"cbc\"", SIZE_OF_SYMBOL + SIZE_OF_STRING);
}

// (let* ((a 0) (b (lambda (x y) a))) (b (begin (set! a 1)) (begin (set! a 2)))) => 2
#[test]
fn test_test206() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(4),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::Closure {
            size: 4,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferFree(0),
        Op::Indirect,
        Op::Return(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::Constant(Object::Number(1)),
        Op::AssignFree(0),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::AssignFree(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// #\a => #\a
#[test]
fn test_test207() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Char('a')), Op::Halt];
    let expected = Object::Char('a');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eof-object? 3) => #f
#[test]
fn test_test208() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(27),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// 102 => 102
#[test]
fn test_test209() {
    let mut vm = Vm::new();
    let ops = vec![Op::Constant(Object::Number(102)), Op::Halt];
    let expected = Object::Number(102);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// `(list ,(+ 1 2) 4) => (list 3 4)
#[test]
fn test_test210() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(vm.gc.symbol_intern("list")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list 3 4)", SIZE_OF_SYMBOL);
}

// (let ((name 'a)) `(list ,name ',name)) => (list a 'a)
#[test]
fn test_test211() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(6),
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Enter(1),
        Op::Constant(vm.gc.symbol_intern("list")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("quote")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cons,
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list a 'a)", SIZE_OF_SYMBOL * 3);
}

// `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) => (a 3 4 5 6 b)
#[test]
fn test_test212_modified() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let ops = vec![
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(7),
        Op::ReferFree(410),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(4), Object::Number(-5), Object::Number(6)),
        ),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("map1")),
        Op::Call(2),
        Op::Push,
        Op::Constant(vm.gc.cons(b, Object::Nil)),
        Op::Append2,
        Op::Cons,
        Op::Cons,
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a 3 4 5 6 b)", SIZE_OF_SYMBOL * 2);
}

// (vector? #(3)) => #t
#[test]
fn test_test213() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(vm.gc.new_vector(&vec![Object::Number(3)])),
        Op::VectorP,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (define (proc-01) 3) (proc-01)) => 3
#[test]
fn test_test214() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::DefineGlobal(vm.gc.intern("proc-01")),
        Op::Frame(3),
        Op::ReferGlobal(vm.gc.intern("proc-01")),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define (add3 a b) (+ a b)) (add3 1 2)) => 3
#[test]
fn test_test215() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::DefineGlobal(vm.gc.intern("add3")),
        Op::Frame(7),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("add3")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define add2 (lambda (a b) (+ a b))) (add2 1 2)) => 3
#[test]
fn test_test216() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::DefineGlobal(vm.gc.intern("add2")),
        Op::Frame(7),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("add2")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1)) => 3
#[test]
fn test_test217() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::DefineGlobal(vm.gc.intern("z")),
        Op::ReferGlobal(vm.gc.intern("z")),
        Op::Push,
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::VectorSet,
        Op::ReferGlobal(vm.gc.intern("z")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::VectorSet,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::Constant(Object::Number(3)),
        Op::NullP,
        Op::ReferGlobal(vm.gc.intern("z")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::VectorSet,
        Op::ReferGlobal(vm.gc.intern("z")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::VectorRef,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_VECTOR);
}

// (begin (define (proc-2) (define (rec) 3) (rec)) (proc-2)) => 3
#[test]
fn test_test218() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 15,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(0),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(3)),
        Op::Return(0),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::TailCall(0, 3),
        Op::Leave(1),
        Op::Return(0),
        Op::DefineGlobal(vm.gc.intern("proc-2")),
        Op::Frame(3),
        Op::ReferGlobal(vm.gc.intern("proc-2")),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define (func2) (define val 4) val) (func2)) => 4
#[test]
fn test_test219() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Closure {
            size: 12,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::LetFrame(0),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(4)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Return(0),
        Op::DefineGlobal(vm.gc.intern("func2")),
        Op::Frame(3),
        Op::ReferGlobal(vm.gc.intern("func2")),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (if (values 1 2 3) #t #f) => #t
#[test]
fn test_test220() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Test(2),
        Op::Constant(Object::True),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 4 5)) (lambda (a b) b)) => 5
#[test]
fn test_test221() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(152),
        Op::Push,
        Op::Display(1),
        Op::Frame(8),
        Op::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::Values(2),
        Op::Return(0),
        Op::Call(0),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(9),
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c))) => 6
#[test]
fn test_test222() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(152),
        Op::Push,
        Op::Display(1),
        Op::Frame(10),
        Op::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Return(0),
        Op::Call(0),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(15),
        Op::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 1 2 3)) list) => (1 2 3)
#[test]
fn test_test223() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(89),
        Op::Push,
        Op::ReferFree(152),
        Op::Push,
        Op::Display(2),
        Op::Frame(10),
        Op::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Return(0),
        Op::Call(0),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(7),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (call-with-values (lambda () 1) (lambda (x) (+ x 1234))) => 1235
#[test]
fn test_test224() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(152),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(1)),
        Op::Return(0),
        Op::Call(0),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(12),
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1234)),
        Op::NumberAdd,
        Op::Return(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1235);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (receive (a b c) (values 1 2 3) (+ a b c)) => 6
#[test]
fn test_test225() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(4),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Receive(3, 0),
        Op::Enter(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Push,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Leave(3),
        Op::Halt,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (receive z (values 'x 'y) z) => (x y)
#[test]
fn test_test226() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(1),
        Op::Constant(vm.gc.symbol_intern("x")),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("y")),
        Op::Values(2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(x y)", SIZE_OF_SYMBOL * 2);
}

// (receive (a . b) (values 'x 'y 'z) b) => (y z)
#[test]
fn test_test227() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(vm.gc.symbol_intern("x")),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("y")),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("z")),
        Op::Values(3),
        Op::Receive(1, 1),
        Op::Enter(2),
        Op::ReferLocal(1),
        Op::Leave(2),
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(y z)", SIZE_OF_SYMBOL * 3);
}

// (receive (a . b) (values 'x 'y 'z) a) => x
#[test]
fn test_test228() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(vm.gc.symbol_intern("x")),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("y")),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("z")),
        Op::Values(3),
        Op::Receive(1, 1),
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = vm.gc.symbol_intern("x");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (receive x (apply values '(1 2 3)) x) => (1 2 3)
#[test]
fn test_test229() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(110),
        Op::Push,
        Op::ReferFree(152),
        Op::Push,
        Op::Display(2),
        Op::Frame(7),
        Op::ReferFree(1),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (call-with-values (lambda () (values 1 2)) cons) => (1 . 2)
#[test]
fn test_test230() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(152),
        Op::Push,
        Op::Display(2),
        Op::Frame(8),
        Op::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Values(2),
        Op::Return(0),
        Op::Call(0),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(7),
        Op::ReferFree(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", 0);
}

// (cons 'a '()) => (a)
#[test]
fn test_test232() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a)", SIZE_OF_SYMBOL);
}

// (cons '(a) '(b c d)) => ((a) b c d)
#[test]
fn test_test233_modified() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");
    let b = vm.gc.symbol_intern("b");
    let c = vm.gc.symbol_intern("c");
    let d = vm.gc.symbol_intern("d");
    let ops = vec![
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Push,
        Op::Constant(vm.gc.list3(b, c, d)),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((a) b c d)", SIZE_OF_SYMBOL * 4);
}

// (cons "a" '(b c)) => ("a" b c)
#[test]
fn test_test234_modified() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let c = vm.gc.symbol_intern("c");
    let ops = vec![
        Op::Constant(vm.gc.new_string("a")),
        Op::Push,
        Op::Constant(vm.gc.list2(b, c)),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"a\" b c)", SIZE_OF_SYMBOL * 2);
}

// (cons 'a 3) => (a . 3)
#[test]
fn test_test235() {
    let mut vm = Vm::new();
    let ops = vec![
        Op::Constant(vm.gc.symbol_intern("a")),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a . 3)", SIZE_OF_SYMBOL);
}

// (cons '(a b) 'c) => ((a b) . c)
#[test]
fn test_test236() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");
    let b = vm.gc.symbol_intern("b");
    let ops = vec![
        Op::Constant(vm.gc.list2(a, b)),
        Op::Push,
        Op::Constant(vm.gc.symbol_intern("c")),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((a b) . c)", SIZE_OF_SYMBOL * 3);
}
