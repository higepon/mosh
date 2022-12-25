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
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(vm.gc.list2(a, b)),
        Op::Push,
        Op::Constant(c),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((a b) . c)", SIZE_OF_SYMBOL * 3);
}

// (car '(a b c)) => a
#[test]
fn test_test237() {
    let mut vm = Vm::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![Op::Constant(vm.gc.list3(a, b, c)), Op::Car, Op::Halt];
    let expected = vm.gc.symbol_intern("a");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (car '((a) b c d)) => (a)
#[test]
fn test_test238() {
    let mut vm = Vm::new();
    let d = vm.gc.symbol_intern("d");
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");
    let list = vm.gc.list1(a);
    let ops = vec![Op::Constant(vm.gc.list4(list, b, c, d)), Op::Car, Op::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "(a)", SIZE_OF_SYMBOL * 4);
}

// (car '(1 . 2)) => 1
#[test]
fn test_test239() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Number(2))),
        Op::Car,
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cdr '((a) b c d)) => (b c d)
#[test]
fn test_test240() {
    let mut vm = Vm::new();
    let d = vm.gc.symbol_intern("d");
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");
    let list = vm.gc.list1(a);
    let ops = vec![Op::Constant(vm.gc.list4(list, b, c, d)), Op::Cdr, Op::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "(b c d)", SIZE_OF_SYMBOL * 4);
}

// (cdr '(1 . 2)) => 2
#[test]
fn test_test241() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Number(2))),
        Op::Cdr,
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (reverse '(a b c)) => (c b a)
#[test]
fn test_test242() {
    let mut vm = Vm::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.list3(a, b, c)),
        Op::Push,
        Op::ReferFree(26),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(c b a)", SIZE_OF_SYMBOL * 3);
}

// (equal? 'a 'a) => #t
#[test]
fn test_test244() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(a),
        Op::Push,
        Op::Constant(a),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (equal? '(a) '(a)) => #t
#[test]
fn test_test245() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Push,
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (equal? "abc" "abc") => #t
#[test]
fn test_test247() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.new_string("abc")),
        Op::Push,
        Op::Constant(vm.gc.new_string("abc")),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? 2 2) => #t
#[test]
fn test_test248() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? (make-vector 5 'a) (make-vector 5 'a)) => #t
#[test]
fn test_test249() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(a),
        Op::MakeVector,
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(a),
        Op::MakeVector,
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? 'a 'a) => #t
#[test]
fn test_test250() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![Op::Constant(a), Op::Push, Op::Constant(a), Op::Eq, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? '(a) '(a)) => #f
#[test]
fn test_test251() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Push,
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? (list 'a) (list 'a)) => #f
#[test]
fn test_test252() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(1),
        Op::Eq,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? "a" "a") => #f
#[test]
fn test_test253() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.new_string("a")),
        Op::Push,
        Op::Constant(vm.gc.new_string("a")),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? "" "") => #f
#[test]
fn test_test254() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.new_string("")),
        Op::Push,
        Op::Constant(vm.gc.new_string("")),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? '() '()) => #t
#[test]
fn test_test255() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? 2 2) => #t
#[test]
fn test_test256() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? #\A #\A) => #t
#[test]
fn test_test257() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Char('A')),
        Op::Push,
        Op::Constant(Object::Char('A')),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? car car) => #t
#[test]
fn test_test258() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(3),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((n (+ 2 3))) (eq? n n)) => #t
#[test]
fn test_test259() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberAdd,
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x '(a))) (eq? x x)) => #t
#[test]
fn test_test260() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(vm.gc.cons(a, Object::Nil)),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (let ((x '#())) (eq? x x)) => #t
#[test]
fn test_test261() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::Constant(vm.gc.new_vector(&vec![])),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((p (lambda (x) x))) (eq? p p)) => #t
#[test]
fn test_test262() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
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
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3 4) => -1
#[test]
fn test_test263() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(-4)),
        Op::NumberAdd,
        Op::Halt,
    ];
    let expected = Object::Number(-1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3 4 5) => -6
#[test]
fn test_test264() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(-4)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(-5)),
        Op::NumberAdd,
        Op::Halt,
    ];
    let expected = Object::Number(-6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3) => -3
#[test]
fn test_test265() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(-3)), Op::Halt];
    let expected = Object::Number(-3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cond ((> 3 2) 'greater) ((< 3 2) 'less)) => greater
#[test]
fn test_test266() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("less");
    let a = vm.gc.symbol_intern("greater");

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotGt(3),
        Op::Constant(a),
        Op::LocalJmp(8),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotLt(3),
        Op::Constant(b),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = vm.gc.symbol_intern("greater");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)) => equal
#[test]
fn test_test267() {
    let mut vm = Vm::new();
    let c = vm.gc.symbol_intern("equal");
    let b = vm.gc.symbol_intern("less");
    let a = vm.gc.symbol_intern("greater");

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(3),
        Op::Constant(a),
        Op::LocalJmp(8),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::BranchNotLt(3),
        Op::Constant(b),
        Op::LocalJmp(2),
        Op::Constant(c),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = vm.gc.symbol_intern("equal");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (cond ('(1 2 3) => cadr) (else #f)) => 2
#[test]
fn test_test268() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFree(70),
        Op::Push,
        Op::Display(1),
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Test(6),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i)) => #(0 1 2 3 4)
#[test]
fn test_test269() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(41),
        Op::Frame(21),
        Op::Frame(10),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Call(2),
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("vec")),
        Op::Call(1),
        Op::Call(1),
        Op::Push,
        Op::Frame(8),
        Op::ReferGlobal(vm.gc.intern("vec")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::Constant(Object::Number(5)),
        Op::NumberEqual,
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("vec")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::VectorSet,
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("do")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "#(0 1 2 3 4)", SIZE_OF_SYMBOL * 0);
}

// (cons 'a 'b) => (a . b)
#[test]
fn test_test27() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(a),
        Op::Push,
        Op::Constant(b),
        Op::Cons,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a . b)", SIZE_OF_SYMBOL * 2);
}

// (let ((x '(1 3 5 7 9))) (do ((x x (cdr x)) (sum 0 (+ sum (car x)))) ((null? x) sum))) => 25
#[test]
fn test_test270() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(10),
        Op::Constant(vm.gc.list5(
            Object::Number(1),
            Object::Number(3),
            Object::Number(5),
            Object::Number(7),
            Object::Number(9),
        )),
        Op::Push,
        Op::Enter(1),
        Op::Frame(33),
        Op::Frame(22),
        Op::Frame(11),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("sum")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Car,
        Op::NumberAdd,
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("sum")),
        Op::Call(2),
        Op::Push,
        Op::Frame(8),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::Cdr,
        Op::Push,
        Op::ReferLocal(0),
        Op::Call(2),
        Op::Call(1),
        Op::Push,
        Op::Frame(6),
        Op::ReferGlobal(vm.gc.intern("sum")),
        Op::Push,
        Op::ReferLocal(0),
        Op::NullP,
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("do")),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(25);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or (= 2 2) (> 2 1)) => #t
#[test]
fn test_test273() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotNumberEqual(2),
        Op::LocalJmp(5),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or (= 2 2) (< 2 1)) => #t
#[test]
fn test_test274() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::BranchNotNumberEqual(2),
        Op::LocalJmp(5),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or #f #f #f) => #f
#[test]
fn test_test275() {
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
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or '(b c) (/ 3 0)) => (b c)
#[test]
fn test_test276() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("c");
    let a = vm.gc.symbol_intern("b");

    let ops = vec![
        Op::Constant(vm.gc.list2(a, b)),
        Op::Test(2),
        Op::LocalJmp(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(0)),
        Op::NumberDiv,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(b c)", SIZE_OF_SYMBOL * 2);
}

// (not #t) => #f
#[test]
fn test_test277() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::True), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not 3) => #f
#[test]
fn test_test278() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not (list 3)) => #f
#[test]
fn test_test279() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(1),
        Op::Not,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not #f) => #t
#[test]
fn test_test280() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::False), Op::Not, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not '()) => #f
#[test]
fn test_test281() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Nil), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not (list)) => #f
#[test]
fn test_test282() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ReferFree(89),
        Op::Call(0),
        Op::Not,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not 'nil) => #f
#[test]
fn test_test283() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("nil");

    let ops = vec![Op::Constant(a), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (let ((x 2) (y 3)) (* x y)) => 6
#[test]
fn test_test284() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberMul,
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) => 35
#[test]
fn test_test285() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(6),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(2),
        Op::LetFrame(4),
        Op::Constant(Object::Number(7)),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Push,
        Op::Enter(2),
        Op::ReferLocal(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberMul,
        Op::Leave(2),
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = Object::Number(35);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) => 70
#[test]
fn test_test286() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(6),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Enter(2),
        Op::LetFrame(4),
        Op::ReferLocal(1),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Display(2),
        Op::Constant(Object::Number(7)),
        Op::Push,
        Op::Enter(1),
        Op::LetFrame(3),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::Display(2),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::NumberAdd,
        Op::Push,
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::NumberMul,
        Op::Leave(1),
        Op::Leave(1),
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = Object::Number(70);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 'a 'a) => #t
#[test]
fn test_test287() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(a),
        Op::Push,
        Op::Constant(a),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eqv? 'a 'b) => #f
#[test]
fn test_test288() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Constant(a),
        Op::Push,
        Op::Constant(b),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (eqv? 2 2) => #t
#[test]
fn test_test289() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? '() '()) => #t
#[test]
fn test_test290() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 100000000 100000000) => #t
#[test]
fn test_test291() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(100000000)),
        Op::Push,
        Op::Constant(Object::Number(100000000)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? (cons 1 2) (cons 1 2)) => #f
#[test]
fn test_test292() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? (lambda () 1) (lambda () 2)) => #f
#[test]
fn test_test293() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(1)),
        Op::Return(0),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(2)),
        Op::Return(0),
        Op::Eqv,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 123456789101112 123456789101112) => #t
#[test]
fn test_test294() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(123456789101112)),
        Op::Push,
        Op::Constant(Object::Number(123456789101112)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? #f 'nil) => #f
#[test]
fn test_test295() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("nil");

    let ops = vec![
        Op::Constant(Object::False),
        Op::Push,
        Op::Constant(a),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (digit->integer #\3 10) => 3
#[test]
fn test_test297() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('3')),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferFree(39),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (+) => 0
#[test]
fn test_test298() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(0)), Op::Halt];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (*) => 1
#[test]
fn test_test299() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (with-exception-handler (lambda (e) "error") (lambda () "no-error")) => "no-error"
#[test]
fn test_test300() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(11),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(vm.gc.new_string("error")),
        Op::Return(1),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(vm.gc.new_string("no-error")),
        Op::Return(0),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("with-exception-handler")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"no-error\"", SIZE_OF_SYMBOL * 0);
}

// (guard (con ((string? con) "error-is-string") (else "error-is-not-string")) (raise "raise")) => "error-is-string"
#[test]
fn test_test301() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(29),
        Op::Frame(19),
        Op::Frame(9),
        Op::Constant(vm.gc.new_string("error-is-string")),
        Op::Push,
        Op::Frame(5),
        Op::ReferGlobal(vm.gc.intern("con")),
        Op::Push,
        Op::ReferFree(31),
        Op::Call(1),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("error-is-not-string")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("else")),
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("con")),
        Op::Call(2),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("raise")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("raise")),
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("guard")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"error-is-string\"", SIZE_OF_SYMBOL * 0);
}

// (guard (con ((string? con) "error-is-string") (else "error-is-not-string")) 3) => 3
#[test]
fn test_test302() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(25),
        Op::Frame(19),
        Op::Frame(9),
        Op::Constant(vm.gc.new_string("error-is-string")),
        Op::Push,
        Op::Frame(5),
        Op::ReferGlobal(vm.gc.intern("con")),
        Op::Push,
        Op::ReferFree(31),
        Op::Call(1),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("error-is-not-string")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("else")),
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("con")),
        Op::Call(2),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("guard")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) 1 2 '(3)) => 6
#[test]
fn test_test303() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(19),
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
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) '(1 2 3)) => 6
#[test]
fn test_test304() {
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
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) 1 '(2 3)) => 6
#[test]
fn test_test305() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(17),
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
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(2), Object::Number(3))),
        Op::Push,
        Op::ReferFree(152),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr)) => (2)
#[test]
fn test_test306() {
    let mut vm = Vm::new();
    let list = vm.gc.list2(Object::Number(3), Object::Number(2));
    let ops = vec![
        Op::Frame(22),
        Op::ReferFree(152),
        Op::Push,
        Op::Closure {
            size: 8,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocal(1),
        Op::Push,
        Op::Constant(vm.gc.list1(list)),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(2, 2),
        Op::Return(2),
        Op::Push,
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(4),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Push,
        Op::ReferFree(152),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2)", SIZE_OF_SYMBOL * 0);
}

// (/ 6 2) => 3
#[test]
fn test_test307() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(6)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (mod 23 10) => 3
#[test]
fn test_test308() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(23)),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("mod")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (even? 2) => #t
#[test]
fn test_test309() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(408),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (even? 3) => #f
#[test]
fn test_test310() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(408),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all even? '(2 4 14)) => #t
#[test]
fn test_test313() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::ReferFree(408),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(4), Object::Number(14)),
        ),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("for-all")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all (lambda (n) (and (even? n) n)) '(2 4 14)) => 14
#[test]
fn test_test314() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(17),
        Op::ReferFree(408),
        Op::Push,
        Op::Closure {
            size: 9,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Test(2),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(4), Object::Number(14)),
        ),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("for-all")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(14);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all (lambda (a b) (< a b)) '(1 2 3) '(2 3 4)) => #t
#[test]
fn test_test315() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(14),
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberLt,
        Op::Return(2),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(3), Object::Number(4)),
        ),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("for-all")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all (lambda (a b) (< a b)) '(1 2 4) '(2 3 4)) => #f
#[test]
fn test_test316() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(14),
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(1),
        Op::NumberLt,
        Op::Return(2),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(4)),
        ),
        Op::Push,
        Op::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(3), Object::Number(4)),
        ),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("for-all")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (+ (/ 2) (/ 4) (/ 4)) => 1
#[test]
fn test_test317() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberAdd,
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- (/ 1 2) (/ 1 4) (/ 1 4)) => 0
#[test]
fn test_test318() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberSub,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberSub,
        Op::Halt,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 3 2) (+ (/ 1 2) 1)) => #t
#[test]
fn test_test319() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 5 2) (+ 1 (/ 1 2) 1)) => #t
#[test]
fn test_test320() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(5)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 3 2) (- 3 (/ 1 2) 1)) => #t
#[test]
fn test_test321() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberSub,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (* (/ 3 2) 2) => 3
#[test]
fn test_test322() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberMul,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (* 2 (/ 3 2)) => 3
#[test]
fn test_test323() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberMul,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (* (/ 4 2) (/ 3 2)) => 3
#[test]
fn test_test324() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberMul,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ (/ 2 2) (/ 1 2)) => 2
#[test]
fn test_test325() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ (/ 4 2) 1) => 2
#[test]
fn test_test326() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> 1 (/ 1 2)) => #t
#[test]
fn test_test329() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> (/ 1 2) 1) => #f
#[test]
fn test_test330() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> 1 (/ 1 2)) => #t
#[test]
fn test_test331() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> (/ 1 2) (/ 1 3)) => #t
#[test]
fn test_test332() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= (/ 1 2) 1) => #t
#[test]
fn test_test333() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (>= 1 (/ 1 2)) => #t
#[test]
fn test_test334() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (>= (/ 1 2) (/ 1 3)) => #t
#[test]
fn test_test335() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberGe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< (/ 1 2) 1) => #t
#[test]
fn test_test336() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< 1 (/ 1 2)) => #f
#[test]
fn test_test337() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< (/ 1 2) (/ 1 3)) => #f
#[test]
fn test_test338() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= (/ 1 2) 1) => #t
#[test]
fn test_test339() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= 1 (/ 1 2)) => #f
#[test]
fn test_test340() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= (/ 1 2) (/ 1 3)) => #f
#[test]
fn test_test341() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 2 2) 1) => #t
#[test]
fn test_test342() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= 1 (/ 2 2)) => #t
#[test]
fn test_test343() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 1 2) (/ 2 4)) => #t
#[test]
fn test_test344() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (>= (/ 1 2) (inexact (/ 1 3))) => #t
#[test]
fn test_test345() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberGe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> (/ 3 2) (+ (inexact (/ 1 3)) (inexact (/ 1 3)) (inexact (/ 1 3))) (/ 99 100)) => #t
#[test]
fn test_test346() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberAdd,
        Op::BranchNotGt(35),
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(99)),
        Op::Push,
        Op::Constant(Object::Number(100)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> 1 (/ (inexact 98) 100) (/ 97 100)) => #t
#[test]
fn test_test347() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Frame(5),
        Op::Constant(Object::Number(98)),
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::Push,
        Op::Constant(Object::Number(100)),
        Op::NumberDiv,
        Op::BranchNotGt(15),
        Op::Frame(5),
        Op::Constant(Object::Number(98)),
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::Push,
        Op::Constant(Object::Number(100)),
        Op::NumberDiv,
        Op::Push,
        Op::Constant(Object::Number(97)),
        Op::Push,
        Op::Constant(Object::Number(100)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (rational? 3) => #t
#[test]
fn test_test348() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(287),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (rational? (/ 1 4)) => #t
#[test]
fn test_test349() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(287),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (rational? (/ (/ 1 2) (+ (greatest-fixnum) 1))) => #t
#[test]
fn test_test350() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(16),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(287),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (flonum? (/ (inexact (/ 1 3)) (+ (greatest-fixnum) 1))) => #t
#[test]
fn test_test351() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(20),
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(288),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ (+ (greatest-fixnum) 1) 1) (+ (greatest-fixnum) 1)) => #t
#[test]
fn test_test352() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberDiv,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (rational? (/ (+ (greatest-fixnum) 1) (/ 1 3))) => #t
#[test]
fn test_test353() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(16),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(287),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (flonum? (/ (+ (greatest-fixnum) 1) (inexact (/ 1 3)))) => #t
#[test]
fn test_test354() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(20),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(300),
        Op::Call(1),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(288),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)) => 1
#[test]
fn test_test355() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1))) => #t
#[test]
fn test_test356() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(18),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ 2) => 1/2
#[test]
fn test_test357() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(1 / 2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ 3) => 1/3
#[test]
fn test_test358() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(1 / 3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (least-fixnum)) => #t
#[test]
fn test_test359() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (greatest-fixnum)) => #t
#[test]
fn test_test360() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (+ (greatest-fixnum) 1)) => #f
#[test]
fn test_test361() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(10),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (- (least-fixnum) 1)) => #f
#[test]
fn test_test362() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(10),
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (number? (+ (greatest-fixnum) 1)) => #t
#[test]
fn test_test363() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(10),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (number? (- (least-fixnum) 1)) => #t
#[test]
fn test_test364() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(10),
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> (+ (greatest-fixnum) 1) (greatest-fixnum)) => #t
#[test]
fn test_test365() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< (- (least-fixnum) 1) (least-fixnum)) => #t
#[test]
fn test_test366() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::Push,
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (- (+ (greatest-fixnum) 1) 1)) => #t
#[test]
fn test_test367() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(13),
        Op::Frame(3),
        Op::ReferFree(293),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (fixnum? (+ (- (least-fixnum) 1) 1)) => #t
#[test]
fn test_test368() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(13),
        Op::Frame(3),
        Op::ReferFree(292),
        Op::Call(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberSub,
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::Push,
        Op::ReferFree(289),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (number? 3) => #t
#[test]
fn test_test369() {
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
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (number? (/ 1 4)) => #t
#[test]
fn test_test370() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(8),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (mod 123 10) => 3
#[test]
fn test_test371() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("mod")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (mod 123 -10) => 3
#[test]
fn test_test372() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::Constant(Object::Number(-10)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("mod")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (mod -123 10) => 7
#[test]
fn test_test373() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(-123)),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("mod")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (mod -123 -10) => 7
#[test]
fn test_test374() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(-123)),
        Op::Push,
        Op::Constant(Object::Number(-10)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("mod")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div 123 10) => 12
#[test]
fn test_test375() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferFree(411),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(12);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div 123 -10) => -12
#[test]
fn test_test376() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::Constant(Object::Number(-10)),
        Op::Push,
        Op::ReferFree(411),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(-12);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div -123 10) => -13
#[test]
fn test_test377() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(-123)),
        Op::Push,
        Op::Constant(Object::Number(10)),
        Op::Push,
        Op::ReferFree(411),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(-13);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div -123 -10) => 13
#[test]
fn test_test378() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Number(-123)),
        Op::Push,
        Op::Constant(Object::Number(-10)),
        Op::Push,
        Op::ReferFree(411),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(13);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (string-ref "abc" 2) => #\c
#[test]
fn test_test379() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.new_string("abc")),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(96),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('c');
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (list? '(a b c)) => #t
#[test]
fn test_test380() {
    let mut vm = Vm::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.list3(a, b, c)),
        Op::Push,
        Op::ReferFree(88),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (list? '()) => #t
#[test]
fn test_test381() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::ReferFree(88),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (list? '(a . b)) => #f
#[test]
fn test_test382() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.cons(a, b)),
        Op::Push,
        Op::ReferFree(88),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// "abc" => "abc"
#[test]
fn test_test383() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(vm.gc.new_string("abc")), Op::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "\"abc\"", SIZE_OF_SYMBOL * 0);
}

// (match 123 ((? string? x) (list 'string x)) ((? number? x) (list 'number x))) => (number 123)
#[test]
fn test_test385() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("number");
    let a = vm.gc.symbol_intern("string");

    let ops = vec![
        Op::Frame(41),
        Op::Constant(Object::Number(123)),
        Op::Push,
        Op::Frame(17),
        Op::Frame(7),
        Op::Constant(a),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(2),
        Op::Push,
        Op::Frame(7),
        Op::ReferFree(31),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("?")),
        Op::Call(2),
        Op::Call(1),
        Op::Push,
        Op::Frame(17),
        Op::Frame(7),
        Op::Constant(b),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Push,
        Op::ReferFree(89),
        Op::Call(2),
        Op::Push,
        Op::Frame(7),
        Op::ReferFree(0),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("?")),
        Op::Call(2),
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("match")),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(number 123)", SIZE_OF_SYMBOL * 2);
}

// (do ((i 0) (j 0)) ((zero? j) (set! i 1) (set! i 2) i)) => 2
#[test]
fn test_test390() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(33),
        Op::Frame(13),
        Op::Frame(5),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("j")),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(Object::Number(0)),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Call(1),
        Op::Call(1),
        Op::Push,
        Op::Frame(15),
        Op::Constant(Object::Number(1)),
        Op::AssignGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::AssignGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::Push,
        Op::Frame(5),
        Op::ReferGlobal(vm.gc.intern("j")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("zero?")),
        Op::Call(1),
        Op::Call(3),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("do")),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)) => composite
#[test]
fn test_test391() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("composite");
    let a = vm.gc.symbol_intern("prime");

    let ops = vec![
        Op::LetFrame(6),
        Op::ReferFree(158),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::NumberMul,
        Op::Push,
        Op::Enter(1),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list4(
            Object::Number(2),
            Object::Number(3),
            Object::Number(5),
            Object::Number(7),
        )),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Test(3),
        Op::Constant(a),
        Op::LocalJmp(12),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list5(
            Object::Number(1),
            Object::Number(4),
            Object::Number(6),
            Object::Number(8),
            Object::Number(9),
        )),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Test(3),
        Op::Constant(b),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = vm.gc.symbol_intern("composite");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (case 1 ((2 1) 0)) => 0
#[test]
fn test_test393() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFree(158),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Enter(1),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(2), Object::Number(1))),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Test(3),
        Op::Constant(Object::Number(0)),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (case 2 ((2 1) 0)) => 0
#[test]
fn test_test394() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFree(158),
        Op::Push,
        Op::Display(1),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Enter(1),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.list2(Object::Number(2), Object::Number(1))),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Test(3),
        Op::Constant(Object::Number(0)),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (procedure? car) => #t
#[test]
fn test_test395() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(159),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (procedure? 'car) => #f
#[test]
fn test_test396() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("car");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(159),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (procedure? (lambda (x) (* x x))) => #t
#[test]
fn test_test397() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(10),
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferLocal(0),
        Op::NumberMul,
        Op::Return(1),
        Op::Push,
        Op::ReferFree(159),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\a) => #t
#[test]
fn test_test399() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('a')),
        Op::Push,
        Op::ReferFree(164),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\c #\b #\a) => #t
#[test]
fn test_test400() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(9),
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('a')),
        Op::Push,
        Op::ReferFree(164),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\b) => #t
#[test]
fn test_test401() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(164),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\c) => #f
#[test]
fn test_test402() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::ReferFree(164),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\a) => #t
#[test]
fn test_test403() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('a')),
        Op::Push,
        Op::ReferFree(165),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\b) => #f
#[test]
fn test_test404() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(165),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\c) => #f
#[test]
fn test_test405() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::ReferFree(165),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\a #\b) => #t
#[test]
fn test_test406() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('a')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(162),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\b #\b) => #t
#[test]
fn test_test407() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(162),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\c #\b) => #f
#[test]
fn test_test408() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(162),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\a #\b) => #t
#[test]
fn test_test409() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('a')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(163),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\b #\b) => #f
#[test]
fn test_test410() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(163),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\c #\b) => #f
#[test]
fn test_test411() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Char('c')),
        Op::Push,
        Op::Constant(Object::Char('b')),
        Op::Push,
        Op::ReferFree(163),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cons* 1 2 3 4) => (1 2 3 . 4)
#[test]
fn test_test412() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(11),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(4),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (cons* 1) => 1
#[test]
fn test_test413() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append 1) => 1
#[test]
fn test_test414() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append '(1) 2) => (1 . 2)
#[test]
fn test_test415() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) 3) => (1 2 . 3)
#[test]
fn test_test416() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 . 3)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3)) => (1 2 3)
#[test]
fn test_test417() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test418() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Append2,
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test419() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Append2,
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append '() 1) => 1
#[test]
fn test_test420() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Append2,
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append '(1) '()) => (1)
#[test]
fn test_test421() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Append2,
        Op::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", SIZE_OF_SYMBOL * 0);
}

// (append! 1) => 1
#[test]
fn test_test422() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append! '(1) 2) => (1 . 2)
#[test]
fn test_test423() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) 3) => (1 2 . 3)
#[test]
fn test_test424() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(Object::Number(3)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 . 3)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3)) => (1 2 3)
#[test]
fn test_test425() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test426() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(9),
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test427() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(9),
        Op::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        Op::Push,
        Op::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Number(4)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append! '() 1) => 1
#[test]
fn test_test428() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(Object::Nil),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append! '(1) '()) => (1)
#[test]
fn test_test429() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        Op::Push,
        Op::Constant(Object::Nil),
        Op::Push,
        Op::ReferFree(176),
        Op::Call(2),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", SIZE_OF_SYMBOL * 0);
}

// (string #\1 #\2 #\3) => "123"
#[test]
fn test_test430() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(9),
        Op::Constant(Object::Char('1')),
        Op::Push,
        Op::Constant(Object::Char('2')),
        Op::Push,
        Op::Constant(Object::Char('3')),
        Op::Push,
        Op::ReferFree(24),
        Op::Call(3),
        Op::Halt,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123\"", SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable (lambda (x) 2) (lambda (a b) #t)) (hashtable-set! ht 1 1) (hashtable-set! ht 2 2) (hashtable-ref ht 1)) => 2
#[test]
fn test_test432() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(3),
        Op::Frame(11),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(2)),
        Op::Return(1),
        Op::Push,
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::True),
        Op::Return(2),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "my" "apple") (hashtable-set! ht "our" "water") (hashtable-ref ht "my")) => "apple"
#[test]
fn test_test433() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(5),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("my")),
        Op::Push,
        Op::Constant(vm.gc.new_string("apple")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("our")),
        Op::Push,
        Op::Constant(vm.gc.new_string("water")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("my")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"apple\"", SIZE_OF_SYMBOL * 0);
}

// (hashtable? (make-hashtable string-hash string=?)) => #t
#[test]
fn test_test434() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(11),
        Op::Frame(7),
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Call(2),
        Op::Push,
        Op::ReferFree(201),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (hashtable? (make-eq-hashtable)) => #t
#[test]
fn test_test435() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(98),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(201),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (hashtable? '(a . b)) => #f
#[test]
fn test_test436() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.cons(a, b)),
        Op::Push,
        Op::ReferFree(201),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "my" "apple") (hashtable-set! ht "our" "water") (hashtable-size ht)) => 2
#[test]
fn test_test437() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(10),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(202),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(5),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("my")),
        Op::Push,
        Op::Constant(vm.gc.new_string("apple")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("our")),
        Op::Push,
        Op::Constant(vm.gc.new_string("water")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht "my" "apple") (hashtable-set! ht "my" "apple") (hashtable-size ht)) => 2
#[test]
fn test_test438() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(8),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(202),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(3),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("my")),
        Op::Push,
        Op::Constant(vm.gc.new_string("apple")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("my")),
        Op::Push,
        Op::Constant(vm.gc.new_string("apple")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-delete! ht 1) (hashtable-ref ht 1 #f)) => #f
#[test]
fn test_test439() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(9),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(203),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(4),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(2),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(Object::False),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(3),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-delete! ht "one") (hashtable-ref ht "one" #f)) => #f
#[test]
fn test_test440() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(203),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(6),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(2),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::False),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(3),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-contains? ht 2)) => #f
#[test]
fn test_test441() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(6),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(204),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(3),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-contains? ht 1)) => #t
#[test]
fn test_test442() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(6),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(204),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(3),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-contains? ht "two")) => #f
#[test]
fn test_test443() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(8),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(204),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(5),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-contains? ht "one")) => #t
#[test]
fn test_test444() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(8),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(204),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(5),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" "one") (hashtable-update! ht "one" (lambda (x) (string-append "!" x "!")) "hige") (hashtable-update! ht "two" (lambda (x) (string-append "!" x "!")) "hige") (string-append (hashtable-ref ht "one") (hashtable-ref ht "two"))) => "!one!!hige!"
#[test]
fn test_test445() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(20),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(22),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(6),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::Frame(22),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(4),
        Op::Push,
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Constant(vm.gc.new_string("!")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("!")),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(3, 1),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.new_string("hige")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("hashtable-update!")),
        Op::Call(4),
        Op::Frame(22),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferFree(4),
        Op::Push,
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Constant(vm.gc.new_string("!")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("!")),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(3, 1),
        Op::Return(1),
        Op::Push,
        Op::Constant(vm.gc.new_string("hige")),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("hashtable-update!")),
        Op::Call(4),
        Op::Frame(19),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"!one!!hige!\"", SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (let1 ht-copy (hashtable-copy ht) (and (string=? (hashtable-ref ht-copy 1) "one") (not (hashtable-mutable? ht-copy))))) => #t
#[test]
fn test_test446() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(205),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(206),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(6),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::LetFrame(7),
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(4),
        Op::Push,
        Op::Display(4),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::Frame(13),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(2),
        Op::Test(7),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Not,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (let1 ht-copy (hashtable-copy ht #t) (and (string=? (hashtable-ref ht-copy 1) "one") (hashtable-mutable? ht-copy)))) => #t
#[test]
fn test_test447() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(12),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(205),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(206),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(6),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::LetFrame(8),
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(4),
        Op::Push,
        Op::Display(4),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::True),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(13),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(2),
        Op::Test(6),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" "one") (let1 ht-copy (hashtable-copy ht) (and (string=? (hashtable-ref ht-copy "one") "one") (not (hashtable-mutable? ht-copy))))) => #t
#[test]
fn test_test448() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(13),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(205),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(206),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(8),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(7),
        Op::Call(3),
        Op::LetFrame(7),
        Op::ReferFree(5),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(6),
        Op::Push,
        Op::Display(4),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::Frame(13),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(2),
        Op::Test(7),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Not,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" "one") (let1 ht-copy (hashtable-copy ht #t) (and (string=? (hashtable-ref ht-copy "one") "one") (hashtable-mutable? ht-copy)))) => #t
#[test]
fn test_test449() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(14),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(205),
        Op::Push,
        Op::ReferFree(101),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(206),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(8),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(7),
        Op::Call(3),
        Op::LetFrame(8),
        Op::ReferFree(5),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(6),
        Op::Push,
        Op::Display(4),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::True),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(13),
        Op::Frame(7),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(2),
        Op::Test(6),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-set! ht 2 "two") (hashtable-clear! ht) (hashtable-size ht)) => 0
#[test]
fn test_test450() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(9),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(207),
        Op::Push,
        Op::ReferFree(202),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(4),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(3),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(1),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-set! ht "two" 2) (hashtable-clear! ht) (hashtable-size ht)) => 0
#[test]
fn test_test451() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(207),
        Op::Push,
        Op::ReferFree(202),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(6),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(5),
        Op::Call(3),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(1),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-set! ht 2 "two") (list-sort < (vector->list (hashtable-keys ht)))) => (1 2)
#[test]
fn test_test452() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(11),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(212),
        Op::Push,
        Op::ReferFree(102),
        Op::Push,
        Op::ReferFree(167),
        Op::Push,
        Op::ReferFree(98),
        Op::Push,
        Op::Display(5),
        Op::Frame(3),
        Op::ReferFree(0),
        Op::Call(0),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(15),
        Op::ReferFree(3),
        Op::Push,
        Op::Frame(9),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(1),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("list-sort")),
        Op::Call(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2)", SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-set! ht "two" 2) (let1 keys (vector->list (hashtable-keys ht)) (and (member "one" keys) (member "two" keys) (= 2 (length keys))))) => #t
#[test]
fn test_test453() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(18),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(102),
        Op::Push,
        Op::ReferFree(167),
        Op::Push,
        Op::ReferFree(93),
        Op::Push,
        Op::ReferFree(191),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(8),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(7),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(7),
        Op::Call(3),
        Op::LetFrame(9),
        Op::ReferFree(4),
        Op::Push,
        Op::ReferFree(3),
        Op::Push,
        Op::ReferFree(6),
        Op::Push,
        Op::ReferFree(5),
        Op::Push,
        Op::Display(4),
        Op::Frame(9),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(1),
        Op::Call(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Enter(1),
        Op::Frame(7),
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Test(17),
        Op::Frame(7),
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(3),
        Op::Call(2),
        Op::Test(9),
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(2),
        Op::Call(1),
        Op::NumberEqual,
        Op::Leave(1),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-hashtable string-hash string=?) (hashtable-set! ht "one" 1) (hashtable-set! ht "two" 2) (receive (keys vals) (hashtable-entries ht) (append (vector->list keys) (vector->list vals)))) => ("two" "one" 2 1)
#[test]
fn test_test454() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(13),
        Op::ReferFree(100),
        Op::Push,
        Op::ReferFree(167),
        Op::Push,
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Push,
        Op::Display(5),
        Op::Frame(7),
        Op::ReferFree(2),
        Op::Push,
        Op::ReferFree(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(2),
        Op::Push,
        Op::Enter(1),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("one")),
        Op::Push,
        Op::Constant(Object::Number(1)),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::Frame(9),
        Op::ReferLocal(0),
        Op::Push,
        Op::Constant(vm.gc.new_string("two")),
        Op::Push,
        Op::Constant(Object::Number(2)),
        Op::Push,
        Op::ReferFree(4),
        Op::Call(3),
        Op::LetFrame(4),
        Op::ReferFree(3),
        Op::Push,
        Op::Display(1),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferGlobal(vm.gc.intern("hashtable-entries")),
        Op::Call(1),
        Op::Receive(2, 0),
        Op::Enter(2),
        Op::Frame(5),
        Op::ReferLocal(0),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::ReferLocal(1),
        Op::Push,
        Op::ReferFree(0),
        Op::Call(1),
        Op::Append2,
        Op::Leave(2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(two one 2 1)", SIZE_OF_SYMBOL * 0);
}

// (equal? eq? (hashtable-equivalence-function (make-eq-hashtable))) => #t
#[test]
fn test_test455() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ReferFree(91),
        Op::Push,
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(98),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(209),
        Op::Call(1),
        Op::Equal,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (hashtable-hash-function (make-eq-hashtable)) => #f
#[test]
fn test_test456() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(98),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(210),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? string=? (hashtable-equivalence-function (make-hashtable string-hash string=?))) => #t
#[test]
fn test_test457() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ReferFree(56),
        Op::Push,
        Op::Frame(11),
        Op::Frame(7),
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Call(2),
        Op::Push,
        Op::ReferFree(209),
        Op::Call(1),
        Op::Equal,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? string-hash (hashtable-hash-function (make-hashtable string-hash string=?))) => #t
#[test]
fn test_test458() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ReferFree(103),
        Op::Push,
        Op::Frame(11),
        Op::Frame(7),
        Op::ReferFree(103),
        Op::Push,
        Op::ReferFree(56),
        Op::Push,
        Op::ReferFree(200),
        Op::Call(2),
        Op::Push,
        Op::ReferFree(210),
        Op::Call(1),
        Op::Equal,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (string-ci-hash "abc") (string-ci-hash "AbC")) => #t
#[test]
fn test_test459() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("abc")),
        Op::Push,
        Op::ReferFree(105),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.new_string("AbC")),
        Op::Push,
        Op::ReferFree(105),
        Op::Call(1),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (symbol-hash 'abc) (symbol-hash 'abc)) => #t
#[test]
fn test_test460() {
    let mut vm = Vm::new();
    let a = vm.gc.symbol_intern("abc");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(106),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(106),
        Op::Call(1),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (= (symbol-hash 'abc) (symbol-hash 'aBc)) => #f
#[test]
fn test_test461() {
    let mut vm = Vm::new();
    let b = vm.gc.symbol_intern("aBc");
    let a = vm.gc.symbol_intern("abc");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(a),
        Op::Push,
        Op::ReferFree(106),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(b),
        Op::Push,
        Op::ReferFree(106),
        Op::Call(1),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (= (equal-hash '(a b c)) (equal-hash '(a b c))) => #t
#[test]
fn test_test462() {
    let mut vm = Vm::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(5),
        Op::Constant(vm.gc.list3(a, b, c)),
        Op::Push,
        Op::ReferFree(107),
        Op::Call(1),
        Op::Push,
        Op::Frame(5),
        Op::Constant(vm.gc.list3(a, b, c)),
        Op::Push,
        Op::ReferFree(107),
        Op::Call(1),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (equal? eqv? (hashtable-equivalence-function (make-eqv-hashtable))) => #t
#[test]
fn test_test463() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ReferFree(92),
        Op::Push,
        Op::Frame(7),
        Op::Frame(3),
        Op::ReferFree(99),
        Op::Call(0),
        Op::Push,
        Op::ReferFree(209),
        Op::Call(1),
        Op::Equal,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}
