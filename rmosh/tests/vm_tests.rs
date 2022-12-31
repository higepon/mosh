use rmosh::{
    self,
    gc::Gc,
    objects::{Closure, Object, Pair, Procedure, SString, Symbol, Vector},
    op::Op,
    vm::Vm, equal::Equal,
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
fn show_size() {
    println!("SIZE_OF_CLOSURE={}", SIZE_OF_CLOSURE);
    println!("SIZE_OF_PAIR={}", SIZE_OF_PAIR);
    println!("SIZE_OF_PROCEDURE={}", SIZE_OF_PROCEDURE);
    println!("SIZE_OF_STRING={}", SIZE_OF_STRING);
    println!("SIZE_OF_SYMBOL={}", SIZE_OF_SYMBOL);
    println!("SIZE_OF_VECTOR={}", SIZE_OF_VECTOR);
}
*/

/*
   Base display closure
   free variables
   symbols for closure names.
   3 strings.
   4 closures.
*/
static SIZE_OF_MIN_VM: usize = SIZE_OF_CLOSURE
    + (SIZE_OF_PROCEDURE * 623)
    + SIZE_OF_CLOSURE * 4
    + SIZE_OF_SYMBOL * 5
    + SIZE_OF_STRING * 3;

fn test_ops_with_size(vm: &mut Vm, ops: Vec<Op>, expected: Object, expected_heap_diff: usize) {
    let ret = vm.run(ops.as_ptr(), ops.len());
    // Remove reference to ret.
    vm.ac = Object::Unspecified;
    vm.mark_and_sweep();
    assert_eq!(vm.gc.bytes_allocated(), SIZE_OF_MIN_VM + expected_heap_diff);
    let e = Equal::new();
    if !e.is_equal(&mut vm.gc, &ret, &expected) {
        println!("ret={} expected={}", ret, expected);
        assert_eq!(ret, expected);
    }
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

// (and)
#[test]
fn and_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::True), Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}


// ((lambda () 3))
#[test]
fn call0_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (+ a a)) 1)
#[test]
fn call1_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(2)), Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b) (+ a b)) 1 2)
#[test]
fn call2_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}


// (if 1 2 3)
#[test]
fn if0_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (if #f 2 3)
#[test]
fn if1_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (if #f #f #t)
#[test]
fn if2_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::True),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 0)) x)
#[test]
fn let0_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(0)), Op::Halt];
    let expected = Object::Number(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 1) (y 2)) (+ x y))
#[test]
fn let1_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 1)) (let ((y 2)) (+ x y)))
#[test]
fn nested_let0_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 1)) (let ((y 2)) (let ((z 3)) (+ x y z))))
#[test]
fn nested_let1_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(6)), Op::Halt];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// #t
#[test]
fn test0_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::True), Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (source-info '(3))
#[test]
fn test1_optimized() {
    let mut vm = Vm::new();

    let str0 = vm
        .gc
        .new_string("<transcoded-textual-input-port <binary-input-port src/all-tests.scm>>");
    let list0 = vm.gc.listn(&[str0, Object::Number(2)]);
    let list1 = vm.gc.listn(&[Object::Number(3)]);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(149, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (((lambda () (lambda () 102))))
#[test]
fn test10_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(102)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (when #t 1 2 34)
#[test]
fn test100_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::True),
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(34)),
        Op::Halt,
    ];
    let expected = Object::Number(34);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not 3)
#[test]
fn test101_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (unless #f 1 2 48)
#[test]
fn test102_optimized() {
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
    let expected = Object::Number(48);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (and 3 4 5)
#[test]
fn test103_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::Constant(Object::Number(5)),
        Op::Halt,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 0 (and (set! a (+ a 1))) a)
#[test]
fn test104_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(Object::Number(0)),
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 0 (or (set! a (+ a 1))) a)
#[test]
fn test105_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(Object::Number(0)),
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (and 3 #f 5)
#[test]
fn test106_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::False),
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or 3 4 5)
#[test]
fn test107_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or #f #f #f)
#[test]
fn test108_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::False),
        Op::Constant(Object::False),
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> 4 3)
#[test]
fn test109_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(3)),
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda () (lambda (a) 102))) 101)
#[test]
fn test11_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(6),
        Op::ConstantPush(Object::Number(101)),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(102)),
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> 4 3 2)
#[test]
fn test110_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(4),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> 4 3 1 2)
#[test]
fn test111_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(7),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(1)),
        Op::BranchNotGt(4),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (>= 3 3 3)
#[test]
fn test112_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotGe(4),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (>= 4 3 3)
#[test]
fn test113_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotGe(4),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (>= 4 3)
#[test]
fn test114_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(3)),
        Op::NumberGe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< 1 2)
#[test]
fn test115_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< 1 2 3)
#[test]
fn test116_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotLt(4),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< 1 5 3)
#[test]
fn test117_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(5)),
        Op::BranchNotLt(4),
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(Object::Number(3)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 2)
#[test]
fn test118_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 2 3)
#[test]
fn test119_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotLe(4),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda () (lambda (a) a))) 103)
#[test]
fn test12_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(6),
        Op::ConstantPush(Object::Number(103)),
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
    let expected = Object::Number(103);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 3 3)
#[test]
fn test120_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotLe(4),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 5 3)
#[test]
fn test121_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(5)),
        Op::BranchNotLe(4),
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(Object::Number(3)),
        Op::NumberLe,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? #t #t)
#[test]
fn test122_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::True),
        Op::Constant(Object::True),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? #t #f)
#[test]
fn test123_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::True),
        Op::Constant(Object::False),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? 'a 'a)
#[test]
fn test124_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![Op::ConstantPush(sym0), Op::Constant(sym0), Op::Eq, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (eq? 'a 'b)
#[test]
fn test125_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![Op::ConstantPush(sym0), Op::Constant(sym1), Op::Eq, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (pair? (cons 1 2))
#[test]
fn test126_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::PairP,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (pair? 3)
#[test]
fn test127_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::PairP, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (symbol? 'a)
#[test]
fn test128_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![Op::Constant(sym0), Op::SymbolP, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (symbol? 3)
#[test]
fn test129_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::SymbolP, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda (a) (lambda () a)) 10))
#[test]
fn test13_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(10)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(10);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cond (#f 1) (#t 3))
#[test]
fn test130_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::True),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cond (#f 1) (#f 2) (else 3))
#[test]
fn test131_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::False),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cond (#t 3) (#f 2) (else 1))
#[test]
fn test132_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::True),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cond ((cons 1 2) => car) (#f 2) (else 3))
#[test]
fn test133_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFreePush(3),
        Op::Display(1),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Test(5),
        Op::Frame(6),
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 1),
        Op::LocalJmp(3),
        Op::Constant(Object::False),
        Op::Constant(Object::Number(3)),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0)) `(,a 4 5))
#[test]
fn test134_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(0), Object::Number(4), Object::Number(5)]);
    let list1 = vm.gc.listn(&[Object::Number(4), Object::Number(5)]);

    let ops = vec![
        Op::ConstantPush(Object::Number(0)),
        Op::Constant(list1),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,a 4 5))
#[test]
fn test135_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0, Object::Number(4), Object::Number(5)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4), Object::Number(5)]);

    let ops = vec![
        Op::ConstantPush(list2),
        Op::Constant(list3),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a 4 5))
#[test]
fn test136_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
        Object::Number(5),
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list2 = vm.gc.listn(&[Object::Number(4), Object::Number(5)]);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(list2),
        Op::Append2,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((name 'a)) `(list ,name ',name))
#[test]
fn test137_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("list");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("quote");
    let list0 = vm.gc.listn(&[sym2, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1, list0]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::ConstantPush(sym1),
        Op::ConstantPush(sym2),
        Op::ConstantPush(sym1),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::PushConstant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// `(list ,(+ 1 2) 4)
#[test]
fn test138_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("list");
    let list0 = vm.gc.listn(&[sym0, Object::Number(3), Object::Number(4)]);
    let list1 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(list1),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(1 . ,a))
#[test]
fn test139_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(list1),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (set! a 12) a) 2)
#[test]
fn test14_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(2)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(12)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `,a)
#[test]
fn test140_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![Op::Constant(list1), Op::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a))
#[test]
fn test141_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![Op::Constant(list1), Op::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(0 ,@a))
#[test]
fn test142_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(0),
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::ConstantPush(Object::Number(0)),
        Op::Constant(list1),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(0 ,a 4))
#[test]
fn test143_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[Object::Number(0), list0, Object::Number(4)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(Object::Number(0)),
        Op::ConstantPush(list2),
        Op::Constant(list3),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a 4))
#[test]
fn test144_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list2 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(list2),
        Op::Append2,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `((,@a) 4))
#[test]
fn test145_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0, Object::Number(4)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(list2),
        Op::Constant(list3),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `((,a) 4))
#[test]
fn test146_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0]);
    let list2 = vm.gc.listn(&[list1, Object::Number(4)]);
    let list3 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list4 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(list3),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::PushConstant(list4),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list2;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// `b
#[test]
fn test147_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("b");

    let ops = vec![Op::Constant(sym0), Op::Halt];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (list 1 2 3)
#[test]
fn test148_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::Frame(5),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(89, 3),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (aif (+ 1 2) it #f)
#[test]
fn test149_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Test(2),
        Op::Constant(Object::Number(3)),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) ((lambda () (set! a 101)))) '())
#[test]
fn test15_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Nil),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(101)),
        Op::AssignLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(101);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-length "abc")
#[test]
fn test150_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(19, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-length "あいう")
#[test]
fn test151_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("あいう");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(19, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string->symbol "abc")
#[test]
fn test152_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("abc");
    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(20, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (number->string 123)
#[test]
fn test153_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("123");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(123)),
        Op::ReferFreeCall(25, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (proc1 . a) a) (proc1 1 2 3 4))
#[test]
fn test154_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);

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
        Op::Frame(6),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Number(4)),
        Op::ReferGlobalCall(vm.gc.intern("proc1"), 4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3)
#[test]
fn test155_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::List(2),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) a) 1 2 3 4 5)
#[test]
fn test156_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Number(4)),
        Op::ConstantPush(Object::Number(5)),
        Op::List(4),
        Op::Constant(Object::Number(1)),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3 4 5)
#[test]
fn test157_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
        Object::Number(5),
    ]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Number(4)),
        Op::ConstantPush(Object::Number(5)),
        Op::List(4),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) e) 1 2 3 4)
#[test]
fn test158_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::List(0),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Nil;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) a) 1 2 3 4)
#[test]
fn test159_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::List(0), Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (g) ((lambda (f) (f 2)) (lambda (a) (g a)))) (lambda (x) x))
#[test]
fn test16_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(2)), Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) b) 1 2 3 4)
#[test]
fn test160_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::List(0), Op::Constant(Object::Number(2)), Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) c) 1 2 3 4)
#[test]
fn test161_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::List(0), Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) d) 1 2 3 4)
#[test]
fn test162_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::List(0), Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (append '(1 2) '(3 4))
#[test]
fn test163_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);
    let list1 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);
    let list2 = vm.gc.listn(&[Object::Number(3), Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(list2),
        Op::Append2,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (append)
#[test]
fn test164_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Nil), Op::Halt];
    let expected = Object::Nil;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define x 3) x)
#[test]
fn test165_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::DefineGlobal(vm.gc.intern("x")),
        Op::ReferGlobal(vm.gc.intern("x")),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (hoge . a) a) (hoge 1 2 3))
#[test]
fn test166_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

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
        Op::Frame(5),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferGlobalCall(vm.gc.intern("hoge"), 3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (begin (define (hige a . b) b) (hige 1 2 3))
#[test]
fn test167_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

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
        Op::Frame(5),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferGlobalCall(vm.gc.intern("hige"), 3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda a a) '(3 2))
#[test]
fn test168_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);
    let list1 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);

    let ops = vec![
        Op::Frame(7),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::PushConstant(list1),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (equal? '(1 2 (3)) '(1 2 (3)))
#[test]
fn test169_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[Object::Number(1), Object::Number(2), list0]);
    let list2 = vm.gc.listn(&[Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(1), Object::Number(2), list2]);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(list3),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () 3 4 5))
#[test]
fn test17_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::Constant(Object::Number(5)),
        Op::Halt,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3)) 3 2 1)
#[test]
fn test170_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(1)),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (make-string 3)
#[test]
fn test171_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("   ");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(17, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (make-string 3 #\c)
#[test]
fn test172_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("ccc");

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Char('c')),
        Op::ReferFreeCall(17, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (apply car '((3)))
#[test]
fn test173_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0]);

    let ops = vec![
        Op::Frame(4),
        Op::ReferFreePush(3),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a) a) '(3))
#[test]
fn test174_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);

    let ops = vec![
        Op::Frame(7),
        Op::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Return(1),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a b) (+ a b)) '(5 2))
#[test]
fn test175_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(5), Object::Number(2)]);

    let ops = vec![
        Op::Frame(9),
        Op::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a b c) (+ a b c)) '(5 2 1))
#[test]
fn test176_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(5), Object::Number(2), Object::Number(1)]);

    let ops = vec![
        Op::Frame(11),
        Op::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(8);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a) (car a)) '((3)))
#[test]
fn test177_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0]);

    let ops = vec![
        Op::Frame(8),
        Op::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(0),
        Op::Car,
        Op::Return(1),
        Op::PushConstant(list1),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a . b) (+ a (car b))) '(1 2))
#[test]
fn test178_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);

    let ops = vec![
        Op::Frame(10),
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::Car,
        Op::NumberAdd,
        Op::Return(2),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-append "12" "345" "6")
#[test]
fn test179_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("123456");
    let str1 = vm.gc.new_string("12");
    let str2 = vm.gc.new_string("345");
    let str3 = vm.gc.new_string("6");

    let ops = vec![
        Op::Frame(5),
        Op::ConstantPush(str1),
        Op::ConstantPush(str2),
        Op::ConstantPush(str3),
        Op::ReferFreeCall(22, 3),
        Op::Halt,
        Op::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 4,
    );
}

// (number? 3)
#[test]
fn test18_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (find (lambda (e) (= e 3)) (list 1 2 3))
#[test]
fn test180_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(12),
        Op::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPushConstant(0, Object::Number(3)),
        Op::NumberEqual,
        Op::Return(1),
        Op::PushFrame(5),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(89, 3),
        Op::Push,
        Op::ReferGlobalCall(vm.gc.intern("find"), 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string? "hige")
#[test]
fn test181_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("hige");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(31, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (assoc "key" '(("key" "value")))
#[test]
fn test182_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("key");
    let str1 = vm.gc.new_string("value");
    let str2 = vm.gc.new_string("key");
    let str3 = vm.gc.new_string("key");
    let str4 = vm.gc.new_string("value");
    let list0 = vm.gc.listn(&[str0, str1]);
    let list1 = vm.gc.listn(&[str3, str4]);
    let list2 = vm.gc.listn(&[list1]);

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(str2),
        Op::ConstantPush(list2),
        Op::ReferFreeCall(154, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 5,
    );
}

// ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p))
#[test]
fn test184_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(3));

    let ops = vec![
        Op::LetFrame(2),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(3)),
        Op::SetCdr,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (define q (cons 1 2)) (set-car! q 3) q))
#[test]
fn test185_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(3), Object::Number(2));

    let ops = vec![
        Op::LetFrame(2),
        Op::Undef,
        Op::Push,
        Op::Box(0),
        Op::Enter(1),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(3)),
        Op::SetCar,
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin #f #t)
#[test]
fn test186_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::True),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (vector-length (make-vector 3))
#[test]
fn test187_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::VectorLength,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let loop ((i 0)) (if (= i 100) (+ i 1) (loop (+ i 1))))
#[test]
fn test188_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(5),
        Op::ConstantPush(Object::Number(0)),
        Op::Enter(1),
        Op::ReferLocalPushConstant(0, Object::Number(100)),
        Op::BranchNotNumberEqual(4),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAdd,
        Op::LocalJmp(5),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::Shiftj(1, 1, -1),
        Op::LocalJmp(-8),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(101);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0)) (cond (#t (set! a (+ a 1)) (set! a (+ a 1)) a)))
#[test]
fn test189_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ConstantPush(Object::Number(0)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::True),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (number? 'a)
#[test]
fn test19_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (char? #\あ)
#[test]
fn test190_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Char('あ')),
        Op::ReferFreeCall(53, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? (list 'a) (list 'a))
#[test]
fn test191_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(89, 1),
        Op::PushFrame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(89, 1),
        Op::Eq,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((x (list 'a))) (eq? x x))
#[test]
fn test192_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFreePush(89),
        Op::Display(1),
        Op::Frame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(0, 1),
        Op::PushEnter(1),
        Op::ReferLocalPush(0),
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (map1 (lambda (s) (string-append s "123")) '("ABC" "DEF"))
#[test]
fn test193_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("ABC123");
    let str1 = vm.gc.new_string("DEF123");
    let str2 = vm.gc.new_string("123");
    let str3 = vm.gc.new_string("ABC");
    let str4 = vm.gc.new_string("DEF");
    let list0 = vm.gc.listn(&[str0, str1]);
    let list1 = vm.gc.listn(&[str3, str4]);

    let ops = vec![
        Op::Frame(11),
        Op::ReferFreePush(22),
        Op::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocalPushConstant(0, str2),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(2, 1),
        Op::Return(1),
        Op::PushConstant(list1),
        Op::Push,
        Op::ReferGlobalCall(vm.gc.intern("map1"), 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 5,
    );
}

// (let1 a '() (let1 G68 (lambda (i) (if (>= i 10000) i (a (+ i 1)))) (set! a G68) (a 0)))
#[test]
fn test194_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ConstantPush(Object::Nil),
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocalPush(0),
        Op::ReferLocalPush(0),
        Op::Display(2),
        Op::ReferLocalPush(0),
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocalPushConstantBranchNotGe(0, Object::Number(10000), 3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::AssignFree(0),
        Op::Frame(5),
        Op::ConstantPush(Object::Number(0)),
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
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((p (open-string-input-port "12345"))) (read-char p) (read-char p))
#[test]
fn test195_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("12345");

    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFreePush(35),
        Op::Display(1),
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(0, 1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('2');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (eof-object? (let ((p (open-string-input-port "1"))) (read-char p) (read-char p)))
#[test]
fn test196_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("1");

    let ops = vec![
        Op::Frame(15),
        Op::LetFrame(2),
        Op::ReferFreePush(35),
        Op::Display(1),
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(0, 1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Push,
        Op::ReferFreeCall(27, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a))))
#[test]
fn test197_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![
        Op::ConstantPush(sym1),
        Op::Constant(sym0),
        Op::BranchNotEqv(3),
        Op::Constant(sym1),
        Op::LocalJmp(7),
        Op::ConstantPush(sym0),
        Op::Constant(sym0),
        Op::BranchNotEqv(3),
        Op::Constant(sym0),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (begin (let ((xxy 'a)) (case xxy ((b) 'b) ((c) 'c) (else 3))))
#[test]
fn test198_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("c");

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(sym1),
        Op::BranchNotEqv(3),
        Op::Constant(sym0),
        Op::LocalJmp(7),
        Op::ConstantPush(sym2),
        Op::Constant(sym1),
        Op::BranchNotEqv(3),
        Op::Constant(sym2),
        Op::LocalJmp(2),
        Op::Constant(Object::Number(3)),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (* 2 3)
#[test]
fn test199_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(6)), Op::Halt];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (and)
#[test]
fn test2_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::True), Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (number? 'a)
#[test]
fn test20_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (* 2 3 4)
#[test]
fn test200_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(24)), Op::Halt];
    let expected = Object::Number(24);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string->number "123")
#[test]
fn test201_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("123");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(21, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(123);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (let ((p (open-string-input-port "123 456"))) (read-char p))
#[test]
fn test202_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("123 456");

    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFreePush(35),
        Op::Display(1),
        Op::Frame(3),
        Op::ConstantPush(str0),
        Op::ReferFreeCall(0, 1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::ReadChar,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('1');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (reverse '(1 2 3 4))
#[test]
fn test203_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(4),
        Object::Number(3),
        Object::Number(2),
        Object::Number(1),
    ]);
    let list1 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(26, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-split "wiki&cmd" #\&)
#[test]
fn test204_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("wiki");
    let str1 = vm.gc.new_string("cmd");
    let str2 = vm.gc.new_string("wiki&cmd");
    let list0 = vm.gc.listn(&[str0, str1]);

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(str2),
        Op::ConstantPush(Object::Char('&')),
        Op::ReferFreeCall(23, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 3,
    );
}

// (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1)
#[test]
fn test205_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("cbc");

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Char('c')),
        Op::ReferFreeCall(17, 2),
        Op::DefineGlobal(vm.gc.intern("str1")),
        Op::Frame(5),
        Op::ReferGlobalPush(vm.gc.intern("str1")),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Char('b')),
        Op::ReferFreeCall(18, 3),
        Op::ReferGlobal(vm.gc.intern("str1")),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (let* ((a 0) (b (lambda (x y) a))) (b (begin (set! a 1)) (begin (set! a 2))))
#[test]
fn test206_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(0)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(1)),
        Op::AssignLocal(0),
        Op::Constant(Object::Number(2)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// #\a
#[test]
fn test207_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Char('a')), Op::Halt];
    let expected = Object::Char('a');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eof-object? 3)
#[test]
fn test208_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(27, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// 102
#[test]
fn test209_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(102)), Op::Halt];
    let expected = Object::Number(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+ 4)
#[test]
fn test21_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// `(list ,(+ 1 2) 4)
#[test]
fn test210_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("list");
    let list0 = vm.gc.listn(&[sym0, Object::Number(3), Object::Number(4)]);
    let list1 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(list1),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((name 'a)) `(list ,name ',name))
#[test]
fn test211_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("list");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("quote");
    let list0 = vm.gc.listn(&[sym2, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1, list0]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::ConstantPush(sym1),
        Op::ConstantPush(sym2),
        Op::ConstantPush(sym1),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::PushConstant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
#[test]
fn test212_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let list0 = vm.gc.listn(&[
        sym0,
        Object::Number(3),
        Object::Number(4),
        Object::Number(5),
        Object::Number(6),
        sym1,
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(4), Object::Number(-5), Object::Number(6)]);
    let list2 = vm.gc.listn(&[sym1]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::ConstantPush(Object::Number(3)),
        Op::Frame(4),
        Op::ReferFreePush(410),
        Op::ConstantPush(list1),
        Op::ReferGlobalCall(vm.gc.intern("map"), 2),
        Op::PushConstant(list2),
        Op::Append2,
        Op::Cons,
        Op::Cons,
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (vector? #(3))
#[test]
fn test213_optimized() {
    let mut vm = Vm::new();

    let vec0 = vm.gc.new_vector(&vec![Object::Number(3)]);

    let ops = vec![Op::Constant(vec0), Op::VectorP, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (proc-01) 3) (proc-01))
#[test]
fn test214_optimized() {
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
        Op::Frame(2),
        Op::ReferGlobalCall(vm.gc.intern("proc-01"), 0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (add3 a b) (+ a b)) (add3 1 2))
#[test]
fn test215_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::DefineGlobal(vm.gc.intern("add3")),
        Op::Frame(4),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ReferGlobalCall(vm.gc.intern("add3"), 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define add2 (lambda (a b) (+ a b))) (add2 1 2))
#[test]
fn test216_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAdd,
        Op::Return(2),
        Op::DefineGlobal(vm.gc.intern("add2")),
        Op::Frame(4),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ReferGlobalCall(vm.gc.intern("add2"), 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1))
#[test]
fn test217_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::DefineGlobal(vm.gc.intern("z")),
        Op::ReferGlobalPush(vm.gc.intern("z")),
        Op::ConstantPush(Object::Number(0)),
        Op::Constant(Object::Number(1)),
        Op::VectorSet,
        Op::ReferGlobalPush(vm.gc.intern("z")),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::VectorSet,
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::Constant(Object::Number(3)),
        Op::NullP,
        Op::ReferGlobalPush(vm.gc.intern("z")),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(3)),
        Op::VectorSet,
        Op::ReferGlobalPush(vm.gc.intern("z")),
        Op::Constant(Object::Number(1)),
        Op::VectorRef,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (proc-2) (define (rec) 3) (rec)) (proc-2))
#[test]
fn test218_optimized() {
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
        Op::DefineGlobal(vm.gc.intern("proc-2")),
        Op::Frame(2),
        Op::ReferGlobalCall(vm.gc.intern("proc-2"), 0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (define (func2) (define val 4) val) (func2))
#[test]
fn test219_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(4)),
        Op::Return(0),
        Op::DefineGlobal(vm.gc.intern("func2")),
        Op::Frame(2),
        Op::ReferGlobalCall(vm.gc.intern("func2"), 0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+ 4 3)
#[test]
fn test22_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(7)), Op::Halt];
    let expected = Object::Number(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (if (values 1 2 3) #t #f)
#[test]
fn test220_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Test(2),
        Op::Constant(Object::True),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () (values 4 5)) (lambda (a b) b))
#[test]
fn test221_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFreePush(152),
        Op::Display(1),
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(5)),
        Op::Values(2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(7),
        Op::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocal(1),
        Op::Return(2),
        Op::Push,
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))
#[test]
fn test222_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(4),
        Op::ReferFreePush(152),
        Op::Display(1),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(11),
        Op::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::Push,
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () (values 1 2 3)) list)
#[test]
fn test223_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::LetFrame(4),
        Op::ReferFreePush(89),
        Op::ReferFreePush(152),
        Op::Display(2),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(4),
        Op::ReferFreePush(1),
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () 1) (lambda (x) (+ x 1234)))
#[test]
fn test224_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFreePush(152),
        Op::Display(1),
        Op::Constant(Object::Number(1)),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(8),
        Op::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPushConstant(0, Object::Number(1234)),
        Op::NumberAdd,
        Op::Return(1),
        Op::Push,
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1235);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (receive (a b c) (values 1 2 3) (+ a b c))
#[test]
fn test225_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(4),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Values(3),
        Op::Receive(3, 0),
        Op::Enter(3),
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Leave(3),
        Op::Halt,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (receive z (values 'x 'y) z)
#[test]
fn test226_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("x");
    let sym1 = vm.gc.symbol_intern("y");
    let list0 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(sym0),
        Op::Constant(sym1),
        Op::Values(2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (receive (a . b) (values 'x 'y 'z) b)
#[test]
fn test227_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("y");
    let sym1 = vm.gc.symbol_intern("z");
    let sym2 = vm.gc.symbol_intern("x");
    let list0 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(sym2),
        Op::ConstantPush(sym0),
        Op::Constant(sym1),
        Op::Values(3),
        Op::Receive(1, 1),
        Op::Enter(2),
        Op::ReferLocal(1),
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (receive (a . b) (values 'x 'y 'z) a)
#[test]
fn test228_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("x");
    let sym1 = vm.gc.symbol_intern("y");
    let sym2 = vm.gc.symbol_intern("z");

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(sym0),
        Op::ConstantPush(sym1),
        Op::Constant(sym2),
        Op::Values(3),
        Op::Receive(1, 1),
        Op::Enter(2),
        Op::ReferLocal(0),
        Op::Leave(2),
        Op::Halt,
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (receive x (apply values '(1 2 3)) x)
#[test]
fn test229_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::LetFrame(2),
        Op::ReferFreePush(110),
        Op::ReferFreePush(152),
        Op::Display(2),
        Op::Frame(4),
        Op::ReferFreePush(1),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(0, 2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+ 4 3 10)
#[test]
fn test23_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(17)), Op::Halt];
    let expected = Object::Number(17);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () (values 1 2)) cons)
#[test]
fn test230_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![
        Op::LetFrame(3),
        Op::ReferFreePush(1),
        Op::ReferFreePush(152),
        Op::Display(2),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Values(2),
        Op::Receive(0, 1),
        Op::Enter(1),
        Op::Frame(4),
        Op::ReferFreePush(1),
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 2),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 'a '())
#[test]
fn test232_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (cons '(a) '(b c d))
#[test]
fn test233_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let sym3 = vm.gc.symbol_intern("d");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[list0, sym1, sym2, sym3]);
    let list2 = vm.gc.listn(&[sym0]);
    let list3 = vm.gc.listn(&[sym1, sym2, sym3]);

    let ops = vec![
        Op::ConstantPush(list2),
        Op::Constant(list3),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (cons "a" '(b c))
#[test]
fn test234_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let str0 = vm.gc.new_string("a");
    let str1 = vm.gc.new_string("a");
    let list0 = vm.gc.listn(&[str0, sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        Op::ConstantPush(str1),
        Op::Constant(list1),
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 2,
    );
}

// (cons 'a 3)
#[test]
fn test235_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let pair0 = vm.gc.cons(sym0, Object::Number(3));

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (cons '(a b) 'c)
#[test]
fn test236_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);
    let pair0 = vm.gc.cons(list0, sym2);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(sym2),
        Op::Cons,
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (car '(a b c))
#[test]
fn test237_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);

    let ops = vec![Op::Constant(list0), Op::Car, Op::Halt];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (car '((a) b c d))
#[test]
fn test238_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let sym3 = vm.gc.symbol_intern("d");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);
    let list2 = vm.gc.listn(&[list1, sym1, sym2, sym3]);

    let ops = vec![Op::Constant(list2), Op::Car, Op::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (car '(1 . 2))
#[test]
fn test239_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![Op::Constant(pair0), Op::Car, Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+ 1 1 1 1)
#[test]
fn test24_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cdr '((a) b c d))
#[test]
fn test240_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let sym2 = vm.gc.symbol_intern("d");
    let sym3 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);
    let list1 = vm.gc.listn(&[sym3]);
    let list2 = vm.gc.listn(&[list1, sym0, sym1, sym2]);

    let ops = vec![Op::Constant(list2), Op::Cdr, Op::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (cdr '(1 . 2))
#[test]
fn test241_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![Op::Constant(pair0), Op::Cdr, Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (reverse '(a b c))
#[test]
fn test242_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("c");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);
    let list1 = vm.gc.listn(&[sym2, sym1, sym0]);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(26, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (reverse '(a (b c) d (e (f))))
#[test]
fn test243_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("e");
    let sym1 = vm.gc.symbol_intern("f");
    let sym2 = vm.gc.symbol_intern("d");
    let sym3 = vm.gc.symbol_intern("b");
    let sym4 = vm.gc.symbol_intern("c");
    let sym5 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym1]);
    let list1 = vm.gc.listn(&[sym0, list0]);
    let list2 = vm.gc.listn(&[sym3, sym4]);
    let list3 = vm.gc.listn(&[list1, sym2, list2, sym5]);
    let list4 = vm.gc.listn(&[sym3, sym4]);
    let list5 = vm.gc.listn(&[sym1]);
    let list6 = vm.gc.listn(&[sym0, list5]);
    let list7 = vm.gc.listn(&[sym5, list4, sym2, list6]);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(list7),
        Op::ReferFreeCall(26, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = list3;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 6 + SIZE_OF_STRING * 0,
    );
}

// (equal? 'a 'a)
#[test]
fn test244_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(sym0),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (equal? '(a) '(a))
#[test]
fn test245_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);

    let ops = vec![
        Op::ConstantPush(list0),
        Op::Constant(list1),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (equal? '(a (b) c) '(a (b) c))
#[test]
fn test246_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym1]);
    let list1 = vm.gc.listn(&[sym0, list0, sym2]);
    let list2 = vm.gc.listn(&[sym1]);
    let list3 = vm.gc.listn(&[sym0, list2, sym2]);

    let ops = vec![
        Op::ConstantPush(list1),
        Op::Constant(list3),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (equal? "abc" "abc")
#[test]
fn test247_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("abc");
    let str1 = vm.gc.new_string("abc");

    let ops = vec![
        Op::ConstantPush(str0),
        Op::Constant(str1),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 2,
    );
}

// (equal? 2 2)
#[test]
fn test248_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (equal? (make-vector 5 'a) (make-vector 5 'a))
#[test]
fn test249_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(sym0),
        Op::MakeVector,
        Op::PushConstant(Object::Number(5)),
        Op::PushConstant(sym0),
        Op::MakeVector,
        Op::Equal,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (- 10 5)
#[test]
fn test25_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(5)), Op::Halt];
    let expected = Object::Number(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? 'a 'a)
#[test]
fn test250_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![Op::ConstantPush(sym0), Op::Constant(sym0), Op::Eq, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (eq? '(a) '(a))
#[test]
fn test251_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);

    let ops = vec![
        Op::ConstantPush(list0),
        Op::Constant(list1),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (eq? (list 'a) (list 'a))
#[test]
fn test252_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(89, 1),
        Op::PushFrame(3),
        Op::ConstantPush(sym0),
        Op::ReferFreeCall(89, 1),
        Op::Eq,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (eq? "a" "a")
#[test]
fn test253_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("a");
    let str1 = vm.gc.new_string("a");

    let ops = vec![Op::ConstantPush(str0), Op::Constant(str1), Op::Eq, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 2,
    );
}

// (eq? "" "")
#[test]
fn test254_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("");
    let str1 = vm.gc.new_string("");

    let ops = vec![Op::ConstantPush(str0), Op::Constant(str1), Op::Eq, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 2,
    );
}

// (eq? '() '())
#[test]
fn test255_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Nil),
        Op::Constant(Object::Nil),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? 2 2)
#[test]
fn test256_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? #\A #\A)
#[test]
fn test257_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Char('A')),
        Op::Constant(Object::Char('A')),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? car car)
#[test]
fn test258_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::ReferFreePush(3), Op::ReferFree(3), Op::Eq, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((n (+ 2 3))) (eq? n n))
#[test]
fn test259_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(Object::Number(5)),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (- 10 5 2)
#[test]
fn test26_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x '(a))) (eq? x x))
#[test]
fn test260_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);

    let ops = vec![
        Op::ConstantPush(list0),
        Op::Constant(list1),
        Op::Eq,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((x '#())) (eq? x x))
#[test]
fn test261_optimized() {
    let mut vm = Vm::new();

    let vec0 = vm.gc.new_vector(&vec![]);
    let vec1 = vm.gc.new_vector(&vec![]);

    let ops = vec![Op::ConstantPush(vec0), Op::Constant(vec1), Op::Eq, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((p (lambda (x) x))) (eq? p p))
#[test]
fn test262_optimized() {
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
        Op::PushEnter(1),
        Op::ReferLocalPush(0),
        Op::ReferLocal(0),
        Op::Eq,
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (- 3 4)
#[test]
fn test263_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(-1)), Op::Halt];
    let expected = Object::Number(-1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (- 3 4 5)
#[test]
fn test264_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(-6)), Op::Halt];
    let expected = Object::Number(-6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (- 3)
#[test]
fn test265_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(-3)), Op::Halt];
    let expected = Object::Number(-3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cond ((> 3 2) 'greater) ((< 3 2) 'less))
#[test]
fn test266_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("greater");
    let sym1 = vm.gc.symbol_intern("less");

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotGt(3),
        Op::Constant(sym0),
        Op::LocalJmp(7),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotLt(3),
        Op::Constant(sym1),
        Op::LocalJmp(2),
        Op::Undef,
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal))
#[test]
fn test267_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("equal");
    let sym1 = vm.gc.symbol_intern("greater");
    let sym2 = vm.gc.symbol_intern("less");

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotGt(3),
        Op::Constant(sym1),
        Op::LocalJmp(7),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotLt(3),
        Op::Constant(sym2),
        Op::LocalJmp(2),
        Op::Constant(sym0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (cond ('(1 2 3) => cadr) (else #f))
#[test]
fn test268_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::Constant(list0),
        Op::Test(4),
        Op::Frame(3),
        Op::ConstantPush(list1),
        Op::ReferFreeCall(70, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (do ((vec (make-vector 5)) (i 0 (+ i 1))) ((= i 5) vec) (vector-set! vec i i))
#[test]
fn test269_optimized() {
    let mut vm = Vm::new();

    let vec0 = vm.gc.new_vector(&vec![
        Object::Number(0),
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);

    let ops = vec![
        Op::Frame(28),
        Op::Frame(14),
        Op::Frame(6),
        Op::ConstantPush(Object::Number(0)),
        Op::ReferGlobalPush(vm.gc.intern("i")),
        Op::Constant(Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferGlobalCall(vm.gc.intern("i"), 2),
        Op::PushFrame(6),
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(Object::Nil),
        Op::MakeVector,
        Op::Push,
        Op::ReferGlobalCall(vm.gc.intern("vec"), 1),
        Op::Call(1),
        Op::PushFrame(6),
        Op::ReferGlobalPush(vm.gc.intern("vec")),
        Op::ReferGlobalPush(vm.gc.intern("i")),
        Op::Constant(Object::Number(5)),
        Op::NumberEqual,
        Op::Call(1),
        Op::Push,
        Op::ReferGlobalPush(vm.gc.intern("vec")),
        Op::ReferGlobalPush(vm.gc.intern("i")),
        Op::ReferGlobal(vm.gc.intern("i")),
        Op::VectorSet,
        Op::Push,
        Op::ReferGlobalCall(vm.gc.intern("do"), 3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = vec0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 'a 'b)
#[test]
fn test27_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let pair0 = vm.gc.cons(sym0, sym1);

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(sym1),
        Op::Cons,
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}



// (let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec)
#[test]
fn test271_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("Sue");
    let str1 = vm.gc.new_string("Sue");
    let str2 = vm.gc.new_string("Anna");
    let str3 = vm.gc.new_string("Anna");
    let str4 = vm.gc.new_string("Sue");
    let str5 = vm.gc.new_string("Sue");
    let list0 = vm.gc.listn(&[str0, str1]);
    let list1 = vm.gc.listn(&[
        Object::Number(2),
        Object::Number(2),
        Object::Number(2),
        Object::Number(2),
    ]);
    let list2 = vm.gc.listn(&[str4, str5]);
    let vec0 = vm.gc.new_vector(&vec![Object::Number(0), list0, str2]);

    let ops = vec![
        Op::LetFrame(5),
        Op::ConstantPush(Object::Number(0)),
        Op::ConstantPush(list1),
        Op::Constant(str3),
        Op::Vector(3),
        Op::PushEnter(1),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::PushConstant(list2),
        Op::VectorSet,
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = vec0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (vector-ref '#(1 1 2 3 5 8 13 21) 5)
#[test]
fn test272_optimized() {
    let mut vm = Vm::new();

    let vec0 = vm.gc.new_vector(&vec![
        Object::Number(1),
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(5),
        Object::Number(8),
        Object::Number(13),
        Object::Number(21),
    ]);

    let ops = vec![
        Op::ConstantPush(vec0),
        Op::Constant(Object::Number(5)),
        Op::VectorRef,
        Op::Halt,
    ];
    let expected = Object::Number(8);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or (= 2 2) (> 2 1))
#[test]
fn test273_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotNumberEqual(2),
        Op::LocalJmp(4),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(1)),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or (= 2 2) (< 2 1))
#[test]
fn test274_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::BranchNotNumberEqual(2),
        Op::LocalJmp(4),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(1)),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or #f #f #f)
#[test]
fn test275_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::False),
        Op::Constant(Object::False),
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or '(b c) (/ 3 0))
#[test]
fn test276_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![Op::Constant(list1), Op::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (not #t)
#[test]
fn test277_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::True), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not 3)
#[test]
fn test278_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not (list 3))
#[test]
fn test279_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(89, 1),
        Op::Not,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (car (cons 2 3))
#[test]
fn test28_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Car,
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not #f)
#[test]
fn test280_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::False), Op::Not, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not '())
#[test]
fn test281_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Nil), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not (list))
#[test]
fn test282_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(2),
        Op::ReferFreeCall(89, 0),
        Op::Not,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (not 'nil)
#[test]
fn test283_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("nil");

    let ops = vec![Op::Constant(sym0), Op::Not, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 2) (y 3)) (* x y))
#[test]
fn test284_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(6)), Op::Halt];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))
#[test]
fn test285_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(35)), Op::Halt];
    let expected = Object::Number(35);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x)))
#[test]
fn test286_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(70)), Op::Halt];
    let expected = Object::Number(70);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? 'a 'a)
#[test]
fn test287_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(sym0),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (eqv? 'a 'b)
#[test]
fn test288_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![
        Op::ConstantPush(sym0),
        Op::Constant(sym1),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (eqv? 2 2)
#[test]
fn test289_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cdr (cons 2 3))
#[test]
fn test29_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Cons,
        Op::Cdr,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? '() '())
#[test]
fn test290_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Nil),
        Op::Constant(Object::Nil),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? 100000000 100000000)
#[test]
fn test291_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(100000000)),
        Op::Constant(Object::Number(100000000)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? (cons 1 2) (cons 1 2))
#[test]
fn test292_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(2)),
        Op::Cons,
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? (lambda () 1) (lambda () 2))
#[test]
fn test293_optimized() {
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
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? 123456789101112 123456789101112)
#[test]
fn test294_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(123456789101112)),
        Op::Constant(Object::Number(123456789101112)),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eqv? #f 'nil)
#[test]
fn test295_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("nil");

    let ops = vec![
        Op::ConstantPush(Object::False),
        Op::Constant(sym0),
        Op::Eqv,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (digit->integer #\3 10)
#[test]
fn test297_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Char('3')),
        Op::ConstantPush(Object::Number(10)),
        Op::ReferFreeCall(39, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+)
#[test]
fn test298_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(0)), Op::Halt];
    let expected = Object::Number(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (*)
#[test]
fn test299_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// 3
#[test]
fn test3_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cadr (cons 2 (cons 3 '())))
#[test]
fn test30_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Cadr,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (apply (lambda (a b c) (+ a b c)) 1 2 '(3))
#[test]
fn test303_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);

    let ops = vec![
        Op::Frame(13),
        Op::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(2)),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 4),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a b c) (+ a b c)) '(1 2 3))
#[test]
fn test304_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::Frame(11),
        Op::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (a b c) (+ a b c)) 1 '(2 3))
#[test]
fn test305_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::Frame(12),
        Op::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ReferLocalPush(0),
        Op::ReferLocal(1),
        Op::NumberAddPush,
        Op::ReferLocal(2),
        Op::NumberAdd,
        Op::Return(3),
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferFreeCall(152, 3),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr))
#[test]
fn test306_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(2)]);
    let list1 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);
    let list2 = vm.gc.listn(&[list1]);

    let ops = vec![
        Op::Frame(16),
        Op::ReferFreePush(152),
        Op::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocalPushConstant(1, list2),
        Op::Push,
        Op::ReferFree(0),
        Op::TailCall(2, 2),
        Op::Return(2),
        Op::Push,
        Op::ReferFreePush(3),
        Op::ReferFreePush(4),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Push,
        Op::ReferFreeCall(152, 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (/ 6 2)
#[test]
fn test307_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(6)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (even? 2)
#[test]
fn test309_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(2)),
        Op::ReferFreeCall(408, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 3 3)
#[test]
fn test31_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (even? 3)
#[test]
fn test310_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(408, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (for-all even? '(3 1 4 1 5 9))
#[test]
fn test311_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Number(3),
        Object::Number(1),
        Object::Number(4),
        Object::Number(1),
        Object::Number(5),
        Object::Number(9),
    ]);

    let ops = vec![
        Op::Frame(4),
        Op::ReferFreePush(408),
        Op::ConstantPush(list0),
        Op::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (for-all even? '(3 1 4 1 5 9 . 2))
#[test]
fn test312_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(9), Object::Number(2));
    let pair1 = vm.gc.cons(Object::Number(5), pair0);
    let pair2 = vm.gc.cons(Object::Number(1), pair1);
    let pair3 = vm.gc.cons(Object::Number(4), pair2);
    let pair4 = vm.gc.cons(Object::Number(1), pair3);
    let pair5 = vm.gc.cons(Object::Number(3), pair4);

    let ops = vec![
        Op::Frame(4),
        Op::ReferFreePush(408),
        Op::ConstantPush(pair5),
        Op::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (for-all even? '(2 4 14))
#[test]
fn test313_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(4), Object::Number(14)]);

    let ops = vec![
        Op::Frame(4),
        Op::ReferFreePush(408),
        Op::ConstantPush(list0),
        Op::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (for-all (lambda (n) (and (even? n) n)) '(2 4 14))
#[test]
fn test314_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(4), Object::Number(14)]);

    let ops = vec![
        Op::Frame(12),
        Op::ReferFreePush(408),
        Op::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::Frame(3),
        Op::ReferLocalPush(0),
        Op::ReferFreeCall(0, 1),
        Op::Test(2),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::PushConstant(list0),
        Op::Push,
        Op::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(14);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (- (/ 1 2) (/ 1 4) (/ 1 4))
#[test]
fn test318_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberSubPush,
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberSub,
        Op::Halt,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= (/ 3 2) (+ (/ 1 2) 1))
#[test]
fn test319_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 3 4)
#[test]
fn test32_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= (/ 5 2) (+ 1 (/ 1 2) 1))
#[test]
fn test320_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(5)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberAddPush,
        Op::Constant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (/ (/ 4 2) 1)
#[test]
fn test326_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> 1 (/ 1 2))
#[test]
fn test329_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3)) a)
#[test]
fn test33_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> (/ 1 2) 1)
#[test]
fn test330_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> 1 (/ 1 2))
#[test]
fn test331_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}


// (<= (/ 1 2) 1)
#[test]
fn test333_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (>= 1 (/ 1 2))
#[test]
fn test334_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberGe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (>= (/ 1 2) (/ 1 3))
#[test]
fn test335_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberGe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< (/ 1 2) 1)
#[test]
fn test336_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< 1 (/ 1 2))
#[test]
fn test337_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< (/ 1 2) (/ 1 3))
#[test]
fn test338_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::PushConstant(Object::Number(3)),
        Op::NumberDiv,
        Op::NumberLt,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= (/ 1 2) 1)
#[test]
fn test339_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3) (b 1)) b)
#[test]
fn test34_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 (/ 1 2))
#[test]
fn test340_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberLe,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (= (/ 2 2) 1)
#[test]
fn test342_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(1)),
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 1 (/ 2 2))
#[test]
fn test343_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= (/ 1 2) (/ 2 4))
#[test]
fn test344_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::PushConstant(Object::Number(2)),
        Op::PushConstant(Object::Number(4)),
        Op::NumberDiv,
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}




// (let ((a 3) (b 1)) a)
#[test]
fn test35_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}




// (= (/ (+ (greatest-fixnum) 1) 1) (+ (greatest-fixnum) 1))
#[test]
fn test352_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Constant(Object::Number(1)),
        Op::NumberDiv,
        Op::PushFrame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}


// (flonum? (/ (+ (greatest-fixnum) 1) (inexact (/ 1 3))))
#[test]
fn test354_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(14),
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Frame(6),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFreeCall(300, 1),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFreeCall(288, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1))
#[test]
fn test355_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (fixnum? (/ (+ (greatest-fixnum) 1) (+ (greatest-fixnum) 1)))
#[test]
fn test356_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(12),
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::NumberDiv,
        Op::Push,
        Op::ReferFreeCall(289, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (/ 2)
#[test]
fn test357_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(1 / 2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (/ 3)
#[test]
fn test358_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(3)),
        Op::NumberDiv,
        Op::Halt,
    ];
    let expected = Object::Number(1 / 3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (fixnum? (least-fixnum))
#[test]
fn test359_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Frame(2),
        Op::ReferFreeCall(292, 0),
        Op::Push,
        Op::ReferFreeCall(289, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3) (b 1)) a b)
#[test]
fn test36_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(1)),
        Op::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (fixnum? (greatest-fixnum))
#[test]
fn test360_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::Push,
        Op::ReferFreeCall(289, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



// (number? (+ (greatest-fixnum) 1))
#[test]
fn test363_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(6),
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (number? (- (least-fixnum) 1))
#[test]
fn test364_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(6),
        Op::Frame(2),
        Op::ReferFreeCall(292, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberSubPush,
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (> (+ (greatest-fixnum) 1) (greatest-fixnum))
#[test]
fn test365_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::NumberGt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (< (- (least-fixnum) 1) (least-fixnum))
#[test]
fn test366_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(2),
        Op::ReferFreeCall(292, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberSubPush,
        Op::Frame(2),
        Op::ReferFreeCall(292, 0),
        Op::NumberLt,
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (fixnum? (- (+ (greatest-fixnum) 1) 1))
#[test]
fn test367_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(8),
        Op::Frame(2),
        Op::ReferFreeCall(293, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberAddPush,
        Op::Constant(Object::Number(1)),
        Op::NumberSubPush,
        Op::ReferFreeCall(289, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (fixnum? (+ (- (least-fixnum) 1) 1))
#[test]
fn test368_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(8),
        Op::Frame(2),
        Op::ReferFreeCall(292, 0),
        Op::PushConstant(Object::Number(1)),
        Op::NumberSubPush,
        Op::Constant(Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferFreeCall(289, 1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (number? 3)
#[test]
fn test369_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 a)
#[test]
fn test37_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (number? (/ 1 4))
#[test]
fn test370_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(6),
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(4)),
        Op::NumberDiv,
        Op::Push,
        Op::ReferFreeCall(0, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}






// (div 123 10)
#[test]
fn test375_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(123)),
        Op::ConstantPush(Object::Number(10)),
        Op::ReferFreeCall(411, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (div 123 -10)
#[test]
fn test376_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(123)),
        Op::ConstantPush(Object::Number(-10)),
        Op::ReferFreeCall(411, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(-12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (div -123 10)
#[test]
fn test377_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(-123)),
        Op::ConstantPush(Object::Number(10)),
        Op::ReferFreeCall(411, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(-13);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (div -123 -10)
#[test]
fn test378_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(Object::Number(-123)),
        Op::ConstantPush(Object::Number(-10)),
        Op::ReferFreeCall(411, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Number(13);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-ref "abc" 2)
#[test]
fn test379_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        Op::Frame(4),
        Op::ConstantPush(str0),
        Op::ConstantPush(Object::Number(2)),
        Op::ReferFreeCall(96, 2),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::Char('c');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 b))
#[test]
fn test38_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (list? '(a b c))
#[test]
fn test380_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(list0),
        Op::ReferFreeCall(88, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (list? '())
#[test]
fn test381_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(Object::Nil),
        Op::ReferFreeCall(88, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (list? '(a . b))
#[test]
fn test382_optimized() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let pair0 = vm.gc.cons(sym0, sym1);

    let ops = vec![
        Op::Frame(3),
        Op::ConstantPush(pair0),
        Op::ReferFreeCall(88, 1),
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// "abc"
#[test]
fn test383_optimized() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("abc");
    let str1 = vm.gc.new_string("abc");

    let ops = vec![Op::Constant(str1), Op::Halt];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 a))
#[test]
fn test39_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// 4
#[test]
fn test4_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 (+ a b)))
#[test]
fn test40_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(7)), Op::Halt];
    let expected = Object::Number(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 (let1 c 5 (+ a b c))))
#[test]
fn test41_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(12)), Op::Halt];
    let expected = Object::Number(12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3) (b 4)) (let1 c 5 (+ a b c)))
#[test]
fn test42_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(12)), Op::Halt];
    let expected = Object::Number(12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3) (b 4)) (+ (let1 c 5 (+ a b c)) 1))
#[test]
fn test43_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(13)), Op::Halt];
    let expected = Object::Number(13);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 a 4 a))
#[test]
fn test44_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (set! a (+ a 1)) (+ a 1))
#[test]
fn test45_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ConstantPush(Object::Number(3)),
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::PushConstant(Object::Number(1)),
        Op::NumberAdd,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 (set! b a) b))
#[test]
fn test46_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(4)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(3)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 2) (b 3)) a)
#[test]
fn test47_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(2)), Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b (lambda () a) (b)))
#[test]
fn test48_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3)) (let ((b (lambda () a))) (b)))
#[test]
fn test49_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (if #f #f #t)
#[test]
fn test5_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::True),
        Op::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0) (b 1) (c 2)) c)
#[test]
fn test50_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(2)), Op::Halt];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 1 (let1 b 2 (let1 c a (+ a b c))))
#[test]
fn test51_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3)) a)
#[test]
fn test52_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 3) (b 4)) (+ a b))
#[test]
fn test53_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(7)), Op::Halt];
    let expected = Object::Number(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let* ((a 3) (b (+ a 1))) b)
#[test]
fn test54_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(4)), Op::Halt];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 (let1 c (lambda () b) (set! a c))) (a))
#[test]
fn test55_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(Object::Number(3)),
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(1),
        Op::ReferLocalPush(0),
        Op::Display(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(4)),
        Op::Return(0),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::AssignFree(0),
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
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0) (b 1)) (let ((c (lambda () b))) (c)))
#[test]
fn test56_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0) (b 1)) ((lambda () b)))
#[test]
fn test57_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0) (b 1)) (let ((c (lambda () (set! b 3) b))) (c)))
#[test]
fn test58_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(1)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(3)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a 0) (b 1)) (let1 c (lambda () (set! b 3) b) (c)))
#[test]
fn test59_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(1)),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(3)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) 3) 4)
#[test]
fn test6_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let1 a 100 (let1 c (let1 d (lambda () a) d) (c)))
#[test]
fn test60_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::LetFrame(1),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(100)),
        Op::Return(0),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::PushEnter(1),
        Op::Frame(2),
        Op::ReferLocalCall(0, 0),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(100);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (letrec ((a 1) (b (lambda () a))) (b))
#[test]
fn test61_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(1)), Op::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0))
#[test]
fn test62_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(4),
        Op::ConstantPush(Object::Number(0)),
        Op::Enter(1),
        Op::ReferLocalPushConstant(0, Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::LocalJmp(5),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::Shiftj(1, 1, -1),
        Op::LocalJmp(-7),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(10);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '())) (let ((G68 (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (set! a G68) (a 0)))
#[test]
fn test63_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(3),
        Op::ConstantPush(Object::Nil),
        Op::Box(0),
        Op::Enter(1),
        Op::LetFrame(2),
        Op::ReferLocalPush(0),
        Op::ReferLocalPush(0),
        Op::Display(2),
        Op::ReferLocalPush(0),
        Op::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocalPushConstantBranchNotGe(0, Object::Number(1000), 3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::AssignFree(0),
        Op::Frame(5),
        Op::ConstantPush(Object::Number(0)),
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
    let expected = Object::Number(1000);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (letrec ((a (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (a 0))
#[test]
fn test64_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(4),
        Op::ConstantPush(Object::Number(0)),
        Op::Enter(1),
        Op::ReferLocalPushConstantBranchNotGe(0, Object::Number(1000), 3),
        Op::ReferLocal(0),
        Op::LocalJmp(5),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::Shiftj(1, 1, -1),
        Op::LocalJmp(-6),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(1000);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (set! a 1000) a) '())
#[test]
fn test65_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Nil),
        Op::Box(0),
        Op::Enter(1),
        Op::Constant(Object::Number(1000)),
        Op::AssignLocal(0),
        Op::ReferLocal(0),
        Op::Indirect,
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Number(1000);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (set! a (lambda (i) (if (= i 20) i (a (+ i 1))))) (a 0)) '())
#[test]
fn test66_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(2),
        Op::ConstantPush(Object::Nil),
        Op::Box(0),
        Op::Enter(1),
        Op::ReferLocalPush(0),
        Op::Closure {
            size: 11,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        Op::ReferLocalPushConstant(0, Object::Number(20)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::Return(1),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::ReferFree(0),
        Op::Indirect,
        Op::TailCall(1, 1),
        Op::Return(1),
        Op::AssignLocal(0),
        Op::Frame(5),
        Op::ConstantPush(Object::Number(0)),
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
    let expected = Object::Number(20);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (or #f 3 4)
#[test]
fn test67_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::False),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (define a 3)
#[test]
fn test68_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::DefineGlobal(vm.gc.intern("a")),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// a
#[test]
fn test69_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::ReferGlobal(vm.gc.intern("a")), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (if 3 7 5)) 6)
#[test]
fn test7_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(7)),
        Op::Halt,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 3 4)
#[test]
fn test70_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::NumberEqual,
        Op::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 3 3 3)
#[test]
fn test71_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::BranchNotNumberEqual(4),
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (= 3 4 5)
#[test]
fn test72_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::ConstantPush(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::BranchNotNumberEqual(4),
        Op::ConstantPush(Object::Number(4)),
        Op::Constant(Object::Number(5)),
        Op::NumberEqual,
        Op::Halt,
        Op::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda (a) (lambda () a)) 101))
#[test]
fn test73_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(5),
        Op::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::Constant(Object::Number(101)),
        Op::Return(0),
        Op::Call(0),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(101);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda (a) (lambda (b) (+ a b))) 101) 1)
#[test]
fn test74_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Frame(8),
        Op::ConstantPush(Object::Number(1)),
        Op::Closure {
            size: 5,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        Op::ConstantPush(Object::Number(101)),
        Op::ReferLocal(0),
        Op::NumberAdd,
        Op::Return(1),
        Op::Call(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (null? '())
#[test]
fn test75_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Nil), Op::NullP, Op::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (null? 3)
#[test]
fn test76_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::NullP, Op::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 1 2)
#[test]
fn test77_optimized() {
    let mut vm = Vm::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Cons,
        Op::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 1 (cons 2 '()))
#[test]
fn test78_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);

    let ops = vec![
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::Constant(Object::Nil),
        Op::Cons,
        Op::Cons,
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin 1 2 3)
#[test]
fn test79_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(1)),
        Op::Constant(Object::Number(2)),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () 3))
#[test]
fn test8_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (set! a 4) a))
#[test]
fn test80_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(4)),
        Op::AssignGlobal(vm.gc.intern("a")),
        Op::ReferGlobal(vm.gc.intern("a")),
        Op::Halt,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () ((lambda () 3))))
#[test]
fn test81_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () ((lambda (x) x) 3)))
#[test]
fn test82_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (y) ((lambda (x) x) 3)) 4)
#[test]
fn test83_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (let1 a 1 ((lambda () 3)))))
#[test]
fn test84_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (let1 b 2 (let1 a 1 ((lambda () 3))))))
#[test]
fn test85_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (if 3 ((lambda () 3)))))
#[test]
fn test86_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(3)),
        Op::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (if ((lambda () 3)) 4 5)))
#[test]
fn test87_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::Constant(Object::Number(3)),
        Op::Constant(Object::Number(4)),
        Op::Halt,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b))))))
#[test]
fn test88_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(5),
        Op::ConstantPush(Object::Number(0)),
        Op::Enter(1),
        Op::ReferLocalPushConstant(0, Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::LocalJmp(7),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::Constant(Object::Number(0)),
        Op::NumberAddPush,
        Op::Shiftj(1, 1, -1),
        Op::LocalJmp(-9),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(10);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b))))))
#[test]
fn test89_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(5),
        Op::ConstantPush(Object::Number(0)),
        Op::Enter(1),
        Op::ReferLocalPushConstant(0, Object::Number(10)),
        Op::BranchNotNumberEqual(3),
        Op::ReferLocal(0),
        Op::LocalJmp(7),
        Op::ReferLocalPushConstant(0, Object::Number(1)),
        Op::NumberAddPush,
        Op::Constant(Object::Number(0)),
        Op::NumberAddPush,
        Op::Shiftj(1, 1, -1),
        Op::LocalJmp(-9),
        Op::Leave(1),
        Op::Halt,
        Op::Nop,
        Op::Nop,
        Op::Nop,
    ];
    let expected = Object::Number(10);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) a) 101)
#[test]
fn test9_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(101)), Op::Halt];
    let expected = Object::Number(101);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (define d (lambda (x y z) (+ x y z))) (d 1 2 3)))
#[test]
fn test90_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(6)), Op::Halt];
    let expected = Object::Number(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (define b (lambda () 3)) (b)))
#[test]
fn test91_optimized() {
    let mut vm = Vm::new();

    let ops = vec![Op::Constant(Object::Number(3)), Op::Halt];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda a a) 1 2 3)
#[test]
fn test92_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(1)),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::List(3),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3)
#[test]
fn test93_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::List(2),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3 4)
#[test]
fn test94_optimized() {
    let mut vm = Vm::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(3), Object::Number(4)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(2)),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Number(4)),
        Op::List(3),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b . c) c) 1 2 3 4)
#[test]
fn test95_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(3), Object::Number(4)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(3)),
        Op::ConstantPush(Object::Number(4)),
        Op::List(2),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c . d) d) 1 2 3 4)
#[test]
fn test96_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(4)),
        Op::List(1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c . d) d) 1 2 3)
#[test]
fn test97_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::List(0),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Nil;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda a a))
#[test]
fn test98_optimized() {
    let mut vm = Vm::new();

    let ops = vec![
        Op::LetFrame(1),
        Op::List(0),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = Object::Nil;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda a a) 1)
#[test]
fn test99_optimized() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Number(1)]);

    let ops = vec![
        Op::LetFrame(1),
        Op::ConstantPush(Object::Number(1)),
        Op::List(1),
        Op::PushEnter(1),
        Op::ReferLocal(0),
        Op::Leave(1),
        Op::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}
