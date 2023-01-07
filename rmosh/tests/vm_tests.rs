use rmosh::{
    self,
    equal::Equal,
    gc::Gc,
    objects::{Closure, Object, Pair, Procedure, SString, Symbol, Vector},
    op::OpOld,
    vm::VmOld,
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
    let mut vm = VmOld::new();
    let ops = [
        OpOld::Constant(Object::Number(9)),
        OpOld::DefineGlobal(vm.gc.intern("a")),
        OpOld::ReferGlobal(vm.gc.intern("a")),
        OpOld::Halt,
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

fn test_ops_with_size(vm: &mut VmOld, ops: Vec<OpOld>, expected: Object, expected_heap_diff: usize) {
    // Keep reference so that it won't be freed.
    vm.expected = expected;

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

fn test_ops_with_size_as_str(vm: &mut VmOld, ops: Vec<OpOld>, expected: &str, expected_heap_diff: usize) {
    let ret = vm.run(ops.as_ptr(), ops.len());
    vm.ac = Object::Unspecified;
    vm.mark_and_sweep();
    assert_eq!(ret.to_string(), expected);
    assert_eq!(vm.gc.bytes_allocated(), SIZE_OF_MIN_VM + expected_heap_diff);
}

#[test]
fn test_vm_alloc_many_pairs() {
    let mut vm = VmOld::new();
    let mut ops = vec![];

    for _ in 0..100 {
        ops.push(OpOld::Constant(Object::Number(99)));
        ops.push(OpOld::Push);
        ops.push(OpOld::Constant(Object::Number(101)));
        ops.push(OpOld::Cons);
    }
    ops.push(OpOld::Halt);
    let before_size = vm.gc.bytes_allocated();
    vm.run(&ops[..][0] as *const OpOld, ops.len());
    vm.mark_and_sweep();
    let after_size = vm.gc.bytes_allocated();
    assert_eq!(after_size - before_size, SIZE_OF_MIN_VM + SIZE_OF_PAIR);
}

// All ops in the following tests are generated in data/.

#[test]
fn test_call0() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_call1() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(10),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_call2() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(12),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_if0() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}
#[test]
fn test_if1() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_let0() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(0), 0);
}

#[test]
fn test_let1() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_nested_let0() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_nested_let1() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(4),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
}

#[test]
fn test_and() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_if2() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::False),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::True),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test0() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test2() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test3() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test4() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test5() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::False),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::True),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test6() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test7() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(6)),
        OpOld::Push,
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(7)),
        OpOld::Return(1),
        OpOld::Constant(Object::Number(5)),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test8() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test9() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(101)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

#[test]
fn test_test10() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        OpOld::Frame(7),
        OpOld::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(102)),
        OpOld::Return(0),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

#[test]
fn test_test11() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(101)),
        OpOld::Push,
        OpOld::Frame(7),
        OpOld::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(102)),
        OpOld::Return(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

#[test]
fn test_test12() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(103)),
        OpOld::Push,
        OpOld::Frame(7),
        OpOld::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(103), 0);
}

#[test]
fn test_test13() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

#[test]
fn test_test14() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Box(0),
        OpOld::Constant(Object::Number(12)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test15() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(14),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Box(0),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 4,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::Constant(Object::Number(101)),
        OpOld::AssignFree(0),
        OpOld::Return(0),
        OpOld::TailCall(0, 1),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

#[test]
fn test_test16() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(24),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Closure {
            size: 18,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test17() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::Constant(Object::Number(5)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test18() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test19() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
}

#[test]
fn test_test20() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL);
}

#[test]
fn test_test21() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test22() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test23() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(17), 0);
}

#[test]
fn test_test24() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test25() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-5)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test26() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-5)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(-2)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test27_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::Constant(Object::Symbol(vm.gc.intern("b"))),
        OpOld::Cons,
        OpOld::Halt,
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
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Car,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test29() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Cdr,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test30() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cadr,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test31() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

#[test]
fn test_test32() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

#[test]
fn test_test33() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test34() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test35() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test36() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::ReferLocal(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test37() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test38() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test39() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test40() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test41() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(4),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test42() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(12), 0);
}

#[test]
fn test_test43() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(13), 0);
}

#[test]
fn test_test44() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test45() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

#[test]
fn test_test46() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferFree(0),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test47() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test48() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test49() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test50() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(3),
        OpOld::ReferLocal(2),
        OpOld::Leave(3),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(2), 0);
}

#[test]
fn test_test51() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(4),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test52() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test53() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(7), 0);
}

#[test]
fn test_test54() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test55() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Frame(4),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

#[test]
fn test_test56() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test57() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::Frame(7),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Leave(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

#[test]
fn test_test58() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(2),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::AssignFree(0),
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test59() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(2),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::AssignFree(0),
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

#[test]
fn test_test60() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(100)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(3),
        OpOld::ReferLocal(0),
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(100), 0);
}

// (letrec ((a 1) (b (lambda () a))) (b)) => 1
#[test]
fn test_test61() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(0),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(1),
        OpOld::Box(0),
        OpOld::Enter(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 4,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Return(0),
        OpOld::AssignLocal(1),
        OpOld::Frame(4),
        OpOld::ReferLocal(1),
        OpOld::Indirect,
        OpOld::Call(0),
        OpOld::Leave(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0)) => 10
#[test]
fn test_test62() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// (let ((a '())) (let ((G68 (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (set! a G68) (a 0))) => 1000
#[test]
fn test_test63() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1000)),
        OpOld::BranchNotGe(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// (letrec ((a (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (a 0)) => 1000
#[test]
fn test_test64() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1000)),
        OpOld::BranchNotGe(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// ((lambda (a) (set! a 1000) a) '()) => 1000
#[test]
fn test_test65() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Box(0),
        OpOld::Constant(Object::Number(1000)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1000), 0);
}

// ((lambda (a) (set! a (lambda (i) (if (= i 20) i (a (+ i 1))))) (a 0)) '()) => 20
#[test]
fn test_test66() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(31),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Closure {
            size: 27,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Box(0),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(20)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(20), 0);
}

// (define a 3) => 3
#[test]
fn test_test68() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::DefineGlobal(vm.gc.intern("a")),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), SIZE_OF_SYMBOL);
}
// (= 3 4) => #f
#[test]
fn test_test70() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (= 3 3 3) => #t
#[test]
fn test_test71() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotNumberEqual(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberEqual,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (= 3 4 5) => #f
#[test]
fn test_test72() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::BranchNotNumberEqual(5),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::NumberEqual,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (((lambda (a) (lambda () a)) 101)) => 101
#[test]
fn test_test73() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(101)),
        OpOld::Push,
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Return(0),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(101), 0);
}

// (((lambda (a) (lambda (b) (+ a b))) 101) 1) => 102
#[test]
fn test_test74() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(18),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Frame(14),
        OpOld::Constant(Object::Number(101)),
        OpOld::Push,
        OpOld::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(102), 0);
}

// (null? '()) => #t
#[test]
fn test_test75() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Nil), OpOld::NullP, OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (null? 3) => #f
#[test]
fn test_test76() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::NullP, OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (cons 1 2) => (1 . 2)
#[test]
fn test_test77_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Halt,
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
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
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
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (set! a 4) a)) => 4
#[test]
fn test_test80() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Closure {
            size: 5,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(4)),
        OpOld::AssignGlobal(vm.gc.intern("a")),
        OpOld::ReferGlobal(vm.gc.intern("a")),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), SIZE_OF_SYMBOL);
}

// ((lambda () ((lambda () 3)))) => 3
#[test]
fn test_test81() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(8),
        OpOld::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::TailCall(0, 0),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () ((lambda (x) x) 3))) => 3
#[test]
fn test_test82() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(10),
        OpOld::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::TailCall(1, 0),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda (y) ((lambda (x) x) 3)) 4) => 3
#[test]
fn test_test83() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(12),
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 8,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (let1 a 1 ((lambda () 3))))) => 3
#[test]
fn test_test84() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Closure {
            size: 11,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::TailCall(0, 3),
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (let1 b 2 (let1 a 1 ((lambda () 3)))))) => 3
#[test]
fn test_test85() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(18),
        OpOld::Closure {
            size: 16,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::TailCall(0, 6),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (if 3 ((lambda () 3))))) => 3
#[test]
fn test_test86() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(12),
        OpOld::Closure {
            size: 10,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(6),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::TailCall(0, 0),
        OpOld::Return(0),
        OpOld::Undef,
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda () (if ((lambda () 3)) 4 5))) => 4
#[test]
fn test_test87() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Closure {
            size: 11,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(4)),
        OpOld::Return(0),
        OpOld::Constant(Object::Number(5)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(4), 0);
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
#[test]
fn test_test88() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 41,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::LetFrame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(4),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(2),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 7),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// (let loop ((i 0)) (if (= i 10) i (let1 a 1 (let1 b 0 (loop (+ i a b)))))) => 10
#[test]
fn test_test89() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 41,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::LetFrame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(4),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferFree(2),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 7),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(10), 0);
}

// ((lambda () (define d (lambda (x y z) (+ x y z))) (d 1 2 3))) => 6
#[test]
fn test_test90() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(29),
        OpOld::Closure {
            size: 27,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(3),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::AssignLocal(0),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::TailCall(3, 3),
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(6), 0);
}

// ((lambda () (define b (lambda () 3)) (b))) => 3
#[test]
fn test_test91() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(17),
        OpOld::Closure {
            size: 15,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(0),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::TailCall(0, 3),
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// ((lambda a a) 1 2 3) => (1 2 3)
#[test]
fn test_test92() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// ((lambda (a . b) b) 1 2 3) => (2 3)
#[test]
fn test_test93() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
}

// ((lambda (a . b) b) 1 2 3 4) => (2 3 4)
#[test]
fn test_test94() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3 4)", 0);
}

// ((lambda (a b . c) c) 1 2 3 4) => (3 4)
#[test]
fn test_test95() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 3,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(2),
        OpOld::Return(3),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 4)", 0);
}

// ((lambda (a b c . d) d) 1 2 3 4) => (4)
#[test]
fn test_test96() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 4,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(3),
        OpOld::Return(4),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(4)", 0);
}

// ((lambda (a b c . d) d) 1 2 3) => ()
#[test]
fn test_test97() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 4,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(3),
        OpOld::Return(4),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Nil, 0);
}

// ((lambda a a)) => ()
#[test]
fn test_test98() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Nil, 0);
}

// ((lambda a a) 1) => (1)
#[test]
fn test_test99() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", 0);
}

// (when #t 1 2 34) => 34
#[test]
fn test_test100() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Test(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(34)),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(34), 0);
}

// (not 3) => #f
#[test]
fn test_test101() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Not, OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (unless #f 1 2 48) => 48
#[test]
fn test_test102() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Undef,
        OpOld::LocalJmp(4),
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(48)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(48), 0);
}

// (and 3 4 5) => 5
#[test]
fn test_test103() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(4),
        OpOld::Constant(Object::Number(4)),
        OpOld::Test(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(5), 0);
}

// (let1 a 0 (and (set! a (+ a 1))) a) => 1
#[test]
fn test_test104() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (let1 a 0 (or (set! a (+ a 1))) a) => 1
#[test]
fn test_test105() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (and 3 #f 5) => #f
#[test]
fn test_test106() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(4),
        OpOld::Constant(Object::False),
        OpOld::Test(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (or 3 4 5) => 3
#[test]
fn test_test107() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(2),
        OpOld::LocalJmp(5),
        OpOld::Constant(Object::Number(4)),
        OpOld::Test(2),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(5)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (or #f #f #f) => #f
#[test]
fn test_test108() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(2),
        OpOld::LocalJmp(3),
        OpOld::Constant(Object::False),
        OpOld::Test(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (> 4 3) => #t
#[test]
fn test_test109() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGt,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (> 4 3 2) => #t
#[test]
fn test_test110() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (> 4 3 1 2) => #f
#[test]
fn test_test111() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(9),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::BranchNotGt(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (>= 3 3 3) => #t
#[test]
fn test_test112() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGe(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (>= 4 3 3) => #t
#[test]
fn test_test113() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGe(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (>= 4 3) => #t
#[test]
fn test_test114() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 2) => #t
#[test]
fn test_test115() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberLt,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 2 3) => #t
#[test]
fn test_test116() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLt(5),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (< 1 5 3) => #f
#[test]
fn test_test117() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::BranchNotLt(5),
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (<= 1 2) => #t
#[test]
fn test_test118() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberLe,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 2 3) => #t
#[test]
fn test_test119() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLe(5),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 3 3) => #t
#[test]
fn test_test120() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotLe(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (<= 1 5 3) => #f
#[test]
fn test_test121() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::BranchNotLe(5),
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (eq? #t #t) => #t
#[test]
fn test_test122() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Push,
        OpOld::Constant(Object::True),
        OpOld::Eq,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (eq? #t #f) => #f
#[test]
fn test_test123() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Push,
        OpOld::Constant(Object::False),
        OpOld::Eq,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (eq? 'a 'a) => #t
#[test]
fn test_test124() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Eq,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
}

// (eq? 'a 'b) => #f
#[test]
fn test_test125() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::Constant(Object::Symbol(vm.gc.intern("b"))),
        OpOld::Eq,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::False, SIZE_OF_SYMBOL * 2);
}

// (pair? (cons 1 2)) => #t
#[test]
fn test_test126() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::PairP,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, 0);
}

// (pair? 3) => #f
#[test]
fn test_test127() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::PairP, OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (symbol? 'a) => #t
#[test]
fn test_test128() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::SymbolP,
        OpOld::Halt,
    ];
    test_ops_with_size(&mut vm, ops, Object::True, SIZE_OF_SYMBOL);
}

// (symbol? 3) => #f
#[test]
fn test_test129() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::SymbolP, OpOld::Halt];
    test_ops_with_size(&mut vm, ops, Object::False, 0);
}

// (cond (#f 1) (#t 3)) => 3
#[test]
fn test_test130() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(1)),
        OpOld::LocalJmp(6),
        OpOld::Constant(Object::True),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond (#f 1) (#f 2) (else 3)) => 3
#[test]
fn test_test131() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(1)),
        OpOld::LocalJmp(6),
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond (#t 3) (#f 2) (else 1)) => 3
#[test]
fn test_test132() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(3)),
        OpOld::LocalJmp(6),
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (cond ((cons 1 2) => car) (#f 2) (else 3)) => 1
#[test]
fn test_test133() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Test(7),
        OpOld::Frame(11),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::LocalJmp(6),
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(1), 0);
}

// (let ((a 0)) `(,a 4 5)) => (0 4 5)
#[test]
fn test_test134() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 4 5)", 0);
}

// (let ((a '(1 2 3))) `(,a 4 5)) => ((1 2 3) 4 5)
#[test]
fn test_test135() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4 5)", 0);
}

// (let ((a '(1 2 3))) `(,@a 4 5)) => (1 2 3 4 5)
#[test]
fn test_test136() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(4), Object::Number(5))),
        OpOld::Append2,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4 5)", 0);
}

// (let ((name 'a)) `(list ,name ',name)) => (list a 'a)
#[test]
fn test_test137() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::Constant(Object::Symbol(vm.gc.intern("a"))),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(Object::Symbol(vm.gc.intern("list"))),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Symbol(vm.gc.intern("quote"))),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list a 'a)", SIZE_OF_SYMBOL * 3);
}

// `(list ,(+ 1 2) 4) => (list 3 4)
#[test]
fn test_test138() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Symbol(vm.gc.intern("list"))),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list 3 4)", SIZE_OF_SYMBOL);
}

// (let ((a '(1 2 3))) `(1 . ,a)) => (1 1 2 3)
#[test]
fn test_test139() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 1 2 3)", 0);
}

// (let ((a '(1 2 3))) `,a) => (1 2 3)
#[test]
fn test_test140() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(,@a)) => (1 2 3)
#[test]
fn test_test141() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(0 ,@a)) => (0 1 2 3)
#[test]
fn test_test142() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 1 2 3)", 0);
}

// (let ((a '(1 2 3))) `(0 ,a 4)) => (0 (1 2 3) 4)
#[test]
fn test_test143() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(0 (1 2 3) 4)", 0);
}

// (let ((a '(1 2 3))) `(,@a 4)) => (1 2 3 4)
#[test]
fn test_test144() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Append2,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
}

// (let ((a '(1 2 3))) `((,@a) 4)) => ((1 2 3) 4)
#[test]
fn test_test145() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((1 2 3) 4)", 0);
}

// (let ((a '(1 2 3))) `((,a) 4)) => (((1 2 3)) 4)
#[test]
fn test_test146() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(((1 2 3)) 4)", 0);
}

// `b => b
#[test]
fn test_test147_modified() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Symbol(vm.gc.intern("b"))), OpOld::Halt];
    let obj = vm.gc.symbol_intern("b");
    test_ops_with_size(&mut vm, ops, obj, SIZE_OF_SYMBOL);
}

// (list 1 2 3) => (1 2 3)
#[test]
fn test_test148() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (aif (+ 1 2) it #f) => 3
#[test]
fn test_test149() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Test(2),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string-length abc) => 3
#[test]
fn test_test150() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("abc")),
        OpOld::Push,
        OpOld::ReferFree(19),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string-length あいう) => 3
#[test]
fn test_test151() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("あいう")),
        OpOld::Push,
        OpOld::ReferFree(19),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size(&mut vm, ops, Object::Number(3), 0);
}

// (string->symbol abc) => abc
#[test]
fn test_test152_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("abc")),
        OpOld::Push,
        OpOld::ReferFree(20),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let s = vm.gc.symbol_intern("abc");
    test_ops_with_size(&mut vm, ops, s, SIZE_OF_SYMBOL);
}

// (number->string 123) => 123
#[test]
fn test_test153() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(123)),
        OpOld::Push,
        OpOld::ReferFree(25),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123\"", 0);
}

// (begin (define (proc1 . a) a) (proc1 1 2 3 4)) => (1 2 3 4)
#[test]
fn test_test154_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::DefineGlobal(vm.gc.intern("proc1")),
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("proc1")),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// ((lambda (a . b) b) 1 2 3) => (2 3)
#[test]
fn test_test155() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", 0);
}

// ((lambda (a . b) a) 1 2 3 4 5) => 1
#[test]
fn test_test156() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(15),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(2),
        OpOld::Call(5),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a . b) b) 1 2 3 4 5) => (2 3 4 5)
#[test]
fn test_test157() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(15),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Call(5),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2 3 4 5)", 0);
}

// ((lambda (a b c d . e) e) 1 2 3 4) => ()
#[test]
fn test_test158() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(4),
        OpOld::Return(5),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Nil;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) a) 1 2 3 4) => 1
#[test]
fn test_test159() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(5),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) b) 1 2 3 4) => 2
#[test]
fn test_test160() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(5),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda (a b c d . e) c) 1 2 3 4) => 3
#[test]
fn test_test161() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 5,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(2),
        OpOld::Return(5),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (append '(1 2) '(3 4)) => (1 2 3 4)
#[test]
fn test_test163() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(3), Object::Number(4))),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 4)", 0);
}

// (append) => ()
#[test]
fn test_test164() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Nil), OpOld::Halt];
    let expected = Object::Nil;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (define x 3) x) => 3
#[test]
fn test_test165_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::DefineGlobal(vm.gc.intern("x")),
        OpOld::ReferGlobal(vm.gc.intern("x")),
        OpOld::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (begin (define (hoge . a) a) (hoge 1 2 3)) => (1 2 3)
#[test]
fn test_test166() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::DefineGlobal(vm.gc.intern("hoge")),
        OpOld::Frame(9),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("hoge")),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// (begin (define (hige a . b) b) (hige 1 2 3)) => (2 3)
#[test]
fn test_test167() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("hige")),
        OpOld::Frame(9),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("hige")),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    // This register a closure globally and increase size.
    test_ops_with_size_as_str(&mut vm, ops, "(2 3)", SIZE_OF_CLOSURE + SIZE_OF_SYMBOL);
}

// (apply (lambda a a) '(3 2)) => (3 2)
#[test]
fn test_test168() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(3), Object::Number(2))),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 2)", 0);
}

// (let ((a 3)) 3 2 1) => 1
#[test]
fn test_test170() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(1)),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (make-string 3) =>
#[test]
fn test_test171() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(17),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"   \"", 0);
}

// (make-string 3 #\c) => "ccc"
#[test]
fn test_test172() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::ReferFree(17),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"ccc\"", 0);
}

// (apply car '((3))) => 3
#[test]
fn test_test173_modified() {
    let mut vm = VmOld::new();
    let list = vm.gc.list1(Object::Number(3));
    let ops = vec![
        OpOld::Frame(7),
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::Constant(vm.gc.list1(list)),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a) a) '(3)) => 3
#[test]
fn test_test174() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a b) (+ a b)) '(5 2)) => 7
#[test]
fn test_test175() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(12),
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(5), Object::Number(2))),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(7);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a b c) (+ a b c)) '(5 2 1)) => 8
#[test]
fn test_test176() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(15),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(5), Object::Number(2), Object::Number(1)),
        ),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(8);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a) (car a)) '((3))) => 3
#[test]
fn test_test177_modified() {
    let mut vm = VmOld::new();
    let list = vm.gc.list1(Object::Number(3));
    let ops = vec![
        OpOld::Frame(10),
        OpOld::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Car,
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.list1(list)),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (apply (lambda (a . b) (+ a (car b))) '(1 2)) => 3
#[test]
fn test_test178() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(13),
        OpOld::Closure {
            size: 7,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::Car,
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (string-append "12" "345" "6") => "123456"
#[test]
fn test_test179() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(vm.gc.new_string("12")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("345")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("6")),
        OpOld::Push,
        OpOld::ReferFree(22),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123456\"", 0);
}

// (string? "hige") => #t
#[test]
fn test_test181() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("hige")),
        OpOld::Push,
        OpOld::ReferFree(31),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p)) => (1 . 3)
#[test]
fn test_test184() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(22),
        OpOld::Closure {
            size: 20,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(2),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::SetCdr,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 3)", 0);
}

// ((lambda () (define q (cons 1 2)) (set-car! q 3) q)) => (3 . 2)
#[test]
fn test_test185() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(22),
        OpOld::Closure {
            size: 20,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(2),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::SetCar,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(3 . 2)", 0);
}

// (begin #f #t) => #t
#[test]
fn test_test186() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::True),
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (vector-length (make-vector 3)) => 3
#[test]
fn test_test187() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::VectorLength,
        OpOld::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let loop ((i 0)) (if (= i 100) (+ i 1) (loop (+ i 1)))) => 101
#[test]
fn test_test188() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 19,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(100)),
        OpOld::BranchNotNumberEqual(6),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(101);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((a 0)) (cond (#t (set! a (+ a 1)) (set! a (+ a 1)) a))) => 2
#[test]
fn test_test189() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::True),
        OpOld::Test(16),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (char? #\あ) => #t
#[test]
fn test_test190() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Char('あ')),
        OpOld::Push,
        OpOld::ReferFree(53),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eq? (list 'a) (list 'a)) => #f
#[test]
fn test_test191() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(1),
        OpOld::Eq,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (let ((x (list 'a))) (eq? x x)) => #t
#[test]
fn test_test192() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFree(89),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL);
}

// (map1 (lambda (x) 2) '(1)) => (2)
#[test]
fn test_test193_modified0() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(9),
        // size was originally 3
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(2)),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("map1")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];

    test_ops_with_size_as_str(&mut vm, ops, "(2)", 0);
}

// (map1 (lambda (s) (string-append s "123")) '("ABC" "DEF")) => ("ABC123" "DEF123")
#[test]
fn test_test193_modified() {
    let mut vm = VmOld::new();
    let abc = vm.gc.new_string("ABC");
    let def = vm.gc.new_string("DEF");
    let ops = vec![
        OpOld::Frame(16),
        OpOld::ReferFree(22),
        OpOld::Push,
        OpOld::Closure {
            size: 8,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("123")),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::TailCall(2, 1),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(abc, def)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("map1")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"ABC123\" \"DEF123\")", 0);
}

// (let1 a '() (let1 G68 (lambda (i) (if (>= i 10000) i (a (+ i 1)))) (set! a G68) (a 0))) => 10000
#[test]
fn test_test194() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 16,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(10000)),
        OpOld::BranchNotGe(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(0),
        OpOld::Frame(6),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(10000);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((p (open-string-input-port "12345"))) (read-char p) (read-char p)) => #\2
#[test]
fn test_test195() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(35),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("12345")),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Char('2');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eof-object? (let ((p (open-string-input-port "1"))) (read-char p) (read-char p))) => #t
#[test]
fn test_test196() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(20),
        OpOld::LetFrame(2),
        OpOld::ReferFree(35),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("1")),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Push,
        OpOld::ReferFree(27),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a)))) => a
#[test]
fn test_test197_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(vm.gc.symbol_intern("b")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(vm.gc.symbol_intern("b")),
        OpOld::LocalJmp(8),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = vm.gc.symbol_intern("a");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (begin (let ((xxy 'a)) (case xxy ((b) 'b) ((c) 'c) (else 3)))) => 3
#[test]
fn test_test198_mofidified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(vm.gc.symbol_intern("b")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(vm.gc.symbol_intern("b")),
        OpOld::LocalJmp(8),
        OpOld::Constant(vm.gc.symbol_intern("c")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(vm.gc.symbol_intern("c")),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (* 2 3 4) => 24
#[test]
fn test_test200() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberMul,
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberMul,
        OpOld::Halt,
    ];
    let expected = Object::Number(24);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (string->number "123") => 123
#[test]
fn test_test201() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("123")),
        OpOld::Push,
        OpOld::ReferFree(21),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(123);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (let ((p (open-string-input-port "123 456"))) (read-char p)) => #\1
#[test]
fn test_test202() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(35),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.new_string("123 456")),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Char('1');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (reverse '(1 2 3 4)) => (4 3 2 1)
#[test]
fn test_test203_modified() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.list4(
            Object::Number(1),
            Object::Number(2),
            Object::Number(3),
            Object::Number(4),
        )),
        OpOld::Push,
        OpOld::ReferFree(26),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(4 3 2 1)", 0);
}

// (string-split "wiki&cmd" #\&) => ("wiki" "cmd")
#[test]
fn test_test204() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.new_string("wiki&cmd")),
        OpOld::Push,
        OpOld::Constant(Object::Char('&')),
        OpOld::Push,
        OpOld::ReferFree(23),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"wiki\" \"cmd\")", 0);
}

// (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1) => "cbc"
#[test]
fn test_test205() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::ReferFree(17),
        OpOld::Call(2),
        OpOld::DefineGlobal(vm.gc.intern("str1")),
        OpOld::Frame(9),
        OpOld::ReferGlobal(vm.gc.intern("str1")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(18),
        OpOld::Call(3),
        OpOld::ReferGlobal(vm.gc.intern("str1")),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    // The string is kept as global variable.
    test_ops_with_size_as_str(&mut vm, ops, "\"cbc\"", SIZE_OF_SYMBOL + SIZE_OF_STRING);
}

// (let* ((a 0) (b (lambda (x y) a))) (b (begin (set! a 1)) (begin (set! a 2)))) => 2
#[test]
fn test_test206() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Closure {
            size: 4,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Return(2),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::Constant(Object::Number(1)),
        OpOld::AssignFree(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::AssignFree(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// #\a => #\a
#[test]
fn test_test207() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Char('a')), OpOld::Halt];
    let expected = Object::Char('a');
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (eof-object? 3) => #f
#[test]
fn test_test208() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(27),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// 102 => 102
#[test]
fn test_test209() {
    let mut vm = VmOld::new();
    let ops = vec![OpOld::Constant(Object::Number(102)), OpOld::Halt];
    let expected = Object::Number(102);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// `(list ,(+ 1 2) 4) => (list 3 4)
#[test]
fn test_test210() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(vm.gc.symbol_intern("list")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(4), Object::Nil)),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list 3 4)", SIZE_OF_SYMBOL);
}

// (let ((name 'a)) `(list ,name ',name)) => (list a 'a)
#[test]
fn test_test211() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Constant(vm.gc.symbol_intern("list")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("quote")),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(list a 'a)", SIZE_OF_SYMBOL * 3);
}

// `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b) => (a 3 4 5 6 b)
#[test]
fn test_test212_modified() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let ops = vec![
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Frame(7),
        OpOld::ReferFree(410),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(4), Object::Number(-5), Object::Number(6)),
        ),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("map1")),
        OpOld::Call(2),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(b, Object::Nil)),
        OpOld::Append2,
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a 3 4 5 6 b)", SIZE_OF_SYMBOL * 2);
}

// (vector? #(3)) => #t
#[test]
fn test_test213() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(vm.gc.new_vector(&vec![Object::Number(3)])),
        OpOld::VectorP,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (begin (define (proc-01) 3) (proc-01)) => 3
#[test]
fn test_test214() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("proc-01")),
        OpOld::Frame(3),
        OpOld::ReferGlobal(vm.gc.intern("proc-01")),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define (add3 a b) (+ a b)) (add3 1 2)) => 3
#[test]
fn test_test215() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("add3")),
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("add3")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define add2 (lambda (a b) (+ a b))) (add2 1 2)) => 3
#[test]
fn test_test216() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("add2")),
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("add2")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1)) => 3
#[test]
fn test_test217() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::DefineGlobal(vm.gc.intern("z")),
        OpOld::ReferGlobal(vm.gc.intern("z")),
        OpOld::Push,
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::VectorSet,
        OpOld::ReferGlobal(vm.gc.intern("z")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::VectorSet,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::Constant(Object::Number(3)),
        OpOld::NullP,
        OpOld::ReferGlobal(vm.gc.intern("z")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::VectorSet,
        OpOld::ReferGlobal(vm.gc.intern("z")),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::VectorRef,
        OpOld::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_VECTOR);
}

// (begin (define (proc-2) (define (rec) 3) (rec)) (proc-2)) => 3
#[test]
fn test_test218() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 15,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(0),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::TailCall(0, 3),
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("proc-2")),
        OpOld::Frame(3),
        OpOld::ReferGlobal(vm.gc.intern("proc-2")),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (begin (define (func2) (define val 4) val) (func2)) => 4
#[test]
fn test_test219() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Closure {
            size: 12,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::LetFrame(0),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(4)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("func2")),
        OpOld::Frame(3),
        OpOld::ReferGlobal(vm.gc.intern("func2")),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL + SIZE_OF_CLOSURE);
}

// (if (values 1 2 3) #t #f) => #t
#[test]
fn test_test220() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Test(2),
        OpOld::Constant(Object::True),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 4 5)) (lambda (a b) b)) => 5
#[test]
fn test_test221() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(8),
        OpOld::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::Values(2),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(5);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c))) => 6
#[test]
fn test_test222() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(10),
        OpOld::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(15),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (call-with-values (lambda () (values 1 2 3)) list) => (1 2 3)
#[test]
fn test_test223() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(89),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Frame(10),
        OpOld::Closure {
            size: 8,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(7),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (call-with-values (lambda () 1) (lambda (x) (+ x 1234))) => 1235
#[test]
fn test_test224() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(12),
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1234)),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(1235);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (receive (a b c) (values 1 2 3) (+ a b c)) => 6
#[test]
fn test_test225() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Receive(3, 0),
        OpOld::Enter(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Leave(3),
        OpOld::Halt,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, 0);
}

// (receive z (values 'x 'y) z) => (x y)
#[test]
fn test_test226() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::Constant(vm.gc.symbol_intern("x")),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("y")),
        OpOld::Values(2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(x y)", SIZE_OF_SYMBOL * 2);
}

// (receive (a . b) (values 'x 'y 'z) b) => (y z)
#[test]
fn test_test227() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(vm.gc.symbol_intern("x")),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("y")),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("z")),
        OpOld::Values(3),
        OpOld::Receive(1, 1),
        OpOld::Enter(2),
        OpOld::ReferLocal(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(y z)", SIZE_OF_SYMBOL * 3);
}

// (receive (a . b) (values 'x 'y 'z) a) => x
#[test]
fn test_test228() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(vm.gc.symbol_intern("x")),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("y")),
        OpOld::Push,
        OpOld::Constant(vm.gc.symbol_intern("z")),
        OpOld::Values(3),
        OpOld::Receive(1, 1),
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    let expected = vm.gc.symbol_intern("x");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (receive x (apply values '(1 2 3)) x) => (1 2 3)
#[test]
fn test_test229() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(110),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Frame(7),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", 0);
}

// (call-with-values (lambda () (values 1 2)) cons) => (1 . 2)
#[test]
fn test_test230() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Frame(8),
        OpOld::Closure {
            size: 6,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Values(2),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(7),
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", 0);
}

// (cons 'a '()) => (a)
#[test]
fn test_test232() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a)", SIZE_OF_SYMBOL);
}

// (cons '(a) '(b c d)) => ((a) b c d)
#[test]
fn test_test233_modified() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");
    let b = vm.gc.symbol_intern("b");
    let c = vm.gc.symbol_intern("c");
    let d = vm.gc.symbol_intern("d");
    let ops = vec![
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Push,
        OpOld::Constant(vm.gc.list3(b, c, d)),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((a) b c d)", SIZE_OF_SYMBOL * 4);
}

// (cons "a" '(b c)) => ("a" b c)
#[test]
fn test_test234_modified() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let c = vm.gc.symbol_intern("c");
    let ops = vec![
        OpOld::Constant(vm.gc.new_string("a")),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(b, c)),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(\"a\" b c)", SIZE_OF_SYMBOL * 2);
}

// (cons 'a 3) => (a . 3)
#[test]
fn test_test235() {
    let mut vm = VmOld::new();
    let ops = vec![
        OpOld::Constant(vm.gc.symbol_intern("a")),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a . 3)", SIZE_OF_SYMBOL);
}

// (cons '(a b) 'c) => ((a b) . c)
#[test]
fn test_test236() {
    let mut vm = VmOld::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(vm.gc.list2(a, b)),
        OpOld::Push,
        OpOld::Constant(c),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "((a b) . c)", SIZE_OF_SYMBOL * 3);
}

// (car '(a b c)) => a
#[test]
fn test_test237() {
    let mut vm = VmOld::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![OpOld::Constant(vm.gc.list3(a, b, c)), OpOld::Car, OpOld::Halt];
    let expected = vm.gc.symbol_intern("a");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (car '((a) b c d)) => (a)
#[test]
fn test_test238() {
    let mut vm = VmOld::new();
    let d = vm.gc.symbol_intern("d");
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");
    let list = vm.gc.list1(a);
    let ops = vec![OpOld::Constant(vm.gc.list4(list, b, c, d)), OpOld::Car, OpOld::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "(a)", SIZE_OF_SYMBOL * 4);
}

// (car '(1 . 2)) => 1
#[test]
fn test_test239() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Number(2))),
        OpOld::Car,
        OpOld::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cdr '((a) b c d)) => (b c d)
#[test]
fn test_test240() {
    let mut vm = VmOld::new();
    let d = vm.gc.symbol_intern("d");
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");
    let list = vm.gc.list1(a);
    let ops = vec![OpOld::Constant(vm.gc.list4(list, b, c, d)), OpOld::Cdr, OpOld::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "(b c d)", SIZE_OF_SYMBOL * 4);
}

// (cdr '(1 . 2)) => 2
#[test]
fn test_test241() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Number(2))),
        OpOld::Cdr,
        OpOld::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (reverse '(a b c)) => (c b a)
#[test]
fn test_test242() {
    let mut vm = VmOld::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.list3(a, b, c)),
        OpOld::Push,
        OpOld::ReferFree(26),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(c b a)", SIZE_OF_SYMBOL * 3);
}

// (equal? 'a 'a) => #t
#[test]
fn test_test244() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::Constant(a),
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (equal? '(a) '(a)) => #t
#[test]
fn test_test245() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (equal? "abc" "abc") => #t
#[test]
fn test_test247() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.new_string("abc")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("abc")),
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? 2 2) => #t
#[test]
fn test_test248() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (equal? (make-vector 5 'a) (make-vector 5 'a)) => #t
#[test]
fn test_test249() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Constant(a),
        OpOld::MakeVector,
        OpOld::Push,
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Constant(a),
        OpOld::MakeVector,
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? 'a 'a) => #t
#[test]
fn test_test250() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![OpOld::Constant(a), OpOld::Push, OpOld::Constant(a), OpOld::Eq, OpOld::Halt];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? '(a) '(a)) => #f
#[test]
fn test_test251() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? (list 'a) (list 'a)) => #f
#[test]
fn test_test252() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Frame(5),
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(1),
        OpOld::Eq,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eq? "a" "a") => #f
#[test]
fn test_test253() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.new_string("a")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("a")),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? "" "") => #f
#[test]
fn test_test254() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.new_string("")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("")),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? '() '()) => #t
#[test]
fn test_test255() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? 2 2) => #t
#[test]
fn test_test256() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? #\A #\A) => #t
#[test]
fn test_test257() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Char('A')),
        OpOld::Push,
        OpOld::Constant(Object::Char('A')),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eq? car car) => #t
#[test]
fn test_test258() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::ReferFree(3),
        OpOld::Eq,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((n (+ 2 3))) (eq? n n)) => #t
#[test]
fn test_test259() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x '(a))) (eq? x x)) => #t
#[test]
fn test_test260() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(vm.gc.cons(a, Object::Nil)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (let ((x '#())) (eq? x x)) => #t
#[test]
fn test_test261() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Constant(vm.gc.new_vector(&vec![])),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((p (lambda (x) x))) (eq? p p)) => #t
#[test]
fn test_test262() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3 4) => -1
#[test]
fn test_test263() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-4)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    let expected = Object::Number(-1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3 4 5) => -6
#[test]
fn test_test264() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-4)),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(-5)),
        OpOld::NumberAdd,
        OpOld::Halt,
    ];
    let expected = Object::Number(-6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- 3) => -3
#[test]
fn test_test265() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(-3)), OpOld::Halt];
    let expected = Object::Number(-3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cond ((> 3 2) 'greater) ((< 3 2) 'less)) => greater
#[test]
fn test_test266() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("less");
    let a = vm.gc.symbol_intern("greater");

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotGt(3),
        OpOld::Constant(a),
        OpOld::LocalJmp(8),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLt(3),
        OpOld::Constant(b),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = vm.gc.symbol_intern("greater");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)) => equal
#[test]
fn test_test267() {
    let mut vm = VmOld::new();
    let c = vm.gc.symbol_intern("equal");
    let b = vm.gc.symbol_intern("less");
    let a = vm.gc.symbol_intern("greater");

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(3),
        OpOld::Constant(a),
        OpOld::LocalJmp(8),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotLt(3),
        OpOld::Constant(b),
        OpOld::LocalJmp(2),
        OpOld::Constant(c),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = vm.gc.symbol_intern("equal");
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (cond ('(1 2 3) => cadr) (else #f)) => 2
#[test]
fn test_test268() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFree(70),
        OpOld::Push,
        OpOld::Display(1),
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Test(6),
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cons 'a 'b) => (a . b)
#[test]
fn test_test27() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::Constant(b),
        OpOld::Cons,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(a . b)", SIZE_OF_SYMBOL * 2);
}

// (or (= 2 2) (> 2 1)) => #t
#[test]
fn test_test273() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotNumberEqual(2),
        OpOld::LocalJmp(5),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or (= 2 2) (< 2 1)) => #t
#[test]
fn test_test274() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotNumberEqual(2),
        OpOld::LocalJmp(5),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or #f #f #f) => #f
#[test]
fn test_test275() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(2),
        OpOld::LocalJmp(3),
        OpOld::Constant(Object::False),
        OpOld::Test(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (or '(b c) (/ 3 0)) => (b c)
#[test]
fn test_test276() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("c");
    let a = vm.gc.symbol_intern("b");

    let ops = vec![
        OpOld::Constant(vm.gc.list2(a, b)),
        OpOld::Test(2),
        OpOld::LocalJmp(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(0)),
        OpOld::NumberDiv,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(b c)", SIZE_OF_SYMBOL * 2);
}

// (not #t) => #f
#[test]
fn test_test277() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::True), OpOld::Not, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not 3) => #f
#[test]
fn test_test278() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Not, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not (list 3)) => #f
#[test]
fn test_test279() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(89),
        OpOld::Call(1),
        OpOld::Not,
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not #f) => #t
#[test]
fn test_test280() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::False), OpOld::Not, OpOld::Halt];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not '()) => #f
#[test]
fn test_test281() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Nil), OpOld::Not, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not (list)) => #f
#[test]
fn test_test282() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ReferFree(89),
        OpOld::Call(0),
        OpOld::Not,
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (not 'nil) => #f
#[test]
fn test_test283() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("nil");

    let ops = vec![OpOld::Constant(a), OpOld::Not, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (let ((x 2) (y 3)) (* x y)) => 6
#[test]
fn test_test284() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberMul,
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) => 35
#[test]
fn test_test285() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::LetFrame(4),
        OpOld::Constant(Object::Number(7)),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberMul,
        OpOld::Leave(2),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    let expected = Object::Number(35);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) => 70
#[test]
fn test_test286() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Enter(2),
        OpOld::LetFrame(4),
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::Constant(Object::Number(7)),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::LetFrame(3),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::Display(2),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::NumberMul,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    let expected = Object::Number(70);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 'a 'a) => #t
#[test]
fn test_test287() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::Constant(a),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (eqv? 'a 'b) => #f
#[test]
fn test_test288() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::Constant(b),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (eqv? 2 2) => #t
#[test]
fn test_test289() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? '() '()) => #t
#[test]
fn test_test290() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 100000000 100000000) => #t
#[test]
fn test_test291() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(100000000)),
        OpOld::Push,
        OpOld::Constant(Object::Number(100000000)),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? (cons 1 2) (cons 1 2)) => #f
#[test]
fn test_test292() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? (lambda () 1) (lambda () 2)) => #f
#[test]
fn test_test293() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(2)),
        OpOld::Return(0),
        OpOld::Eqv,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? 123456789101112 123456789101112) => #t
#[test]
fn test_test294() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(123456789101112)),
        OpOld::Push,
        OpOld::Constant(Object::Number(123456789101112)),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (eqv? #f 'nil) => #f
#[test]
fn test_test295() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("nil");

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Push,
        OpOld::Constant(a),
        OpOld::Eqv,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (digit->integer #\3 10) => 3
#[test]
fn test_test297() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('3')),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::ReferFree(39),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (+) => 0
#[test]
fn test_test298() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(0)), OpOld::Halt];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (*) => 1
#[test]
fn test_test299() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) 1 2 '(3)) => 6
#[test]
fn test_test303() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(19),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) '(1 2 3)) => 6
#[test]
fn test_test304() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(15),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(1), Object::Number(2), Object::Number(3)),
        ),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (a b c) (+ a b c)) 1 '(2 3)) => 6
#[test]
fn test_test305() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(17),
        OpOld::Closure {
            size: 9,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.list2(Object::Number(2), Object::Number(3))),
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(6);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (apply (lambda (x y) (apply y '((3 2)))) `(,car ,cdr)) => (2)
#[test]
fn test_test306() {
    let mut vm = VmOld::new();
    let list = vm.gc.list2(Object::Number(3), Object::Number(2));
    let ops = vec![
        OpOld::Frame(22),
        OpOld::ReferFree(152),
        OpOld::Push,
        OpOld::Closure {
            size: 8,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocal(1),
        OpOld::Push,
        OpOld::Constant(vm.gc.list1(list)),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::TailCall(2, 2),
        OpOld::Return(2),
        OpOld::Push,
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::ReferFree(4),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Push,
        OpOld::ReferFree(152),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(2)", SIZE_OF_SYMBOL * 0);
}

// (/ 6 2) => 3
#[test]
fn test_test307() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(6)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (even? 2) => #t
#[test]
fn test_test309() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferFree(408),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (even? 3) => #f
#[test]
fn test_test310() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(408),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all even? '(2 4 14)) => #t
#[test]
fn test_test313() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::ReferFree(408),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(4), Object::Number(14)),
        ),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("for-all")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (for-all (lambda (n) (and (even? n) n)) '(2 4 14)) => 14
#[test]
fn test_test314() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(17),
        OpOld::ReferFree(408),
        OpOld::Push,
        OpOld::Closure {
            size: 9,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Test(2),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Push,
        OpOld::Constant(
            vm.gc
                .list3(Object::Number(2), Object::Number(4), Object::Number(14)),
        ),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("for-all")),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(14);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (- (/ 1 2) (/ 1 4) (/ 1 4)) => 0
#[test]
fn test_test318() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::NumberSub,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::NumberSub,
        OpOld::Halt,
    ];
    let expected = Object::Number(0);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 3 2) (+ (/ 1 2) 1)) => #t
#[test]
fn test_test319() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 5 2) (+ 1 (/ 1 2) 1)) => #t
#[test]
fn test_test320() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(5)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberAdd,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ (/ 4 2) 1) => 2
#[test]
fn test_test326() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberDiv,
        OpOld::Halt,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> 1 (/ 1 2)) => #t
#[test]
fn test_test329() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGt,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> (/ 1 2) 1) => #f
#[test]
fn test_test330() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberGt,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (> 1 (/ 1 2)) => #t
#[test]
fn test_test331() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGt,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= (/ 1 2) 1) => #t
#[test]
fn test_test333() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberLe,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (>= 1 (/ 1 2)) => #t
#[test]
fn test_test334() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGe,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (>= (/ 1 2) (/ 1 3)) => #t
#[test]
fn test_test335() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::NumberGe,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< (/ 1 2) 1) => #t
#[test]
fn test_test336() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberLt,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< 1 (/ 1 2)) => #f
#[test]
fn test_test337() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberLt,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (< (/ 1 2) (/ 1 3)) => #f
#[test]
fn test_test338() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::NumberLt,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= (/ 1 2) 1) => #t
#[test]
fn test_test339() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberLe,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (<= 1 (/ 1 2)) => #f
#[test]
fn test_test340() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberLe,
        OpOld::Halt,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (= (/ 2 2) 1) => #t
#[test]
fn test_test342() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberEqual,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (/ 3) => 1/3
#[test]
fn test_test358() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::Halt,
    ];
    let expected = Object::Number(1 / 3);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (number? 3) => #t
#[test]
fn test_test369() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div 123 10) => 12
#[test]
fn test_test375() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(123)),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::ReferFree(411),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(12);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div 123 -10) => -12
#[test]
fn test_test376() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(123)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-10)),
        OpOld::Push,
        OpOld::ReferFree(411),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(-12);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div -123 10) => -13
#[test]
fn test_test377() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(-123)),
        OpOld::Push,
        OpOld::Constant(Object::Number(10)),
        OpOld::Push,
        OpOld::ReferFree(411),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(-13);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (div -123 -10) => 13
#[test]
fn test_test378() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Number(-123)),
        OpOld::Push,
        OpOld::Constant(Object::Number(-10)),
        OpOld::Push,
        OpOld::ReferFree(411),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(13);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (string-ref "abc" 2) => #\c
#[test]
fn test_test379() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.new_string("abc")),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferFree(96),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Char('c');
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (list? '(a b c)) => #t
#[test]
fn test_test380() {
    let mut vm = VmOld::new();
    let c = vm.gc.symbol_intern("c");
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.list3(a, b, c)),
        OpOld::Push,
        OpOld::ReferFree(88),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 3);
}

// (list? '()) => #t
#[test]
fn test_test381() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::ReferFree(88),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (list? '(a . b)) => #f
#[test]
fn test_test382() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.cons(a, b)),
        OpOld::Push,
        OpOld::ReferFree(88),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// "abc" => "abc"
#[test]
fn test_test383() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(vm.gc.new_string("abc")), OpOld::Halt];
    test_ops_with_size_as_str(&mut vm, ops, "\"abc\"", SIZE_OF_SYMBOL * 0);
}

// (procedure? car) => #t
#[test]
fn test_test395() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::ReferFree(159),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (procedure? 'car) => #f
#[test]
fn test_test396() {
    let mut vm = VmOld::new();
    let a = vm.gc.symbol_intern("car");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(a),
        OpOld::Push,
        OpOld::ReferFree(159),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 1);
}

// (procedure? (lambda (x) (* x x))) => #t
#[test]
fn test_test397() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(10),
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferLocal(0),
        OpOld::NumberMul,
        OpOld::Return(1),
        OpOld::Push,
        OpOld::ReferFree(159),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\a) => #t
#[test]
fn test_test399() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('a')),
        OpOld::Push,
        OpOld::ReferFree(164),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\c #\b #\a) => #t
#[test]
fn test_test400() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('a')),
        OpOld::Push,
        OpOld::ReferFree(164),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\b) => #t
#[test]
fn test_test401() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(164),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>=? #\b #\c) => #f
#[test]
fn test_test402() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::ReferFree(164),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\a) => #t
#[test]
fn test_test403() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('a')),
        OpOld::Push,
        OpOld::ReferFree(165),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\b) => #f
#[test]
fn test_test404() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(165),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char>? #\b #\c) => #f
#[test]
fn test_test405() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::ReferFree(165),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\a #\b) => #t
#[test]
fn test_test406() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('a')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(162),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\b #\b) => #t
#[test]
fn test_test407() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(162),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<=? #\c #\b) => #f
#[test]
fn test_test408() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(162),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\a #\b) => #t
#[test]
fn test_test409() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('a')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(163),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\b #\b) => #f
#[test]
fn test_test410() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(163),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (char<? #\c #\b) => #f
#[test]
fn test_test411() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Char('c')),
        OpOld::Push,
        OpOld::Constant(Object::Char('b')),
        OpOld::Push,
        OpOld::ReferFree(163),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (cons* 1 2 3 4) => (1 2 3 . 4)
#[test]
fn test_test412() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(11),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(4),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (cons* 1) => 1
#[test]
fn test_test413() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append 1) => 1
#[test]
fn test_test414() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append '(1) 2) => (1 . 2)
#[test]
fn test_test415() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) 3) => (1 2 . 3)
#[test]
fn test_test416() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 . 3)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3)) => (1 2 3)
#[test]
fn test_test417() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test418() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Append2,
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test419() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Append2,
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append '() 1) => 1
#[test]
fn test_test420() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Append2,
        OpOld::Halt,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append '(1) '()) => (1)
#[test]
fn test_test421() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Append2,
        OpOld::Halt,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", SIZE_OF_SYMBOL * 0);
}

// (append! 1) => 1
#[test]
fn test_test422() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append! '(1) 2) => (1 . 2)
#[test]
fn test_test423() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 . 2)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) 3) => (1 2 . 3)
#[test]
fn test_test424() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(Object::Number(3)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 . 3)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3)) => (1 2 3)
#[test]
fn test_test425() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test426() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append! '(1 2) '(3) 4) => (1 2 3 . 4)
#[test]
fn test_test427() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(vm.gc.list2(Object::Number(1), Object::Number(2))),
        OpOld::Push,
        OpOld::Constant(vm.gc.cons(Object::Number(3), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Number(4)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1 2 3 . 4)", SIZE_OF_SYMBOL * 0);
}

// (append! '() 1) => 1
#[test]
fn test_test428() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(1);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (append! '(1) '()) => (1)
#[test]
fn test_test429() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Constant(vm.gc.cons(Object::Number(1), Object::Nil)),
        OpOld::Push,
        OpOld::Constant(Object::Nil),
        OpOld::Push,
        OpOld::ReferFree(176),
        OpOld::Call(2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "(1)", SIZE_OF_SYMBOL * 0);
}

// (string #\1 #\2 #\3) => "123"
#[test]
fn test_test430() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(9),
        OpOld::Constant(Object::Char('1')),
        OpOld::Push,
        OpOld::Constant(Object::Char('2')),
        OpOld::Push,
        OpOld::Constant(Object::Char('3')),
        OpOld::Push,
        OpOld::ReferFree(24),
        OpOld::Call(3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    test_ops_with_size_as_str(&mut vm, ops, "\"123\"", SIZE_OF_SYMBOL * 0);
}

// (hashtable? (make-eq-hashtable)) => #t
#[test]
fn test_test435() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Frame(3),
        OpOld::ReferFree(98),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::ReferFree(201),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (hashtable? '(a . b)) => #f
#[test]
fn test_test436() {
    let mut vm = VmOld::new();
    let b = vm.gc.symbol_intern("b");
    let a = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(vm.gc.cons(a, b)),
        OpOld::Push,
        OpOld::ReferFree(201),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht "my" "apple") (hashtable-set! ht "my" "apple") (hashtable-size ht)) => 2
#[test]
fn test_test438() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(8),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(202),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("my")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("apple")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(3),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("my")),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("apple")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(3),
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-delete! ht 1) (hashtable-ref ht 1 #f)) => #f
#[test]
fn test_test439() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(9),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(203),
        OpOld::Push,
        OpOld::ReferFree(101),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(4),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(3),
        OpOld::Call(3),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(2),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(Object::False),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(3),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-contains? ht 2)) => #f
#[test]
fn test_test441() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(204),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(3),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(2)),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::False;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (hashtable-contains? ht 1)) => #t
#[test]
fn test_test442() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(6),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(204),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(3),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(3),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (let1 ht-copy (hashtable-copy ht) (and (string=? (hashtable-ref ht-copy 1) "one") (not (hashtable-mutable? ht-copy))))) => #t
#[test]
fn test_test446() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(11),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(205),
        OpOld::Push,
        OpOld::ReferFree(101),
        OpOld::Push,
        OpOld::ReferFree(56),
        OpOld::Push,
        OpOld::ReferFree(206),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(6),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(5),
        OpOld::Call(3),
        OpOld::LetFrame(7),
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(4),
        OpOld::Push,
        OpOld::Display(4),
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(1),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(13),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(3),
        OpOld::Call(2),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(2),
        OpOld::Test(7),
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(1),
        OpOld::Not,
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (let1 ht (make-eq-hashtable) (hashtable-set! ht 1 "one") (let1 ht-copy (hashtable-copy ht #t) (and (string=? (hashtable-ref ht-copy 1) "one") (hashtable-mutable? ht-copy)))) => #t
#[test]
fn test_test447() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(12),
        OpOld::ReferFree(100),
        OpOld::Push,
        OpOld::ReferFree(205),
        OpOld::Push,
        OpOld::ReferFree(101),
        OpOld::Push,
        OpOld::ReferFree(56),
        OpOld::Push,
        OpOld::ReferFree(206),
        OpOld::Push,
        OpOld::ReferFree(98),
        OpOld::Push,
        OpOld::Display(6),
        OpOld::Frame(3),
        OpOld::ReferFree(0),
        OpOld::Call(0),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(9),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(5),
        OpOld::Call(3),
        OpOld::LetFrame(8),
        OpOld::ReferFree(3),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Push,
        OpOld::ReferFree(4),
        OpOld::Push,
        OpOld::Display(4),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::True),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::Call(2),
        OpOld::Push,
        OpOld::Enter(1),
        OpOld::Frame(13),
        OpOld::Frame(7),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::Constant(Object::Number(1)),
        OpOld::Push,
        OpOld::ReferFree(3),
        OpOld::Call(2),
        OpOld::Push,
        OpOld::Constant(vm.gc.new_string("one")),
        OpOld::Push,
        OpOld::ReferFree(2),
        OpOld::Call(2),
        OpOld::Test(6),
        OpOld::Frame(5),
        OpOld::ReferLocal(0),
        OpOld::Push,
        OpOld::ReferFree(1),
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0);
}

// (and)
#[test]
fn and_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(2)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::True),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(0)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(6)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (((lambda () (lambda () 102))))
#[test]
fn test10_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(102)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(34)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Test(3),
        OpOld::Undef,
        OpOld::LocalJmp(4),
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(48)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::Constant(Object::Number(5)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::False),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::False),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(6),
        OpOld::ConstantPush(Object::Number(101)),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(102)),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(7),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(1)),
        OpOld::BranchNotGt(4),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGe(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGe(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberGe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberLt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLt(4),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(5)),
        OpOld::BranchNotLt(4),
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberLe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLe(4),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(6),
        OpOld::ConstantPush(Object::Number(103)),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotLe(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(5)),
        OpOld::BranchNotLe(4),
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberLe,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::True),
        OpOld::Constant(Object::True),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::True),
        OpOld::Constant(Object::False),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![OpOld::ConstantPush(sym0), OpOld::Constant(sym0), OpOld::Eq, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![OpOld::ConstantPush(sym0), OpOld::Constant(sym1), OpOld::Eq, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::PairP,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::PairP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![OpOld::Constant(sym0), OpOld::SymbolP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::SymbolP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(10)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::True),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::True),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFreePush(3),
        OpOld::Display(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Test(5),
        OpOld::Frame(6),
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::LocalJmp(3),
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::Number(3)),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(0), Object::Number(4), Object::Number(5)]);
    let list1 = vm.gc.listn(&[Object::Number(4), Object::Number(5)]);

    let ops = vec![
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,a 4 5))
#[test]
fn test135_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0, Object::Number(4), Object::Number(5)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4), Object::Number(5)]);

    let ops = vec![
        OpOld::ConstantPush(list2),
        OpOld::Constant(list3),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 6 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a 4 5))
#[test]
fn test136_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(list1),
        OpOld::Constant(list2),
        OpOld::Append2,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 5 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((name 'a)) `(list ,name ',name))
#[test]
fn test137_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("list");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("quote");
    let list0 = vm.gc.listn(&[sym2, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1, list0]);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::ConstantPush(sym1),
        OpOld::ConstantPush(sym2),
        OpOld::ConstantPush(sym1),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::PushConstant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    vm.expected = list1;
    let expected = vm.expected;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 5 + SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// `(list ,(+ 1 2) 4)
#[test]
fn test138_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("list");
    let list0 = vm.gc.listn(&[sym0, Object::Number(3), Object::Number(4)]);
    let list1 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    vm.expected = expected;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(1 . ,a))
#[test]
fn test139_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (set! a 12) a) 2)
#[test]
fn test14_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(12)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![OpOld::Constant(list1), OpOld::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a))
#[test]
fn test141_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![OpOld::Constant(list1), OpOld::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(0 ,@a))
#[test]
fn test142_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(0 ,a 4))
#[test]
fn test143_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[Object::Number(0), list0, Object::Number(4)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::ConstantPush(list2),
        OpOld::Constant(list3),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 6 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a 4))
#[test]
fn test144_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(list1),
        OpOld::Constant(list2),
        OpOld::Append2,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `((,@a) 4))
#[test]
fn test145_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0, Object::Number(4)]);
    let list2 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        OpOld::ConstantPush(list2),
        OpOld::Constant(list3),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 5 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `((,a) 4))
#[test]
fn test146_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(list3),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::PushConstant(list4),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list2;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 6 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// `b
#[test]
fn test147_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("b");

    let ops = vec![OpOld::Constant(sym0), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(89, 3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (aif (+ 1 2) it #f)
#[test]
fn test149_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Test(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Nil),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(101)),
        OpOld::AssignLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(19, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("あいう");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(19, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("abc");
    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(20, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("123");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(123)),
        OpOld::ReferFreeCall(25, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (begin (define (proc1 . a) a) (proc1 1 2 3 4))
#[test]
fn test154_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::DefineGlobal(vm.gc.intern("proc1")),
        OpOld::Frame(6),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::ReferGlobalCall(vm.gc.intern("proc1"), 4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3)
#[test]
fn test155_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::List(2),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) a) 1 2 3 4 5)
#[test]
fn test156_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::List(4),
        OpOld::Constant(Object::Number(1)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
        Object::Number(5),
    ]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::List(4),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c d . e) e) 1 2 3 4)
#[test]
fn test158_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::List(0),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::List(0), OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(2)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::List(0), OpOld::Constant(Object::Number(2)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::List(0), OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::List(0), OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[
        Object::Number(1),
        Object::Number(2),
        Object::Number(3),
        Object::Number(4),
    ]);
    let list1 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);
    let list2 = vm.gc.listn(&[Object::Number(3), Object::Number(4)]);

    let ops = vec![
        OpOld::ConstantPush(list1),
        OpOld::Constant(list2),
        OpOld::Append2,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (append)
#[test]
fn test164_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Nil), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::DefineGlobal(vm.gc.intern("x")),
        OpOld::ReferGlobal(vm.gc.intern("x")),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::DefineGlobal(vm.gc.intern("hoge")),
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferGlobalCall(vm.gc.intern("hoge"), 3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (begin (define (hige a . b) b) (hige 1 2 3))
#[test]
fn test167_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("hige")),
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferGlobalCall(vm.gc.intern("hige"), 3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0 + SIZE_OF_CLOSURE,
    );
}

// (apply (lambda a a) '(3 2))
#[test]
fn test168_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);
    let list1 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::PushConstant(list1),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (equal? '(1 2 (3)) '(1 2 (3)))
#[test]
fn test169_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[Object::Number(1), Object::Number(2), list0]);
    let list2 = vm.gc.listn(&[Object::Number(3)]);
    let list3 = vm.gc.listn(&[Object::Number(1), Object::Number(2), list2]);

    let ops = vec![
        OpOld::ConstantPush(list1),
        OpOld::Constant(list3),
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::Constant(Object::Number(5)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(1)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("   ");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(17, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("ccc");

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Char('c')),
        OpOld::ReferFreeCall(17, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0]);

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ReferFreePush(3),
        OpOld::ConstantPush(list1),
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);

    let ops = vec![
        OpOld::Frame(7),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(5), Object::Number(2)]);

    let ops = vec![
        OpOld::Frame(9),
        OpOld::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(5), Object::Number(2), Object::Number(1)]);

    let ops = vec![
        OpOld::Frame(11),
        OpOld::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);
    let list1 = vm.gc.listn(&[list0]);

    let ops = vec![
        OpOld::Frame(8),
        OpOld::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Car,
        OpOld::Return(1),
        OpOld::PushConstant(list1),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);

    let ops = vec![
        OpOld::Frame(10),
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: true,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::Car,
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("123456");
    let str1 = vm.gc.new_string("12");
    let str2 = vm.gc.new_string("345");
    let str3 = vm.gc.new_string("6");

    let ops = vec![
        OpOld::Frame(5),
        OpOld::ConstantPush(str1),
        OpOld::ConstantPush(str2),
        OpOld::ConstantPush(str3),
        OpOld::ReferFreeCall(22, 3),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (number? 3)
#[test]
fn test18_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("hige");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(31, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p))
#[test]
fn test184_optimized() {
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(3));

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(3)),
        OpOld::SetCdr,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (define q (cons 1 2)) (set-car! q 3) q))
#[test]
fn test185_optimized() {
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(3), Object::Number(2));

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Undef,
        OpOld::Push,
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(3)),
        OpOld::SetCar,
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin #f #t)
#[test]
fn test186_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::True),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::VectorLength,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Enter(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(100)),
        OpOld::BranchNotNumberEqual(4),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::LocalJmp(5),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Shiftj(1, 1, -1),
        OpOld::LocalJmp(-8),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::True),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Char('あ')),
        OpOld::ReferFreeCall(53, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(89, 1),
        OpOld::PushFrame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(89, 1),
        OpOld::Eq,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFreePush(89),
        OpOld::Display(1),
        OpOld::Frame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::PushEnter(1),
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("ABC123");
    let str1 = vm.gc.new_string("DEF123");
    let str2 = vm.gc.new_string("123");
    let str3 = vm.gc.new_string("ABC");
    let str4 = vm.gc.new_string("DEF");
    let list0 = vm.gc.listn(&[str0, str1]);
    let list1 = vm.gc.listn(&[str3, str4]);

    let ops = vec![
        OpOld::Frame(11),
        OpOld::ReferFreePush(22),
        OpOld::Closure {
            size: 6,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocalPushConstant(0, str2),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::TailCall(2, 1),
        OpOld::Return(1),
        OpOld::PushConstant(list1),
        OpOld::Push,
        OpOld::ReferGlobalCall(vm.gc.intern("map1"), 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 2,
    );
}

// (let1 a '() (let1 G68 (lambda (i) (if (>= i 10000) i (a (+ i 1)))) (set! a G68) (a 0)))
#[test]
fn test194_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ConstantPush(Object::Nil),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocalPush(0),
        OpOld::Display(2),
        OpOld::ReferLocalPush(0),
        OpOld::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocalPushConstantBranchNotGe(0, Object::Number(10000), 3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(0),
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("12345");

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFreePush(35),
        OpOld::Display(1),
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Char('2');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eof-object? (let ((p (open-string-input-port "1"))) (read-char p) (read-char p)))
#[test]
fn test196_optimized() {
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("1");

    let ops = vec![
        OpOld::Frame(15),
        OpOld::LetFrame(2),
        OpOld::ReferFreePush(35),
        OpOld::Display(1),
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Push,
        OpOld::ReferFreeCall(27, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a))))
#[test]
fn test197_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![
        OpOld::ConstantPush(sym1),
        OpOld::Constant(sym0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(sym1),
        OpOld::LocalJmp(7),
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym0),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(sym0),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("c");

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym1),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(sym0),
        OpOld::LocalJmp(7),
        OpOld::ConstantPush(sym2),
        OpOld::Constant(sym1),
        OpOld::BranchNotEqv(3),
        OpOld::Constant(sym2),
        OpOld::LocalJmp(2),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(6)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::True), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(24)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("123");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(21, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Number(123);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((p (open-string-input-port "123 456"))) (read-char p))
#[test]
fn test202_optimized() {
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("123 456");

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFreePush(35),
        OpOld::Display(1),
        OpOld::Frame(3),
        OpOld::ConstantPush(str0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::ReadChar,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = Object::Char('1');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (reverse '(1 2 3 4))
#[test]
fn test203_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::Frame(3),
        OpOld::ConstantPush(list1),
        OpOld::ReferFreeCall(26, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 4 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (string-split "wiki&cmd" #\&)
#[test]
fn test204_optimized() {
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("wiki");
    let str1 = vm.gc.new_string("cmd");
    let str2 = vm.gc.new_string("wiki&cmd");
    let list0 = vm.gc.listn(&[str0, str1]);

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(str2),
        OpOld::ConstantPush(Object::Char('&')),
        OpOld::ReferFreeCall(23, 2),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 2,
    );
}

// (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1)
#[test]
fn test205_optimized() {
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("cbc");

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Char('c')),
        OpOld::ReferFreeCall(17, 2),
        OpOld::DefineGlobal(vm.gc.intern("str1")),
        OpOld::Frame(5),
        OpOld::ReferGlobalPush(vm.gc.intern("str1")),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Char('b')),
        OpOld::ReferFreeCall(18, 3),
        OpOld::ReferGlobal(vm.gc.intern("str1")),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 2,
    );
}

// (let* ((a 0) (b (lambda (x y) a))) (b (begin (set! a 1)) (begin (set! a 2))))
#[test]
fn test206_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::AssignLocal(0),
        OpOld::Constant(Object::Number(2)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Char('a')), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(27, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(102)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("list");
    let list0 = vm.gc.listn(&[sym0, Object::Number(3), Object::Number(4)]);
    let list1 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (let ((name 'a)) `(list ,name ',name))
#[test]
fn test211_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("list");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("quote");
    let list0 = vm.gc.listn(&[sym2, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1, list0]);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::ConstantPush(sym1),
        OpOld::ConstantPush(sym2),
        OpOld::ConstantPush(sym1),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::PushConstant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 5 + SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (vector? #(3))
#[test]
fn test213_optimized() {
    let mut vm = VmOld::new();

    let vec0 = vm.gc.new_vector(&vec![Object::Number(3)]);

    let ops = vec![OpOld::Constant(vec0), OpOld::VectorP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("proc-01")),
        OpOld::Frame(2),
        OpOld::ReferGlobalCall(vm.gc.intern("proc-01"), 0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0 + SIZE_OF_CLOSURE,
    );
}

// (begin (define (add3 a b) (+ a b)) (add3 1 2))
#[test]
fn test215_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("add3")),
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ReferGlobalCall(vm.gc.intern("add3"), 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (begin (define add2 (lambda (a b) (+ a b))) (add2 1 2))
#[test]
fn test216_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 5,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAdd,
        OpOld::Return(2),
        OpOld::DefineGlobal(vm.gc.intern("add2")),
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ReferGlobalCall(vm.gc.intern("add2"), 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1))
#[test]
fn test217_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::DefineGlobal(vm.gc.intern("z")),
        OpOld::ReferGlobalPush(vm.gc.intern("z")),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Constant(Object::Number(1)),
        OpOld::VectorSet,
        OpOld::ReferGlobalPush(vm.gc.intern("z")),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::VectorSet,
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Nil),
        OpOld::MakeVector,
        OpOld::Constant(Object::Number(3)),
        OpOld::NullP,
        OpOld::ReferGlobalPush(vm.gc.intern("z")),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(3)),
        OpOld::VectorSet,
        OpOld::ReferGlobalPush(vm.gc.intern("z")),
        OpOld::Constant(Object::Number(1)),
        OpOld::VectorRef,
        OpOld::Halt,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_VECTOR + SIZE_OF_STRING * 0,
    );
}

// (begin (define (proc-2) (define (rec) 3) (rec)) (proc-2))
#[test]
fn test218_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(3)),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("proc-2")),
        OpOld::Frame(2),
        OpOld::ReferGlobalCall(vm.gc.intern("proc-2"), 0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (begin (define (func2) (define val 4) val) (func2))
#[test]
fn test219_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(4)),
        OpOld::Return(0),
        OpOld::DefineGlobal(vm.gc.intern("func2")),
        OpOld::Frame(2),
        OpOld::ReferGlobalCall(vm.gc.intern("func2"), 0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_CLOSURE + SIZE_OF_STRING * 0,
    );
}

// (+ 4 3)
#[test]
fn test22_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(7)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Test(2),
        OpOld::Constant(Object::True),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFreePush(152),
        OpOld::Display(1),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(5)),
        OpOld::Values(2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(7),
        OpOld::Closure {
            size: 3,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(1),
        OpOld::Return(2),
        OpOld::Push,
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::ReferFreePush(152),
        OpOld::Display(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(11),
        OpOld::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::Push,
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::ReferFreePush(89),
        OpOld::ReferFreePush(152),
        OpOld::Display(2),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(4),
        OpOld::ReferFreePush(1),
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (call-with-values (lambda () 1) (lambda (x) (+ x 1234)))
#[test]
fn test224_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFreePush(152),
        OpOld::Display(1),
        OpOld::Constant(Object::Number(1)),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(8),
        OpOld::Closure {
            size: 4,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPushConstant(0, Object::Number(1234)),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::Push,
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Values(3),
        OpOld::Receive(3, 0),
        OpOld::Enter(3),
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Leave(3),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("x");
    let sym1 = vm.gc.symbol_intern("y");
    let list0 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym1),
        OpOld::Values(2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (receive (a . b) (values 'x 'y 'z) b)
#[test]
fn test227_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("y");
    let sym1 = vm.gc.symbol_intern("z");
    let sym2 = vm.gc.symbol_intern("x");
    let list0 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(sym2),
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym1),
        OpOld::Values(3),
        OpOld::Receive(1, 1),
        OpOld::Enter(2),
        OpOld::ReferLocal(1),
        OpOld::Leave(2),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (receive (a . b) (values 'x 'y 'z) a)
#[test]
fn test228_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("x");
    let sym1 = vm.gc.symbol_intern("y");
    let sym2 = vm.gc.symbol_intern("z");

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(sym0),
        OpOld::ConstantPush(sym1),
        OpOld::Constant(sym2),
        OpOld::Values(3),
        OpOld::Receive(1, 1),
        OpOld::Enter(2),
        OpOld::ReferLocal(0),
        OpOld::Leave(2),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ReferFreePush(110),
        OpOld::ReferFreePush(152),
        OpOld::Display(2),
        OpOld::Frame(4),
        OpOld::ReferFreePush(1),
        OpOld::ConstantPush(list1),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (+ 4 3 10)
#[test]
fn test23_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(17)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ReferFreePush(1),
        OpOld::ReferFreePush(152),
        OpOld::Display(2),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Values(2),
        OpOld::Receive(0, 1),
        OpOld::Enter(1),
        OpOld::Frame(4),
        OpOld::ReferFreePush(1),
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 2),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 'a '())
#[test]
fn test232_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (cons '(a) '(b c d))
#[test]
fn test233_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let sym3 = vm.gc.symbol_intern("d");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[list0, sym1, sym2, sym3]);
    let list2 = vm.gc.listn(&[sym0]);
    let list3 = vm.gc.listn(&[sym1, sym2, sym3]);

    let ops = vec![
        OpOld::ConstantPush(list2),
        OpOld::Constant(list3),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list1;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 5 + SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (cons "a" '(b c))
#[test]
fn test234_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let str0 = vm.gc.new_string("a");
    let str1 = vm.gc.new_string("a");
    let list0 = vm.gc.listn(&[str0, sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![
        OpOld::ConstantPush(str1),
        OpOld::Constant(list1),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 1,
    );
}

// (cons 'a 3)
#[test]
fn test235_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let pair0 = vm.gc.cons(sym0, Object::Number(3));

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (cons '(a b) 'c)
#[test]
fn test236_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);
    let pair0 = vm.gc.cons(list0, sym2);

    let ops = vec![
        OpOld::ConstantPush(list1),
        OpOld::Constant(sym2),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (car '(a b c))
#[test]
fn test237_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);

    let ops = vec![OpOld::Constant(list0), OpOld::Car, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let sym3 = vm.gc.symbol_intern("d");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);
    let list2 = vm.gc.listn(&[list1, sym1, sym2, sym3]);

    let ops = vec![OpOld::Constant(list2), OpOld::Car, OpOld::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (car '(1 . 2))
#[test]
fn test239_optimized() {
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![OpOld::Constant(pair0), OpOld::Car, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let sym2 = vm.gc.symbol_intern("d");
    let sym3 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);
    let list1 = vm.gc.listn(&[sym3]);
    let list2 = vm.gc.listn(&[list1, sym0, sym1, sym2]);

    let ops = vec![OpOld::Constant(list2), OpOld::Cdr, OpOld::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 4 + SIZE_OF_STRING * 0,
    );
}

// (cdr '(1 . 2))
#[test]
fn test241_optimized() {
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![OpOld::Constant(pair0), OpOld::Cdr, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("c");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);
    let list1 = vm.gc.listn(&[sym2, sym1, sym0]);

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(list1),
        OpOld::ReferFreeCall(26, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}

// (reverse '(a (b c) d (e (f))))
#[test]
fn test243_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::Frame(3),
        OpOld::ConstantPush(list7),
        OpOld::ReferFreeCall(26, 1),
        OpOld::Halt,
        OpOld::Nop,
    ];
    let expected = list3;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 9 + SIZE_OF_SYMBOL * 6 + SIZE_OF_STRING * 0,
    );
}

// (equal? 'a 'a)
#[test]
fn test244_optimized() {
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym0),
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);

    let ops = vec![
        OpOld::ConstantPush(list0),
        OpOld::Constant(list1),
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym1]);
    let list1 = vm.gc.listn(&[sym0, list0, sym2]);
    let list2 = vm.gc.listn(&[sym1]);
    let list3 = vm.gc.listn(&[sym0, list2, sym2]);

    let ops = vec![
        OpOld::ConstantPush(list1),
        OpOld::Constant(list3),
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("abc");
    let str1 = vm.gc.new_string("abc");

    let ops = vec![
        OpOld::ConstantPush(str0),
        OpOld::Constant(str1),
        OpOld::Equal,
        OpOld::Halt,
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (equal? 2 2)
#[test]
fn test248_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::Constant(sym0),
        OpOld::MakeVector,
        OpOld::PushConstant(Object::Number(5)),
        OpOld::PushConstant(sym0),
        OpOld::MakeVector,
        OpOld::Equal,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(5)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![OpOld::ConstantPush(sym0), OpOld::Constant(sym0), OpOld::Eq, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let list0 = vm.gc.listn(&[sym0]);
    let list1 = vm.gc.listn(&[sym0]);

    let ops = vec![
        OpOld::ConstantPush(list0),
        OpOld::Constant(list1),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(89, 1),
        OpOld::PushFrame(3),
        OpOld::ConstantPush(sym0),
        OpOld::ReferFreeCall(89, 1),
        OpOld::Eq,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("a");
    let str1 = vm.gc.new_string("a");

    let ops = vec![OpOld::ConstantPush(str0), OpOld::Constant(str1), OpOld::Eq, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? "" "")
#[test]
fn test254_optimized() {
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("");
    let str1 = vm.gc.new_string("");

    let ops = vec![OpOld::ConstantPush(str0), OpOld::Constant(str1), OpOld::Eq, OpOld::Halt];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (eq? '() '())
#[test]
fn test255_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Nil),
        OpOld::Constant(Object::Nil),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Char('A')),
        OpOld::Constant(Object::Char('A')),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::ReferFreePush(3), OpOld::ReferFree(3), OpOld::Eq, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::Constant(Object::Number(5)),
        OpOld::Eq,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
    let expected = Object::Number(3);
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::Closure {
            size: 3,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::PushEnter(1),
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(0),
        OpOld::Eq,
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(-1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(-6)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(-3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("greater");
    let sym1 = vm.gc.symbol_intern("less");

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotGt(3),
        OpOld::Constant(sym0),
        OpOld::LocalJmp(7),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotLt(3),
        OpOld::Constant(sym1),
        OpOld::LocalJmp(2),
        OpOld::Undef,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("equal");
    let sym1 = vm.gc.symbol_intern("greater");
    let sym2 = vm.gc.symbol_intern("less");

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotGt(3),
        OpOld::Constant(sym1),
        OpOld::LocalJmp(7),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotLt(3),
        OpOld::Constant(sym2),
        OpOld::LocalJmp(2),
        OpOld::Constant(sym0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);
    let list1 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Constant(list0),
        OpOld::Test(4),
        OpOld::Frame(3),
        OpOld::ConstantPush(list1),
        OpOld::ReferFreeCall(70, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = Object::Number(2);
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let pair0 = vm.gc.cons(sym0, sym1);

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym1),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec)
#[test]
fn test271_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::LetFrame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::ConstantPush(list1),
        OpOld::Constant(str3),
        OpOld::Vector(3),
        OpOld::PushEnter(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::PushConstant(list2),
        OpOld::VectorSet,
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = vec0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 4,
    );
}

// (vector-ref '#(1 1 2 3 5 8 13 21) 5)
#[test]
fn test272_optimized() {
    let mut vm = VmOld::new();

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
        OpOld::ConstantPush(vec0),
        OpOld::Constant(Object::Number(5)),
        OpOld::VectorRef,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotNumberEqual(2),
        OpOld::LocalJmp(4),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::BranchNotNumberEqual(2),
        OpOld::LocalJmp(4),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::False),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("b");
    let sym1 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1]);
    let list1 = vm.gc.listn(&[sym0, sym1]);

    let ops = vec![OpOld::Constant(list1), OpOld::Halt];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (not #t)
#[test]
fn test277_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::True), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(89, 1),
        OpOld::Not,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Car,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::False), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Nil), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(2),
        OpOld::ReferFreeCall(89, 0),
        OpOld::Not,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("nil");

    let ops = vec![OpOld::Constant(sym0), OpOld::Not, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(6)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(35)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(70)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym0),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![
        OpOld::ConstantPush(sym0),
        OpOld::Constant(sym1),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Cons,
        OpOld::Cdr,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Nil),
        OpOld::Constant(Object::Nil),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(100000000)),
        OpOld::Constant(Object::Number(100000000)),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(1)),
        OpOld::Return(0),
        OpOld::Push,
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(2)),
        OpOld::Return(0),
        OpOld::Eqv,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(123456789101112)),
        OpOld::Constant(Object::Number(123456789101112)),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("nil");

    let ops = vec![
        OpOld::ConstantPush(Object::False),
        OpOld::Constant(sym0),
        OpOld::Eqv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Char('3')),
        OpOld::ConstantPush(Object::Number(10)),
        OpOld::ReferFreeCall(39, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(0)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Cadr,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3)]);

    let ops = vec![
        OpOld::Frame(13),
        OpOld::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(2)),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 4),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Frame(11),
        OpOld::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::Frame(12),
        OpOld::Closure {
            size: 7,
            arg_len: 3,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocal(1),
        OpOld::NumberAddPush,
        OpOld::ReferLocal(2),
        OpOld::NumberAdd,
        OpOld::Return(3),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferFreeCall(152, 3),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(2)]);
    let list1 = vm.gc.listn(&[Object::Number(3), Object::Number(2)]);
    let list2 = vm.gc.listn(&[list1]);

    let ops = vec![
        OpOld::Frame(16),
        OpOld::ReferFreePush(152),
        OpOld::Closure {
            size: 6,
            arg_len: 2,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocalPushConstant(1, list2),
        OpOld::Push,
        OpOld::ReferFree(0),
        OpOld::TailCall(2, 2),
        OpOld::Return(2),
        OpOld::Push,
        OpOld::ReferFreePush(3),
        OpOld::ReferFreePush(4),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Push,
        OpOld::ReferFreeCall(152, 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (/ 6 2)
#[test]
fn test307_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(6)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ReferFreeCall(408, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(408, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[
        Object::Number(3),
        Object::Number(1),
        Object::Number(4),
        Object::Number(1),
        Object::Number(5),
        Object::Number(9),
    ]);

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ReferFreePush(408),
        OpOld::ConstantPush(list0),
        OpOld::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(9), Object::Number(2));
    let pair1 = vm.gc.cons(Object::Number(5), pair0);
    let pair2 = vm.gc.cons(Object::Number(1), pair1);
    let pair3 = vm.gc.cons(Object::Number(4), pair2);
    let pair4 = vm.gc.cons(Object::Number(1), pair3);
    let pair5 = vm.gc.cons(Object::Number(3), pair4);

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ReferFreePush(408),
        OpOld::ConstantPush(pair5),
        OpOld::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(4), Object::Number(14)]);

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ReferFreePush(408),
        OpOld::ConstantPush(list0),
        OpOld::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(4), Object::Number(14)]);

    let ops = vec![
        OpOld::Frame(12),
        OpOld::ReferFreePush(408),
        OpOld::Closure {
            size: 7,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::Frame(3),
        OpOld::ReferLocalPush(0),
        OpOld::ReferFreeCall(0, 1),
        OpOld::Test(2),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::PushConstant(list0),
        OpOld::Push,
        OpOld::ReferGlobalCall(vm.gc.intern("for-all"), 2),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::NumberSubPush,
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::NumberSub,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(5)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberAddPush,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberDiv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberGt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberLe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberGe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::NumberGe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberLt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberLt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::PushConstant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::NumberLt,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberLe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberLe,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::PushConstant(Object::Number(2)),
        OpOld::PushConstant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberDiv,
        OpOld::PushFrame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberEqual,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberDiv,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(12),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::ReferFreeCall(289, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::NumberDiv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberDiv,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(292, 0),
        OpOld::Push,
        OpOld::ReferFreeCall(289, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(1)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::Push,
        OpOld::ReferFreeCall(289, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(6),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(6),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(292, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberSubPush,
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::NumberGt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(2),
        OpOld::ReferFreeCall(292, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberSubPush,
        OpOld::Frame(2),
        OpOld::ReferFreeCall(292, 0),
        OpOld::NumberLt,
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(8),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(293, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberSubPush,
        OpOld::ReferFreeCall(289, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(8),
        OpOld::Frame(2),
        OpOld::ReferFreeCall(292, 0),
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberSubPush,
        OpOld::Constant(Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::ReferFreeCall(289, 1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(6),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberDiv,
        OpOld::Push,
        OpOld::ReferFreeCall(0, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(123)),
        OpOld::ConstantPush(Object::Number(10)),
        OpOld::ReferFreeCall(411, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(123)),
        OpOld::ConstantPush(Object::Number(-10)),
        OpOld::ReferFreeCall(411, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(-123)),
        OpOld::ConstantPush(Object::Number(10)),
        OpOld::ReferFreeCall(411, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(Object::Number(-123)),
        OpOld::ConstantPush(Object::Number(-10)),
        OpOld::ReferFreeCall(411, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("abc");

    let ops = vec![
        OpOld::Frame(4),
        OpOld::ConstantPush(str0),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ReferFreeCall(96, 2),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let sym2 = vm.gc.symbol_intern("c");
    let list0 = vm.gc.listn(&[sym0, sym1, sym2]);

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(list0),
        OpOld::ReferFreeCall(88, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(Object::Nil),
        OpOld::ReferFreeCall(88, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");
    let pair0 = vm.gc.cons(sym0, sym1);

    let ops = vec![
        OpOld::Frame(3),
        OpOld::ConstantPush(pair0),
        OpOld::ReferFreeCall(88, 1),
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let str0 = vm.gc.new_string("abc");
    let str1 = vm.gc.new_string("abc");

    let ops = vec![OpOld::Constant(str1), OpOld::Halt];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 1,
    );
}

// (let1 a 3 (let1 b 4 a))
#[test]
fn test39_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(7)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(12)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(12)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(13)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::PushConstant(Object::Number(1)),
        OpOld::NumberAdd,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(2)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::True),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(2)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(7)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(4)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(1),
        OpOld::ReferLocalPush(0),
        OpOld::Display(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(4)),
        OpOld::Return(0),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(0),
        OpOld::Leave(1),
        OpOld::Frame(4),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(3)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::LetFrame(1),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(100)),
        OpOld::Return(0),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::PushEnter(1),
        OpOld::Frame(2),
        OpOld::ReferLocalCall(0, 0),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(1)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Enter(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::LocalJmp(5),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Shiftj(1, 1, -1),
        OpOld::LocalJmp(-7),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(3),
        OpOld::ConstantPush(Object::Nil),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::LetFrame(2),
        OpOld::ReferLocalPush(0),
        OpOld::ReferLocalPush(0),
        OpOld::Display(2),
        OpOld::ReferLocalPush(0),
        OpOld::Closure {
            size: 10,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocalPushConstantBranchNotGe(0, Object::Number(1000), 3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::AssignFree(0),
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(4),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Enter(1),
        OpOld::ReferLocalPushConstantBranchNotGe(0, Object::Number(1000), 3),
        OpOld::ReferLocal(0),
        OpOld::LocalJmp(5),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Shiftj(1, 1, -1),
        OpOld::LocalJmp(-6),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Nil),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::Constant(Object::Number(1000)),
        OpOld::AssignLocal(0),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(2),
        OpOld::ConstantPush(Object::Nil),
        OpOld::Box(0),
        OpOld::Enter(1),
        OpOld::ReferLocalPush(0),
        OpOld::Closure {
            size: 11,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 1,
        },
        OpOld::ReferLocalPushConstant(0, Object::Number(20)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::Return(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::ReferFree(0),
        OpOld::Indirect,
        OpOld::TailCall(1, 1),
        OpOld::Return(1),
        OpOld::AssignLocal(0),
        OpOld::Frame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::ReferLocal(0),
        OpOld::Indirect,
        OpOld::Call(1),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::False),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::DefineGlobal(vm.gc.intern("a")),
        OpOld::Halt,
    ];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(7)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::NumberEqual,
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::BranchNotNumberEqual(4),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::NumberEqual,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::BranchNotNumberEqual(4),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::Constant(Object::Number(5)),
        OpOld::NumberEqual,
        OpOld::Halt,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Closure {
            size: 3,
            arg_len: 0,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::Constant(Object::Number(101)),
        OpOld::Return(0),
        OpOld::Call(0),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Frame(8),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Closure {
            size: 5,
            arg_len: 1,
            is_optional_arg: false,
            num_free_vars: 0,
        },
        OpOld::ConstantPush(Object::Number(101)),
        OpOld::ReferLocal(0),
        OpOld::NumberAdd,
        OpOld::Return(1),
        OpOld::Call(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Nil), OpOld::NullP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::NullP, OpOld::Halt];
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
    let mut vm = VmOld::new();

    let pair0 = vm.gc.cons(Object::Number(1), Object::Number(2));

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (cons 1 (cons 2 '()))
#[test]
fn test78_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(1), Object::Number(2)]);

    let ops = vec![
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::Constant(Object::Nil),
        OpOld::Cons,
        OpOld::Cons,
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (begin 1 2 3)
#[test]
fn test79_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(1)),
        OpOld::Constant(Object::Number(2)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(4)),
        OpOld::AssignGlobal(vm.gc.intern("a")),
        OpOld::ReferGlobal(vm.gc.intern("a")),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(3)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::Constant(Object::Number(3)),
        OpOld::Constant(Object::Number(4)),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Enter(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::LocalJmp(7),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Constant(Object::Number(0)),
        OpOld::NumberAddPush,
        OpOld::Shiftj(1, 1, -1),
        OpOld::LocalJmp(-9),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(5),
        OpOld::ConstantPush(Object::Number(0)),
        OpOld::Enter(1),
        OpOld::ReferLocalPushConstant(0, Object::Number(10)),
        OpOld::BranchNotNumberEqual(3),
        OpOld::ReferLocal(0),
        OpOld::LocalJmp(7),
        OpOld::ReferLocalPushConstant(0, Object::Number(1)),
        OpOld::NumberAddPush,
        OpOld::Constant(Object::Number(0)),
        OpOld::NumberAddPush,
        OpOld::Shiftj(1, 1, -1),
        OpOld::LocalJmp(-9),
        OpOld::Leave(1),
        OpOld::Halt,
        OpOld::Nop,
        OpOld::Nop,
        OpOld::Nop,
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(101)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(6)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let ops = vec![OpOld::Constant(Object::Number(3)), OpOld::Halt];
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
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(1), Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::List(3),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3)
#[test]
fn test93_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(2), Object::Number(3)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::List(2),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a . b) b) 1 2 3 4)
#[test]
fn test94_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm
        .gc
        .listn(&[Object::Number(2), Object::Number(3), Object::Number(4)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(2)),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::List(3),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 3 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b . c) c) 1 2 3 4)
#[test]
fn test95_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(3), Object::Number(4)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(3)),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::List(2),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 2 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c . d) d) 1 2 3 4)
#[test]
fn test96_optimized() {
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(4)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(4)),
        OpOld::List(1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a b c . d) d) 1 2 3)
#[test]
fn test97_optimized() {
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::List(0),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::List(0),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
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
    let mut vm = VmOld::new();

    let list0 = vm.gc.listn(&[Object::Number(1)]);

    let ops = vec![
        OpOld::LetFrame(1),
        OpOld::ConstantPush(Object::Number(1)),
        OpOld::List(1),
        OpOld::PushEnter(1),
        OpOld::ReferLocal(0),
        OpOld::Leave(1),
        OpOld::Halt,
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_PAIR * 1 + SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

#[test]
fn test_zero() {
    let mut vm = VmOld::new();
    vm.should_load_compiler = true;

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("zero?")),
        OpOld::Call(1),
        OpOld::Halt,
    ];
    let ret = vm.run(ops.as_ptr(), ops.len());
    vm.expected = Object::True;
    // Remove reference to ret.
    vm.ac = Object::Unspecified;
    let e = Equal::new();
    if !e.is_equal(&mut vm.gc, &ret, &vm.expected) {
        println!("ret={} expected={}", ret, vm.expected);
        assert_eq!(ret, vm.expected);
    }
}

#[test]
fn test_compiler() {
    let mut vm = VmOld::new();
    vm.should_load_compiler = true;

    let ops = vec![
        OpOld::Frame(5),
        OpOld::Constant(Object::Number(0)),
        OpOld::Push,
        OpOld::ReferGlobal(vm.gc.intern("compile")),
        OpOld::Call(1),
        OpOld::Halt,
    ];
    let ret = vm.run(ops.as_ptr(), ops.len());

    vm.expected = Object::True;
    // Remove reference to ret.
    vm.ac = Object::Unspecified;
    let e = Equal::new();
    if !e.is_equal(&mut vm.gc, &ret, &vm.expected) {
        println!("ret={} expected={}", ret, vm.expected);
        assert_eq!(ret, vm.expected);
    }
}
