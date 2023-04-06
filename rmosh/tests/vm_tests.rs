use rmosh::{
    self,
    equal::Equal,
    objects::{Closure, Object, Pair, Procedure, SString, Symbol, Vector},
    op::Op,
    vm::Vm,
};

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
static SIZE_OF_MIN_VM: usize = SIZE_OF_CLOSURE * 2
    + (SIZE_OF_PROCEDURE * 623 * 2)
    + SIZE_OF_CLOSURE * 0
    + SIZE_OF_SYMBOL * 0
    + SIZE_OF_STRING * 0 + 192 + 96 + 536 /* todo */;

fn test_ops_with_size(vm: &mut Vm, ops: Vec<Object>, expected: Object, expected_heap_diff: usize) {
    // Keep reference so that it won't be freed.
    vm.expected = expected;

    let ret = vm.run(ops.as_ptr(), ops.len()).unwrap();
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

// (and)
#[test]
fn and() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// ((lambda () 3))
#[test]
fn call0() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(14),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (+ a a)) 1)
#[test]
fn call1() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(21),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(14),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(6),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING,
    );
}

// ((lambda (a b) (+ a b)) 1 2)
#[test]
fn call2() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("b");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1, sym2]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(24),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(14),
        Object::Fixnum(2),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(7),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(1),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Return),
        Object::Fixnum(2),
        Object::Instruction(Op::Call),
        Object::Fixnum(2),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING,
    );
}

// (define a 3)
#[test]
fn define0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::DefineGlobal),
        vm.gc.symbol_intern("a"),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (if 1 2 3)
#[test]
fn if0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Test),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (if #f 2 3)
#[test]
fn if1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Test),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (if #f #f #t)
#[test]
fn if2() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Test),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((x 0)) x)
#[test]
fn let0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((x 1) (y 2)) (+ x y))
#[test]
fn let1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(1),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Fixnum(2),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((x 1)) (let ((y 2)) (+ x y)))
#[test]
fn nested_let0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((x 1)) (let ((y 2)) (let ((z 3)) (+ x y z))))
#[test]
fn nested_let1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(4),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(2),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// #t
#[test]
fn test0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (if #f #f #t)
#[test]
fn test5() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Test),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// ((lambda (a) 3) 4)
#[test]
fn test6() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(17),
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING,
    );
}

// ((lambda (a) (if 3 7 5)) 6)
#[test]
fn test7() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(25),
        Object::Instruction(Op::Constant),
        Object::Fixnum(6),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(18),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Test),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::Fixnum(7),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(5),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(7);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () 3))
#[test]
fn test8() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(14),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) a) 101)
#[test]
fn test9() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(17),
        Object::Instruction(Op::Constant),
        Object::Fixnum(101),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(101);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (((lambda () (lambda () 102))))
#[test]
fn test10() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let str1 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);
    let list2 = vm.gc.listn(&[str1, Object::Fixnum(1)]);
    let list3 = vm.gc.listn(&[list2, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(27),
        Object::Instruction(Op::Frame),
        Object::Fixnum(23),
        Object::Instruction(Op::Closure),
        Object::Fixnum(19),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list3,
        Object::Instruction(Op::Constant),
        Object::Fixnum(102),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (<= 1 2 3)
#[test]
fn test119() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::BranchNotLe),
        Object::Fixnum(7),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NumberLe),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (>= 3 3 3)
#[test]
fn test112() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::BranchNotGe),
        Object::Fixnum(7),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NumberGe),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (< 1 5 3)
#[test]
fn test117() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(5),
        Object::Instruction(Op::BranchNotLt),
        Object::Fixnum(7),
        Object::Instruction(Op::Constant),
        Object::Fixnum(5),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NumberLt),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (> 4 3 2)
#[test]
fn test110() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::BranchNotGt),
        Object::Fixnum(7),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::NumberGt),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (= 3 3 3)
#[test]
fn test71() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::BranchNotNumberEqual),
        Object::Fixnum(7),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NumberEqual),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (begin (let ((xxx 'a)) (case xxx ((b) 'b) ((a) 'a))))
#[test]
fn test197() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");
    let sym1 = vm.gc.symbol_intern("b");

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(4),
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        sym1,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::BranchNotEqv),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        sym1,
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(13),
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::BranchNotEqv),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(2),
        Object::Instruction(Op::Undef),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = sym0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (let ((a '(1 2 3))) `(,@a 4))
#[test]
fn test144() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[
        Object::Fixnum(1),
        Object::Fixnum(2),
        Object::Fixnum(3),
        Object::Fixnum(4),
    ]);
    let list1 = vm
        .gc
        .listn(&[Object::Fixnum(1), Object::Fixnum(2), Object::Fixnum(3)]);
    let list2 = vm.gc.listn(&[Object::Fixnum(4)]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::Constant),
        list1,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        list2,
        Object::Instruction(Op::Append2),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let expected = list0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0 + SIZE_OF_PAIR * 4,
    );
}

// (let ((a 0) (b 1)) (let ((c (lambda () (set! b 3) b))) (c)))
#[test]
fn test58() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(2),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(15),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(1),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::AssignFree),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(5),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(2),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// ((lambda () (set! a 4) a))
#[test]
fn test80() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(18),
        Object::Instruction(Op::Closure),
        Object::Fixnum(14),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::AssignGlobal),
        vm.gc.symbol_intern("a"),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("a"),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING,
    );
}

// ((lambda (a) (set! a 12) a) 2)
#[test]
fn test14() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(24),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Fixnum(17),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Constant),
        Object::Fixnum(12),
        Object::Instruction(Op::AssignLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(12);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (car (cons 2 3))
#[test]
fn test28() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Cons),
        Object::Instruction(Op::Car),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (cdr (cons 2 3))
#[test]
fn test29() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Cons),
        Object::Instruction(Op::Cdr),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (eq? #t #t)
#[test]
fn test122() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Eq),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (eqv? 'a 'a)
#[test]
fn test287() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::Eqv),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (equal? '(1 2 (3)) '(1 2 (3)))
#[test]
fn test169() {
    let mut vm = Vm::new();

    let list0 = vm.gc.listn(&[Object::Fixnum(3)]);
    let list1 = vm.gc.listn(&[Object::Fixnum(1), Object::Fixnum(2), list0]);
    let list2 = vm.gc.listn(&[Object::Fixnum(3)]);
    let list3 = vm.gc.listn(&[Object::Fixnum(1), Object::Fixnum(2), list2]);

    let ops = vec![
        Object::Instruction(Op::Constant),
        list1,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        list3,
        Object::Instruction(Op::Equal),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (vector-length (make-vector 3))
#[test]
fn test187() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Nil,
        Object::Instruction(Op::MakeVector),
        Object::Instruction(Op::VectorLength),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (not 3)
#[test]
fn test101() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Not),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (null? '())
#[test]
fn test75() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Nil,
        Object::Instruction(Op::NullP),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (* 2 3)
#[test]
fn test199() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NumberMul),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(6);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (- (/ 1 2) (/ 1 4) (/ 1 4))
#[test]
fn test318() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::NumberSub),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::NumberSub),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((p (open-string-input-port "12345"))) (read-char p) (read-char p))
#[test]
fn test195() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("12345");

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(35),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::Constant),
        str0,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::ReadChar),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::ReadChar),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Char('2');
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// ((lambda () (define q (cons 1 2)) (set-car! q 3) q))
#[test]
fn test185() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);
    let pair0 = vm.gc.cons(Object::Fixnum(3), Object::Fixnum(2));

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(40),
        Object::Instruction(Op::Closure),
        Object::Fixnum(36),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(6),
        list1,
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::Undef),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Cons),
        Object::Instruction(Op::AssignLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::SetCar),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0 + SIZE_OF_PAIR,
    );
}

// ((lambda () (define p (cons 1 2)) (set-cdr! p 3) p))
#[test]
fn test184() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);
    let pair0 = vm.gc.cons(Object::Fixnum(1), Object::Fixnum(3));

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(40),
        Object::Instruction(Op::Closure),
        Object::Fixnum(36),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(6),
        list1,
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::Undef),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Cons),
        Object::Instruction(Op::AssignLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::SetCdr),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = pair0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0 + SIZE_OF_PAIR,
    );
}

// (symbol? 'a)
#[test]
fn test128() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Object::Instruction(Op::Constant),
        sym0,
        Object::Instruction(Op::SymbolP),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (if (values 1 2 3) #t #f)
#[test]
fn test220() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Values),
        Object::Fixnum(3),
        Object::Instruction(Op::Test),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::True,
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (call-with-values (lambda () (values 4 5)) (lambda (a b) b))
/*
#[test]
fn test221() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let sym2 = vm.gc.symbol_intern("b");
    let str0 = vm.gc.new_string("(input string port)");
    let str1 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);
    let list2 = vm.gc.listn(&[str1, Object::Fixnum(1)]);
    let list3 = vm.gc.listn(&[list2, sym0, sym1, sym2]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(152),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(19),
        Object::Instruction(Op::Closure),
        Object::Fixnum(15),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(5),
        Object::Instruction(Op::Values),
        Object::Fixnum(2),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Receive),
        Object::Fixnum(0),
        Object::Fixnum(1),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(20),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(2),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(6),
        list3,
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(1),
        Object::Instruction(Op::Return),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Call),
        Object::Fixnum(2),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(5);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 3 + SIZE_OF_STRING * 0,
    );
}
*/
// (vector? #(3))
#[test]
fn test213() {
    let mut vm = Vm::new();

    let vec0 = vm.gc.new_vector(&vec![Object::Fixnum(3)]);

    let ops = vec![
        Object::Instruction(Op::Constant),
        vec0,
        Object::Instruction(Op::VectorP),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (begin (define z (make-vector 2)) (vector-set! z 0 1) (vector-set! z 1 2) (make-vector 3) (null? 3) (vector-set! z 1 3) (vector-ref z 1))
#[test]
fn test217() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Nil,
        Object::Instruction(Op::MakeVector),
        Object::Instruction(Op::DefineGlobal),
        vm.gc.symbol_intern("z"),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("z"),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::VectorSet),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("z"),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::VectorSet),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Nil,
        Object::Instruction(Op::MakeVector),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::NullP),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("z"),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::VectorSet),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("z"),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Fixnum(1),
        Object::Instruction(Op::VectorRef),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0 + SIZE_OF_VECTOR,
    );
}

// (((lambda () (lambda (a) 102))) 101)
#[test]
fn test11() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(16),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(101),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(102),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 0,
    );
}

// (- (/ 1 2) (/ 1 4) (/ 1 4))
#[test]
fn test318_2() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::PushConstant),
        Object::Fixnum(1),
        Object::Instruction(Op::PushConstant),
        Object::Fixnum(4),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::NumberSubPush),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::NumberDiv),
        Object::Instruction(Op::NumberSub),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(0);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (eq? (list 'a) (list 'a))
#[test]
fn test191() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("a");

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(6),
        Object::Instruction(Op::ConstantPush),
        sym0,
        Object::Instruction(Op::ReferFreeCall),
        Object::Fixnum(89),
        Object::Fixnum(1),
        Object::Instruction(Op::PushFrame),
        Object::Fixnum(6),
        Object::Instruction(Op::ConstantPush),
        sym0,
        Object::Instruction(Op::ReferFreeCall),
        Object::Fixnum(89),
        Object::Fixnum(1),
        Object::Instruction(Op::Eq),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::False;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (let1 a 3 (let1 b 4 (let1 c (lambda () b) (set! a c))) (a))
#[test]
fn test55() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(3),
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocalPush),
        Object::Fixnum(0),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(4),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::AssignFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(6),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Call),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(4);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 0,
    );
}

// (letrec ((a (lambda (i) (if (= i 10) i (a (+ i 1)))))) (a 0))
#[test]
fn test62() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(4),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocalPushConstant),
        Object::Fixnum(0),
        Object::Fixnum(10),
        Object::Instruction(Op::BranchNotNumberEqual),
        Object::Fixnum(5),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(11),
        Object::Instruction(Op::ReferLocalPushConstant),
        Object::Fixnum(0),
        Object::Fixnum(1),
        Object::Instruction(Op::NumberAddPush),
        Object::Instruction(Op::Shiftj),
        Object::Fixnum(1),
        Object::Fixnum(1),
        Object::Fixnum(-1),
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(-18),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(10);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (cond ((cons 1 2) => car) (#f 2) (else 3))
#[test]
fn test133() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::ReferFreePush),
        Object::Fixnum(3),
        Object::Instruction(Op::Display),
        Object::Fixnum(1),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(1),
        Object::Instruction(Op::Constant),
        Object::Fixnum(2),
        Object::Instruction(Op::Cons),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Test),
        Object::Fixnum(10),
        Object::Instruction(Op::Frame),
        Object::Fixnum(12),
        Object::Instruction(Op::ReferLocalPush),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferFreeCall),
        Object::Fixnum(0),
        Object::Fixnum(1),
        Object::Instruction(Op::LocalJmp),
        Object::Fixnum(5),
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Constant),
        Object::Fixnum(3),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(1);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((a '())) (let ((G68 (lambda (i) (if (>= i 1000) i (a (+ i 1)))))) (set! a G68) (a 0)))
#[test]
fn test63() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("i");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(3),
        Object::Instruction(Op::ConstantPush),
        Object::Nil,
        Object::Instruction(Op::Box),
        Object::Fixnum(0),
        Object::Instruction(Op::Enter),
        Object::Fixnum(1),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferLocalPush),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferLocalPush),
        Object::Fixnum(0),
        Object::Instruction(Op::Display),
        Object::Fixnum(2),
        Object::Instruction(Op::ReferLocalPush),
        Object::Fixnum(0),
        Object::Instruction(Op::Closure),
        Object::Fixnum(26),
        Object::Fixnum(1),
        Object::False,
        Object::Fixnum(1),
        Object::Fixnum(8),
        list1,
        Object::Instruction(Op::ReferLocalPushConstantBranchNotGe),
        Object::Fixnum(0),
        Object::Fixnum(1000),
        Object::Fixnum(5),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocalPushConstant),
        Object::Fixnum(0),
        Object::Fixnum(1),
        Object::Instruction(Op::NumberAddPush),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::TailCall),
        Object::Fixnum(1),
        Object::Fixnum(1),
        Object::Instruction(Op::Return),
        Object::Fixnum(1),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::AssignFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(0),
        Object::Instruction(Op::ReferFree),
        Object::Fixnum(0),
        Object::Instruction(Op::Indirect),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(1000);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2,
    );
}

// (begin (define str1 (make-string 3 #\c)) (string-set! str1 1 #\b) str1)
#[test]
fn test205() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("cbc");

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(3),
        Object::Instruction(Op::ConstantPush),
        Object::Char('c'),
        Object::Instruction(Op::ReferFreeCall),
        Object::Fixnum(17),
        Object::Fixnum(2),
        Object::Instruction(Op::DefineGlobal),
        vm.gc.symbol_intern("str1"),
        Object::Instruction(Op::Frame),
        Object::Fixnum(10),
        Object::Instruction(Op::ReferGlobalPush),
        vm.gc.symbol_intern("str1"),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(1),
        Object::Instruction(Op::ConstantPush),
        Object::Char('b'),
        Object::Instruction(Op::ReferFreeCall),
        Object::Fixnum(18),
        Object::Fixnum(3),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("str1"),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = str0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL + SIZE_OF_STRING * 2,
    );
}

// (let1 a 100 (let1 c (let1 d (lambda () a) d) (c)))
#[test]
fn test60() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Fixnum(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(2),
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(1),
        Object::Instruction(Op::Closure),
        Object::Fixnum(10),
        Object::Fixnum(0),
        Object::False,
        Object::Fixnum(0),
        Object::Fixnum(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Fixnum(100),
        Object::Instruction(Op::Return),
        Object::Fixnum(0),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::Frame),
        Object::Fixnum(4),
        Object::Instruction(Op::ReferLocalCall),
        Object::Fixnum(0),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Fixnum(100);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL,
    );
}

// (vector-length (make-vector 3))
#[test]
fn test187_2() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(3),
        Object::Instruction(Op::Constant),
        Object::Nil,
        Object::Instruction(Op::MakeVector),
        Object::Instruction(Op::VectorLength),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Fixnum(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        0,
    );
}

// (let ((vec (vector 0 '(2 2 2 2) "Anna"))) (vector-set! vec 1 '("Sue" "Sue")) vec)
#[test]
fn test271() {
    let mut vm = Vm::new();

    let str0 = vm.gc.new_string("Sue");
    let str1 = vm.gc.new_string("Sue");
    let str2 = vm.gc.new_string("Anna");
    let str3 = vm.gc.new_string("Anna");
    let str4 = vm.gc.new_string("Sue");
    let str5 = vm.gc.new_string("Sue");
    let list0 = vm.gc.listn(&[str0, str1]);
    let list1 = vm.gc.listn(&[
        Object::Fixnum(2),
        Object::Fixnum(2),
        Object::Fixnum(2),
        Object::Fixnum(2),
    ]);
    let list2 = vm.gc.listn(&[str4, str5]);
    let vec0 = vm.gc.new_vector(&vec![Object::Fixnum(0), list0, str2]);

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Fixnum(5),
        Object::Instruction(Op::ConstantPush),
        Object::Fixnum(0),
        Object::Instruction(Op::ConstantPush),
        list1,
        Object::Instruction(Op::Constant),
        str3,
        Object::Instruction(Op::Vector),
        Object::Fixnum(3),
        Object::Instruction(Op::PushEnter),
        Object::Fixnum(1),
        Object::Instruction(Op::ReferLocalPushConstant),
        Object::Fixnum(0),
        Object::Fixnum(1),
        Object::Instruction(Op::PushConstant),
        list2,
        Object::Instruction(Op::VectorSet),
        Object::Instruction(Op::ReferLocal),
        Object::Fixnum(0),
        Object::Instruction(Op::Leave),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let expected = vec0;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_STRING * 3 + SIZE_OF_VECTOR + SIZE_OF_PAIR * 2,
    );
}

#[test]
fn test_compiler2() {
    let mut vm = Vm::new();
    vm.should_load_compiler = true;

    let plus = vm.gc.symbol_intern("+");
    let code = vm.gc.list3(plus, Object::Fixnum(121), Object::Fixnum(20));

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::Constant),
        code,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("compile"),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let ret = vm.run(ops.as_ptr(), ops.len()).unwrap();
    if let Object::Vector(v) = ret {
        let ret = vm.run(v.data.as_ptr(), v.data.len()).unwrap();
        vm.expected = Object::Fixnum(141);
        // Remove reference to ret.
        vm.ac = Object::Unspecified;
        let e = Equal::new();
        if !e.is_equal(&mut vm.gc, &ret, &vm.expected) {
            println!("ret={} expected={}", ret, vm.expected);
            assert_eq!(ret, vm.expected);
        }
    }
}

#[test]
fn test_compiler() {
    let mut vm = Vm::new();
    vm.should_load_compiler = true;

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::Constant),
        Object::Fixnum(121),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("compile"),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let ret = vm.run(ops.as_ptr(), ops.len()).unwrap();
    if let Object::Vector(v) = ret {
        let ret = vm.run(v.data.as_ptr(), v.data.len()).unwrap();
        vm.expected = Object::Fixnum(121);
        // Remove reference to ret.
        vm.ac = Object::Unspecified;
        let e = Equal::new();
        if !e.is_equal(&mut vm.gc, &ret, &vm.expected) {
            println!("ret={} expected={}", ret, vm.expected);
            assert_eq!(ret, vm.expected);
        }
    }
}
/*
fn read(vm: &mut Vm, s: &str) -> Result<Object, ReadError> {
    let mut port = StringInputPort::new(s);
    port.read(vm)
}

#[test]
fn test_compiler3() {
    let mut vm = Vm::new();
    vm.should_load_compiler = true;

    let sexp = read(&mut vm, "((lambda (a) a) 3)").unwrap();
    println!("read={}", sexp);
    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Fixnum(8),
        Object::Instruction(Op::Constant),
        sexp,
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferGlobal),
        vm.gc.symbol_intern("compile-no-optimize"),
        Object::Instruction(Op::Call),
        Object::Fixnum(1),
        Object::Instruction(Op::Halt),
    ];
    let ret = vm.run(ops.as_ptr(), ops.len()).unwrap();

    match ret {
        Object::Vector(v) => {
            let ret = vm.run(v.data.as_ptr(), v.data.len()).unwrap();
            vm.expected = Object::Fixnum(3);
            // Remove reference to ret.
            vm.ac = Object::Unspecified;
            let e = Equal::new();
            if !e.is_equal(&mut vm.gc, &ret, &vm.expected) {
                println!("ret={} expected={}", ret, vm.expected);
                assert_eq!(ret, vm.expected);
            }
        }
        _ => {}
    }
}
*/
