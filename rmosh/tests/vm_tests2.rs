use rmosh::{
    self,
    equal::Equal,
    gc::Gc,
    objects::{Closure, Object, Op, Pair, Procedure, SString, Symbol, Vector},
    vm2::Vm,
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
static SIZE_OF_MIN_VM: usize = SIZE_OF_CLOSURE
    + (SIZE_OF_PROCEDURE * 623)
    + SIZE_OF_CLOSURE * 0
    + SIZE_OF_SYMBOL * 0
    + SIZE_OF_STRING * 0;

fn test_ops_with_size(vm: &mut Vm, ops: Vec<Object>, expected: Object, expected_heap_diff: usize) {
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
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda () 3))
#[test]
fn call0() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(14),
        Object::Instruction(Op::Closure),
        Object::Number(10),
        Object::Number(0),
        Object::False,
        Object::Number(0),
        Object::Number(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Return),
        Object::Number(0),
        Object::Instruction(Op::Call),
        Object::Number(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) (+ a a)) 1)
#[test]
fn call1() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(21),
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Number(14),
        Object::Number(1),
        Object::False,
        Object::Number(0),
        Object::Number(6),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Return),
        Object::Number(1),
        Object::Instruction(Op::Call),
        Object::Number(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(2);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 1,
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
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1, sym2]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(24),
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Number(14),
        Object::Number(2),
        Object::False,
        Object::Number(0),
        Object::Number(7),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(1),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Return),
        Object::Number(2),
        Object::Instruction(Op::Call),
        Object::Number(2),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 1,
    );
}

// (define a 3)
#[test]
fn define0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::DefineGlobal),
        vm.gc.symbol_intern("a"),
        Object::Instruction(Op::Halt),
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (if 1 2 3)
#[test]
fn if0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Test),
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::LocalJmp),
        Object::Number(3),
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
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
fn if1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Test),
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::LocalJmp),
        Object::Number(3),
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
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
fn if2() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::Test),
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::LocalJmp),
        Object::Number(3),
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
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// (let ((x 0)) x)
#[test]
fn let0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Number(1),
        Object::Instruction(Op::Constant),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Halt),
    ];
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
fn let1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Number(3),
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(2),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(1),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Number(2),
        Object::Instruction(Op::Halt),
    ];
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
fn nested_let0() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Number(3),
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::LetFrame),
        Object::Number(2),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Number(1),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::ReferFree),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Halt),
    ];
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
fn nested_let1() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::LetFrame),
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::LetFrame),
        Object::Number(4),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Number(1),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::LetFrame),
        Object::Number(3),
        Object::Instruction(Op::ReferFree),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Display),
        Object::Number(2),
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Enter),
        Object::Number(1),
        Object::Instruction(Op::ReferFree),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferFree),
        Object::Number(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::NumberAdd),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Leave),
        Object::Number(1),
        Object::Instruction(Op::Halt),
    ];
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
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
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
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::False,
        Object::Instruction(Op::LocalJmp),
        Object::Number(3),
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
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) 3) 4)
#[test]
fn test6() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(17),
        Object::Instruction(Op::Constant),
        Object::Number(4),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Number(10),
        Object::Number(1),
        Object::False,
        Object::Number(0),
        Object::Number(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Return),
        Object::Number(1),
        Object::Instruction(Op::Call),
        Object::Number(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 1,
    );
}

// ((lambda (a) (if 3 7 5)) 6)
#[test]
fn test7() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(25),
        Object::Instruction(Op::Constant),
        Object::Number(6),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Number(18),
        Object::Number(1),
        Object::False,
        Object::Number(0),
        Object::Number(5),
        list1,
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Test),
        Object::Number(5),
        Object::Instruction(Op::Constant),
        Object::Number(7),
        Object::Instruction(Op::Return),
        Object::Number(1),
        Object::Instruction(Op::Constant),
        Object::Number(5),
        Object::Instruction(Op::Return),
        Object::Number(1),
        Object::Instruction(Op::Call),
        Object::Number(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(7);
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
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(14),
        Object::Instruction(Op::Closure),
        Object::Number(10),
        Object::Number(0),
        Object::False,
        Object::Number(0),
        Object::Number(4),
        list1,
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::Return),
        Object::Number(0),
        Object::Instruction(Op::Call),
        Object::Number(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(3);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// ((lambda (a) a) 101)
#[test]
fn test9() {
    let mut vm = Vm::new();

    let sym0 = vm.gc.symbol_intern("lambda");
    let sym1 = vm.gc.symbol_intern("a");
    let str0 = vm.gc.new_string("(input string port)");
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0, sym1]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(17),
        Object::Instruction(Op::Constant),
        Object::Number(101),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Closure),
        Object::Number(10),
        Object::Number(1),
        Object::False,
        Object::Number(0),
        Object::Number(5),
        list1,
        Object::Instruction(Op::ReferLocal),
        Object::Number(0),
        Object::Instruction(Op::Return),
        Object::Number(1),
        Object::Instruction(Op::Call),
        Object::Number(1),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(101);
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
    let list0 = vm.gc.listn(&[str0, Object::Number(1)]);
    let list1 = vm.gc.listn(&[list0, sym0]);
    let list2 = vm.gc.listn(&[str1, Object::Number(1)]);
    let list3 = vm.gc.listn(&[list2, sym0]);

    let ops = vec![
        Object::Instruction(Op::Frame),
        Object::Number(27),
        Object::Instruction(Op::Frame),
        Object::Number(23),
        Object::Instruction(Op::Closure),
        Object::Number(19),
        Object::Number(0),
        Object::False,
        Object::Number(0),
        Object::Number(4),
        list1,
        Object::Instruction(Op::Closure),
        Object::Number(10),
        Object::Number(0),
        Object::False,
        Object::Number(0),
        Object::Number(4),
        list3,
        Object::Instruction(Op::Constant),
        Object::Number(102),
        Object::Instruction(Op::Return),
        Object::Number(0),
        Object::Instruction(Op::Return),
        Object::Number(0),
        Object::Instruction(Op::Call),
        Object::Number(0),
        Object::Instruction(Op::Call),
        Object::Number(0),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::Number(102);
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 1 + SIZE_OF_STRING * 0,
    );
}

// (<= 1 2 3)
#[test]
fn test119() {
    let mut vm = Vm::new();

    let ops = vec![
        Object::Instruction(Op::Constant),
        Object::Number(1),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::BranchNotLe),
        Object::Number(7),
        Object::Instruction(Op::Constant),
        Object::Number(2),
        Object::Instruction(Op::Push),
        Object::Instruction(Op::Constant),
        Object::Number(3),
        Object::Instruction(Op::NumberLe),
        Object::Instruction(Op::Halt),
        Object::Instruction(Op::Nop),
    ];
    let expected = Object::True;
    test_ops_with_size(
        &mut vm,
        ops,
        expected,
        SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0,
    );
}



    // (>= 3 3 3)
    #[test]
    fn test112() {
        let mut vm = Vm::new();


        let ops = vec![
            Object::Instruction(Op::Constant),
            Object::Number(3),
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            Object::Number(3),
            Object::Instruction(Op::BranchNotGe),
            Object::Number(7),
            Object::Instruction(Op::Constant),
            Object::Number(3),
            Object::Instruction(Op::Push),
            Object::Instruction(Op::Constant),
            Object::Number(3),
            Object::Instruction(Op::NumberGe),
            Object::Instruction(Op::Halt),
            Object::Instruction(Op::Nop),

        ];
        let expected = Object::True;
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 0 + SIZE_OF_STRING * 0);
    }

        