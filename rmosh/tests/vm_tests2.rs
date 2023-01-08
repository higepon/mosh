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
        test_ops_with_size(&mut vm, ops, expected, SIZE_OF_SYMBOL * 2 + SIZE_OF_STRING * 1);
    }
        
        
