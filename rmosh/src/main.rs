use gc::GcRef;
use objects::{Object, Symbol};

use crate::op::Op;

pub mod alloc;
pub mod equal;
pub mod gc;
pub mod objects;
pub mod op;
pub mod procs;
pub mod vm;

fn main() {
    println!(
        "size_of<Object>={:?} size_of<GcRef<Symbol>>={} size_of<Op>={}",
        std::mem::size_of::<Object>(),
        std::mem::size_of::<GcRef<Symbol>>(),
        std::mem::size_of::<Op>()
    );
}
