fn main() {
    println!("Hello, world!");

    let num: usize = 7;
    println!("number={}", num);

    println!("number & 0x3={}", num & 0x3);

    let address = 0xdeadbeefusize;
    let raw_pointer = address as *const i32;
    unsafe {
        // This causes Segmentation fault.
        // println!("pointer={}", *raw_pointer);
    }

    let num_tag_bits = 3;
    let fixnum_tag = 0x1;
    let fixnum: isize = 123456;
    // Create fixnum.
    let obj = fixnum << num_tag_bits;
    let obj = obj | fixnum_tag;
    let obj = obj as *const i64;

    // print fixnum
    let num = obj as isize >> num_tag_bits;
    print!("num={}", num);


    // refactor later.
}
