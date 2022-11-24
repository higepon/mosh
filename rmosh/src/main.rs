fn main() {
    println!("Hello, world!");

    let num: usize = 7;
    println!("number={}", num);

    println!("number & 0x3={}", num & 0x3);

    let address = 0xdeadbeefusize;
    let raw_pointer = address as *const i32;
    unsafe {
        // This causes Segmentation fault.
        println!("pointer={}", *raw_pointer);
    }
}
