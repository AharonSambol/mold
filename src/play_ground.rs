





fn func(mut a: &mut i32) {
    *a =  *a + 1;
}
fn main() {
    let mut x: i32 = 325;
    let mut a: i32 = 5;
    let mut b: &mut i32 =  &mut a;
    func(b);
    println!("{}", Box::new(a));
}

