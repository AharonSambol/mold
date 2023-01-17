trait Iter_ {
    type Item;
    fn next(&mut self) -> Self::Item;
}

struct A {
    inner: Vec<i32>
}
impl Iter_ for A {
    type Item = i32;

    fn next(&mut self) -> Self::Item {
        self.inner[0]
    }
}


fn func(mut a: &mut i32) {
    *a =  *a + 1;
}
fn main() {
    let mut a = A { inner: vec![1, 2] };
    // ch(&mut a);
    a.next();
    println!("{:?}", a.inner);

    // let mut x: i32 = 325;
    // let mut a: i32 = 5;
    // let mut b: &mut i32 =  &mut a;
    // func(b);
    // println!("{}", Box::new(a));
}

