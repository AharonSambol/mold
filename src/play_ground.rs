use std::fmt::{Debug, Display};
enum E<T> {
    A(i32),
    B(T)
}

trait Iter_ {
    type Item: Display;
    fn next(self: &mut Self) -> Self::Item;
}

struct A {
    inner: Vec<i32>
}
impl Iter_ for A {
    type Item = i32;

    fn next(mut self: &mut Self) -> Self::Item {
        self.inner[0]
    }
}


fn func(a: i32) -> Box<dyn Iterator<Item=i32>> {
    return Box::new(vec![a].into_iter());
}

fn main() {
    func(2);
    let mut a: Box<dyn Iterator<Item=i32>> = Box::new((0..10));
    a.into_iter().next();
    a = Box::new((0..10).step_by(2).rev());
    for i in (0..10).step_by(2) {
        let mut a = IntoIterator::into_iter((0..10).step_by(2));
        let b = Iterator::next(&mut a);
    }
    let mut a = A { inner: vec![1, 2] };
    // ch(&mut a);
    // let mut a = IntoIterator::into_iter(vec![]);
    // let b = Iterator::next(&mut a);
    a.next();
    println!("{:?}", a.inner);

    // let mut x: i32 = 325;
    // let mut a: i32 = 5;
    // let mut b: &mut i32 =  &mut a;
    // func(b);
    // println!("{}", Box::new(a));
}

