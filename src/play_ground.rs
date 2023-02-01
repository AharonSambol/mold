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


fn func<T>(a: T) -> Box<dyn Iterator<Item=i32>> {
    // let b = *a;
    return Box::new(vec![1].into_iter());
}
fn ln(a: &Box<dyn Debug>) {}
fn ff(a: &Vec<i32>) {
    // ln(&(Box::new(a) as Box<dyn Debug>));
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
    // vec![].iter_mut().rev()
    a.next();
    println!("{:?}", a.inner);
    let inferred = (5_f64 / 2_f64).floor();

    let mut lst = vec![vec![1, 2], vec![3, 4]];
    // lst.iter_mut(). = 2;
    // get_list()[0][-1];
    // lst = get_list();
    // len = lst.len();
    // idx1 = lst[(len+0) %len];
    // idx2 = lst[(len+ -1) %len];
    // let a = lst[0];
    // a = vec![2];

    // let mut a: Vec::<i32> = vec![1i32, 2i32];
    // let _ = {let list = a;let len = list.len();list[(len + (0i32) as usize) % len]} = 3i32;
    // lst[0][lst[0].len()-1] = 5;
    // let mut x: i32 = 325;
    // let mut a: i32 = 5;
    // let mut b: &mut i32 =  &mut a;
    // func(b);
    // println!("{}", Box::new(a));
}

enum i32__or__bool { _i32(i32),_bool(bool), }

trait __len__ {
    fn __len__(self: &Self) -> i32;
}









fn ffunc(mut a: i32__or__bool) {
    ();
}
fn mainn() {
    ffunc(i32__or__bool::_i32(1));
}
