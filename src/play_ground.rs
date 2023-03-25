use std::any::Any;
use std::fmt::{Debug, Display, Formatter, Error};
use std::ptr;

trait Trt {
    type Inner;
    fn df(&self, a: Self::Inner);
}

struct S {}
impl Trt for S {
    type Inner = i32;
    fn df(&self, a: i32) {

    }
}

fn f<'a, T: Iterator<Item=&'a i32>>(f: T) {
    for i in f {
        println!("{i}")
    }
}

fn fc(f: Box<&dyn Trt<Inner=i32>>) {

}
fn a() {
    let s = S {};
    s.df(32);
    fc(Box::new(&s));
    let ps = &s;
    let v = vec![1, 2];
    let v2 = v.iter();
    f(v2);
}
