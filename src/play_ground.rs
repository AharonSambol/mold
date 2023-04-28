use std::any::Any;
use std::fmt::{Debug, Display, Formatter, Error};
use std::ptr;
use crate::construct_ast::mold_ast::Info;

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

enum MutateInfo<'a> {
    Box(&'a mut Info<'a>),
    None
}

fn fc(f: Box<&dyn Trt<Inner=i32>>) {

}
fn ffff(p: MutateInfo) {}
fn a(mut p: MutateInfo) {

    // for i in 0..10 {
    //     if let MutateInfo::Box(b) = p {
    //         ffff(MutateInfo::Box(b));
    //         p = MutateInfo::Box(b);
    //     }
    // }
    let s = S {};
    s.df(32);
    fc(Box::new(&s));
    let ps = &s;
    let v = vec![1, 2];
    // println!("{:?}", v.fmt());
    let v2 = v.iter();
    f(v2);
}
