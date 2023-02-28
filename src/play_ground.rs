use std::any::Any;
use std::fmt::{Debug, Display, Formatter, Error};
use std::ptr;

pub enum int_or_bool {
    i(i32),
    b(bool),
}
pub enum _boxof_Display_endof___or___boxof_Debug_endof_  { __boxof_Display_endof_(Box<dyn Display>),__boxof_Debug_endof_(Box<dyn Debug>) }

impl Display for int_or_bool {
    fn fmt(self: &Self, f: &mut Formatter<'_>) -> Result<(), Error> {
        todo!()
    }
}
impl Debug for int_or_bool {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            int_or_bool::i(x) => Debug::fmt(x, f),
            int_or_bool::b(x) => Debug::fmt(x, f),
        }
    }
}
fn a() {
    let x = int_or_bool::b(false);
    println!("{:?}", x);
}

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
enum IIOrI <T> {
    // II(Box<dyn IntoIterator::<Item=T, IntoIter=G>>),
    I(Box<dyn Iterator::<Item=T>>)
}

#[inline] fn index<T>(vc: &mut Vec<T>, pos: i32) -> &mut T {
    if pos < 0 {
        vc.iter_mut().rev().nth(-pos as usize).unwrap()
    } else{
        vc.iter_mut().nth(pos as usize).unwrap()
    }
}
fn func<T>(a: T) -> Box<dyn Iterator<Item=i32>> {
    let mut vc = vec![1, 2, 3, 4, 5];
    *index(&mut vc, -1) = 7;
    println!("{:?}", vc);
    // let a = IIOrI::I(
    //     Box::new(vc.iter())
    // );
    // if let IIOrI::I(mut a) = a {
    //     // let b = vec![1].into_iter();
    //     a.for_each(|i|
    //         println!("{i}")
    //     );
    // }
    // let b = *a;
    return Box::new(vec![1].into_iter());
}
fn ln(a: &Box<dyn Debug>) {}
fn ff(a: &Vec<i32>) {
    // ln(&(Box::new(a) as Box<dyn Debug>));
}
fn main() {
    let a = 1i32.pow(3u32);
    let a = vec![1, 2, 3];
    let s = a.iter().sum::<u32>();
    let a = 1.2f32;
    let b = a;
    let a = a.abs();
    // print!("{}", ptr::eq(a, b));

    // let mut v = vec![1];
    let a = unsafe { let a = &mut vec![vec![1]][0] as *mut Vec<_>; (*a)[0] };
    println!("{a:?}");
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



fn bubble_sort(mut lst: &mut Vec::<i32>) {
    for mut j in (0..(lst.len() as i32)) {
        for mut i in (1i32..(lst.len() as i32) - j) {
            if unsafe {
                let list = &lst as *const &mut Vec::<i32>;
                let len = (*list).len();(*list)[{
                    let pos =i - 1i32; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                }]
            } > unsafe {
                let list = &lst as *const &mut Vec::<i32>;
                let len = (*list).len();
                (*list)[{
                    let pos =i; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                }]
            } {
                let mut temp: i32 = unsafe {
                    let list = &lst as *const &mut Vec::<i32>;
                    let len = (*list).len();(*list)[{
                        let pos =i; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                    }]
                };
                unsafe {
                    let mut list: *mut &mut Vec::<i32> = &mut lst;
                    let len = (*list).len();
                    let val= unsafe {
                        let list = &lst as *const &mut Vec::<i32>;
                        let len = (*list).len();(*list)[{
                            let pos =i - 1i32; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                        }]
                    };
                    (*list)[{
                        let pos=i; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                    }] = val
                };
                unsafe {
                    let mut list: *mut &mut Vec::<i32> = &mut lst;
                    let len = (*list).len();
                    let val=temp;
                    (*list)[{
                        let pos=i - 1i32; if pos >= 0 { pos as usize } else { (pos + len as i32) as usize }
                    }] = val
                };
            };
        };
    };
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
