// use std::collections::{HashMap, HashSet};
// use std::slice::Iter;
//
// trait P<T> {
//     fn p(&self, t: T) -> Vec<T>;
// }
//
// #[derive(Eq)]
// struct D {
//
// }
// impl<T> P<T> for D {
//     fn p(&self, t: T) -> Vec<T> {
//         vec![t]
//     }
// }
//
// trait Trt {
//     fn a(&self) -> i32;
// }
//
// struct Strct {
//
// }
//
// impl Strct {
//     fn a(&self) -> i32 {
//         1
//     }
// }
// impl Trt for Strct {
//     fn a(&self) -> i32 {
//         self.a()
//     }
// }
//
//
// // impl P<i32> for D {
// //     fn p(&self, t: i32) -> Vec<i32> {
// //         vec![t]
// //     }
// // }
//
// impl PartialEq for D {
//     fn eq(&self, other: &Self) -> bool {
//         todo!()
//     }
// }
//
// fn _a(){
//     // let mut a: HashMap::<i32, Box<dyn P>> = HashMap::from([
//     //     (1i32, Box::new(D{ }) as Box<dyn P>),
//     // ]);
//     // let mut a: Vec<Box<dyn P>> = vec![
//     //     Box::new(D{ }),
//     //     Box::new(D{ }),
//     //     Box::new(D{ })
//     // ];
//     // let mut a: HashSet<Box<dyn P>> = HashSet::from([
//     //     Box::new(D{ }) as Box<dyn P>
//     // ]);
//     let b = "wow";
//     let _c = format!("{}{}", b, "1");
//     let v = vec![1, 2];
//     let r = v.iter().rev();
//     // let i = r.next();
// }
//
//
// // TODO multiple generics
// struct Rev<T> {
//     inner: T
// }
// // impl<T> Rev<T> where T: Iterator {
// //     fn into_iter(&self) -> <T as Iterator>::Item {
// //         self.inner.
// //     }
// // }
// fn dynamic(p: Box<dyn Printable>) {
//     p.print(1);
// }
//
// fn static_<T>(p: T) where T: Printable{
//     p.print(1);
// }
// trait Printable {
//     // type Inner;
//
//     fn print(&self, a: i32) -> bool;
// }
//
// struct St {
//     inner: String
// }
// impl Printable for St {
//     // type Inner = String;
//
//     fn print(&self, a: i32) -> bool {
//         println!("{}", self.inner);
//         false
//     }
// }
//


trait P {
    fn p(self: &mut Self) -> i8;
}
trait P2 {
    fn p(self: &mut Self) -> i8;
}
trait Duck {
    fn quack(self: &mut Self);
    fn swim(self: &mut Self);
}
#[derive(Debug, Clone)]
struct D {  }
impl D {
    fn quack(self: &mut Self) {
        ();
    }
}
impl P for D {
    fn p(self: &mut Self) -> i8 {
        return 1i8;
    }
}
impl P2 for D {
    fn p(self: &mut Self) -> i8 {
        return 2i8;
    }
}
#[derive(Debug, Clone)]
struct D2 {  }
impl D2 {
    fn quack(self: &mut Self) {
        ();
    }
    fn swim(self: &mut Self) {
        ();
    }
}
impl Duck for D2 {
    fn quack(mut self: &mut Self) {
        self.quack();
    }
    fn swim(mut self: &mut Self) {
        self.swim();
    }
}