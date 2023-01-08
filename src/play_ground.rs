use std::collections::{HashMap, HashSet};
use std::slice::Iter;

trait P {
    fn p(&self) -> i8;
}
#[derive(Eq)]
struct D {

}
impl P for D {
    fn p(&self) -> i8 {
        1
    }
}

impl PartialEq for D {
    fn eq(&self, other: &Self) -> bool {
        todo!()
    }
}

fn _a(){
    let mut a: HashMap::<i32, Box<dyn P>> = HashMap::from([
        (1i32, Box::new(D{ }) as Box<dyn P>),
    ]);
    // let mut a: Vec<Box<dyn P>> = vec![
    //     Box::new(D{ }),
    //     Box::new(D{ }),
    //     Box::new(D{ })
    // ];
    // let mut a: HashSet<Box<dyn P>> = HashSet::from([
    //     Box::new(D{ }) as Box<dyn P>
    // ]);
    let b = "wow";
    let _c = format!("{}{}", b, "1");
    let v = vec![1, 2];
    let r = v.iter().rev();
    // let i = r.next();
}


// TODO multiple generics
struct Rev<T> {
    inner: T
}
// impl<T> Rev<T> where T: Iterator {
//     fn into_iter(&self) -> <T as Iterator>::Item {
//         self.inner.
//     }
// }
fn dynamic(p: Box<dyn Printable>) {
    p.print(1);
}

fn static_<T>(p: T) where T: Printable{
    p.print(1);
}
trait Printable {
    // type Inner;

    fn print(&self, a: i32) -> bool;
}

struct St {
    inner: String
}
impl Printable for St {
    // type Inner = String;

    fn print(&self, a: i32) -> bool {
        println!("{}", self.inner);
        false
    }
}