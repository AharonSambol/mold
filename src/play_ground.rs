use std::slice::Iter;

fn _a(){
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