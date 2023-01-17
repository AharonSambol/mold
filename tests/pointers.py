def func(a: &mut int):
    *a = *a + 1

def main():
    x := 325
    a := 5 + -(4 / -2)
    a_val := a + 1
    b := &mut a
    d: &mut int = b
    c : List[&mut int] = [&mut x, b]
    func(b)
    func(&mut a)
    print(a)