struct B:
    inner: int

struct A:
    inner: B
    def get() -> int:
        a := self.inner.inner
        return a


def func(a: &mut int):
    *a = *a + 1

# def get_ptr(a: int) -> &mut int:
#     return &mut a

def main():
    a := A { B { 2 } }
    b := a.get()
    print(b)
#     x := 325
#     a := 5 + -(4 / -2)
#     a_val := a + 1
#     b := &mut a
# #     c := get_ptr(a)
# #     d: &mut int = b
# #     c : List[&mut int] = [&mut x, b]
#     func(b)
#     func(&mut a)
#     print(a)