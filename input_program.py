trait Printable:
    def to_str() -> str

struct St(Printable):
    inner: str
    def Printable::to_str() -> str:
        return inner.clone()

struct Wow:
    def Printable::to_str() -> str:
        return "wow"
    static def new() -> Wow:
        return Wow { }
struct Wow2:
    def Printable::to_str() -> str:
        return "wow"
    static def new() -> Wow:
        return Wow { }

def f(a: Printable):
    print(a.to_str())

def main():
    # TODO need to box variables in other places too not only args
#     a: int = 12
#     a: List[int] = [1, 2, a]
#     a: A[int, bool] = A{ 1, False , 2}
#     v: List[St] = [St{ "some str" }]

#     a := Wow { }
#     v := A{ 1, False, 2}
#     f := v.a
    a: Wow = Wow {}
    v: List[Wow] = [Wow { }, a]
    for i in v:
        f(i)
#     a: Printable = Wow {}
#     f(a)
#     a := Wow {}
    a: Wow = (func())
#     f(func())

def func() -> Wow:
    return Wow {}
# pass
struct A<T, G>:
    a: T
    b: G
    c: T

#     a := A{ 1, False, "w", 3.4 }
#     s := generic_test(1, False)
#     s := generic_test(m"12", False)
#     s: List[List[int]] = [[1, 2, 3, 9], [1, 5]]
#     b:= s[0][0]
#     for i in s:
#         x:=i
#         print(i)
#     sm := 0
#     s.append([33, 3])
#     r := s.iter_mut()
#     r := reversed(s.iter())
#     ww := r.into_iter()
#     for i in reversed(s.iter_mut()):
#         i.append(99)
#         ss:=i
#         dprint(i)
#         show := i
#         for b in i:
#             sm = sm + b
#     s := [1, 2, 3]
#     s.append(4)
#     dprint(s)


# def generic_test<T, I>(a: T, b: I) -> T:
#     return a
#     for i in input.split("  "):
#         dprint(i)
#     mx := [0, 0, 0, 0]

#     sm := 0
#     for i in input.split('\n'):
#         if i == "":
#             i := 1
#             while i < 4 and sm > mx[i]:
#                 mx[i - 1] = mx[i]
#                 i = i + 1
#             mx[i - 1] = sm
#             sm = 0
#         else:
#             sm = sm + int(i)
#     print(mx[1] + mx[2] + mx[3])



# def generic_test<T, G>(a: T, t: G) -> A[B[T], G]:
#     return A { B { a }, t }
#
# struct A<T, I>:
#     b: T
#     i: I
#
# struct B<T>:
#     x: T