# TODO doesnt seem to check that func\struct that takes 2 of same generic are actually same typ

struct Same<T, G>:
    a: T
    b: G
    c: T

struct A<T, I>:
    b: T
    i: I

struct B<T>:
    x: T

def generic_test<T, G>(a: T, t: G) -> A[B[T], G]:
    return A { B { a }, t }

def same_t<T>(a: T, b: T) -> T:
    return a

def main():
    a := generic_test(A { 1, False }, "wow")
    s := Same { B { 1 }, False, B { 2 } }
    x := s.a.x
    a: Same[int, bool] = Same { 1, False, 2 }
    sm := same_t(1, False)