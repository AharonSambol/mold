# TODO doesnt seem to check that func\struct that takes 2 of same generic are actually same typ

struct Same<T, G>:
    def __init__(self, a: T, b: G, c: T):
        self.a = a
        self.b = b
        self.c = c

struct A<T, I>:
    def __init__(self, b: T, i: I):
        self.i = i
        self.b = b


struct B<T>:
    def __init__(self, x: T):
        self.x = x

def generic_test<T, G>(a: T, t: G) -> A[B[T], G]:
    return A(B(a), t)

def same_t<T>(a: T, b: T) -> T:
    return a

def main():
    a = generic_test(A(1, False), "wow")
    s = Same(B(1), False, B(2))
    x = s.a.x
    a: Same[int, bool] = Same(1, False, 2)
    sm = same_t(1, 12)

