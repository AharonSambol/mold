def main():
#     a := [4, 1, 2, 3, 3, 1]
    st := St { 1, Inner { 2, true } }
    a := st.par2.a()
    wow()
#     a.sort()
#     dprint(a)
#     for i in enumerate(range(10, 13)):
#         dprint(i, "wow", 1 + 2)
#     c := 1
#     c = wow(1, 2)

def wow():
    return
#
# def fib(x: int) -> int:
#     s := x + x
#     if x == 0 or x == 1:
#         return x
#     return fib(x - 1) + fib(x - 2)

struct Inner:
    param: int
    par2: bool
    def a() -> float:
        return 2.3

struct St:
    param: int
    par2: Inner
    def to_str(a: int, b: int) -> int:
        return a

    def wow():
        pass
        pass