# trait CC:
#     type Item
#     def a(self) -> Item
# struct AA(Display):
#     type CC.Item = i32
#     def a(self) -> i32:
#         return 1
#     def __init__(self):
#         pass
#
#     def rng(a: int, b: int) -> Iterator[Item=I]:
#         return [a].into_iter()
#
# trait It:
#    type Inner
#     def next(self) -> Inner
#
# trait AAAA:
#     type WWW
#     def __next__(self: &mut Self) -> i32
# struct Vc:
#     type WWW
#     def __init__(self):
#         pass
#     def __next__(self: &mut Self) -> i32:
#         return 1
#
# type i = int

# type IList = List[i]


struct St[T, G, I]:
    def __init__(self, g: G, t: T, i: I):
        self.i = i
        self.t = t
        self.g = g

enum E1:
    A(int, bool)
    B
enum E2:
    A
    B
def fu(v: List[int]):
    a: int | bool = 1
    match a:
        case int:
            pass
        case bool as b:
            pass

def ff(a: bool):
    pass
def main():
    # TODO it thinks this is ok (default args?) a = St(1)
    # TODO print(1 + 5 $ 6666) should have an error
    # TODO error when not put () on enum in case e.g. E.A(x)
    xa = E1.A(1, False)
    a = St(1, False, "wow")
    x = 21
    if x > 10:
        x = 0
    else:
        b = 2
    print(1 + 5 $ 6666)
#{









trait Trt[T=int]:
    def trt_func(self, t: T) -> T

struct Stc(Trt[T=bool]):
    def __init__(self, x: int):
        self.x = x
        pass
    def trt_func(self, t: bool) -> bool:
        return False

def a[G](t: Trt[T=G]):
    pass

def main():
    a(Stc(2))
    vc = [1, 2, 3, 4]
    it = iter(vc)
    # why does it let me pass a str to int()?? (not &str)

    #{
    print(1)
    i = iter([1, 2])
    print(len(i))
    print(2)
    #{
    o: int | None = None
    op = &o
    match op:
        case int as i:
            print(i)
        case None:
            print("None")
