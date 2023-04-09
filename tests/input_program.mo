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

def f(*args: Debug):
    pass

def main():
    # TODO max(iter([1]))
    # TODO a: List[tuple[int, bool, int] | List[int | str]] = [(1, False, 4), [2, "3"]]
    # TODO a: List[dict[int, bool | str] | List[int | str]] = [{1: False, 2: "T"}, [2, "3"]]
    # TODO a: List[Set[int | bool] | List[int | str]] = [{1, False, 4}, [2, "3"]]
    # a: List[List[int | bool] | List[int | str]] = [[1, False, 4], [2, "3"]]
    a = [[1, 4], [2, "3"]]
    # b = clone(&a[0])


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
