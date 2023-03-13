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
def tp(x: Tuple[int, bool]):
    print(x)

def main():
    o: int | None = None
    t: tuple[int, bool] = (1, False)
    tp(t)
    #{
    a = (1, 2, 3)
    print(a)
    pa = &a
    x = 0
    b = &pa[x]
    print(b)

    # match on ref
