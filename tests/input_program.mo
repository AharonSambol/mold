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
enum E:
    a, b(int), c, d
def main():
    a = E.d
    match a:
        case E.a:
            print("a")
        case E.b(x):
            print("b=", x)
        case E.c:
            pass
        case E.d:
            pass
    a: int | bool | str = false
    match a:
        case int:
            print("a")
        case bool:
            print("b=")
        case _:
            pass
