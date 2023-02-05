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
#     type Inner
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
# def f(*a: int):
#     pass
#
struct A:
    def __init__(self, x: int):
        self.x = x
    def get(self: &Self) -> int:
        x = self.x
        return x

def fnc(a: int, b: bool):
    print(a, b)
def main():
    lst = [1, 2, 3, 4]
#     b = iter(lst)
    for i in iter_imut(lst):
        print(i)
    print(lst)
    b := 6 not in lst
#     print(b)