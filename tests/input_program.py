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
trait It:
    type Inner
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

def ar() -> List[int]:
    return [1, 2, 3]

def change(lst: &mut List[int]):
    lst[0] = 100
def main():
    lst = {1: 22, 2: 0, 3: 2, 4: 2}
    imut lst := [[1,2],[3,4]]
#     res = lst[-1][0]
#     print(res)
#     l = [1, 2, 3, 4]
#     change(&mut l)
#     print(l)
#     print(min(lst))
#     print(max(iter_imut(lst)))
#     print(len(lst))
#     print(abs(-3.2))
#     lst := [1, 3, 4]
#     print(sum(lst))
#     a = 2
#     b = 4
#     c = pow(a, b, 2.3)
#     for i in reversed(iter(lst)):
#         print(i)
#     b = iter(lst)
#     print(lst)
#     b := 6 not in lst
#     print(b)