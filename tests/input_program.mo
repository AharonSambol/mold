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

def f(a: &int | bool):
    pass

# def f(x: Display | Debug):
    # pass

def main():
    a: List[int | bool] = [1, False]
    x: int | bool = 3
    x = false
    f(&3)
    # a.append("woooow")
    # for i in &a:
        # f(i)
      #  print(i)

