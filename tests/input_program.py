trait CC:
    type Item
    def a(self) -> Item
struct AA:
    type CC.Item = i32
    def a(self) -> i32:
        return 1
    def __init__(self):
        pass

#     def rng(a: int, b: int) -> Iterator[Item=I]:
#         return [a].into_iter()

# def bubble_sort(lst: List[int]):
#     a := range(0, 10, 2)
#     for i in range(10):
#         print(i)
# trait It:
#     type Inner
#     def next(self) -> Inner

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
def main():
#     lst: IList = [1, 2, 64, 214, 43, 56, 32, 3, 23, 5, 12, 532, 634, 2, 35, 34, 6]
#     a := len(lst)
    r := range(0, 10)
#     r := [1, 2, 3]
#     r : IntoIterator[Item=i32] = r
    for i in range(10):
#     for i in range(0, 10, 2):
#     for i in [1, 2, 3]:
        x := i
        print(i)
#     bubble_sort(lst)