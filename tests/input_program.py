# # trait CC:
# #     type Item
# #     def a(self) -> Item
# # struct AA(Display):
# #     type CC.Item = i32
# #     def a(self) -> i32:
# #         return 1
# #     def __init__(self):
# #         pass
#
# #     def rng(a: int, b: int) -> Iterator[Item=I]:
# #         return [a].into_iter()
#
# def bubble_sort(lst: &mut List[int]):
#     for j in range(0, len(lst), 1):
#         for i in range(1, len(lst), 1):
#             if lst[i-1] > lst[i]:
#                 temp := lst[i]
#                 lst[i] = lst[i-1]
#                 lst[i-1] = temp
# # trait It:
# #     type Inner
# #     def next(self) -> Inner
#
# # trait AAAA:
# #     type WWW
# #     def __next__(self: &mut Self) -> i32
# # struct Vc:
# #     type WWW
# #     def __init__(self):
# #         pass
# #     def __next__(self: &mut Self) -> i32:
# #         return 1
# #
# # type i = int
# # type IList = List[i]
# # enum Opt:
# #     Some(int)
# #     None
# def f(*a: int):
#     pass
#
# def main():
# #     f(1, 2, 3, 4)
#     lst := [1, 2, 64, 214, 43, 56, 32, 3, 23, 5, 12, 532, 634, 2, 35, 34, 6]
# #     l := lst.iter()
# #     ln := len(lst)
# #     ln := lst.len()
# #     a := len(lst)
# #     r := range(0, 10)
# #     lst := [1, 2, 3]
# #     r := r.iter_mut()
# #     r := reversed(r.iter())
# #     r : IntoIterator[Item=i32] = r
# #     for i in r:
# #     for i in range(0, 10, 2):
# #     for i in [1, 2, 3]:
# #         x := i
# #         print(*i, 2)
#     bubble_sort(&mut lst)
#     for i in lst:
#         print(i)


def bin_search(lst: &List[int], elem: int) -> int:
    start = 0
    end = len(lst) - 1
    while start <= end:
        mid = (start + end) // 2
        if lst[mid] == elem:
            return mid
        elif lst[mid] < elem:
            start = mid + 1
        else:
            end = mid - 1
    return -1


def bubble_sort(lst: &mut List[int]):
    for j in range(len(lst)):
        for i in range(1, len(lst) - j):
            if lst[i-1] > lst[i]:
                temp = lst[i]
                lst[i] = lst[i-1]
                lst[i-1] = temp

def bin_search_rec(lst: &List[int], elem: int, start: int, end: int) -> int:
    if start <= end:
        mid = (start + end) // 2
        if lst[mid] == elem:
            return mid
        elif lst[mid] < elem:
            return bin_search_rec(lst, elem, mid + 1, end)
        return bin_search_rec(lst, elem, start, mid - 1)
    return -1


def main():

    lst = [1, 2, 64, 214, 43, 56, 32, 3, 23, 5, 12, 532, 634, 2, 35, 34, 6, 391, 34, 436, 457, 37, 35, 34743, 457, 3475, 643, 7835, 43]
    new_lst: List[int] = []
    for x in range(6000):
        for i in lst.iter():
            new_lst.append(*i + x)
    bubble_sort(&mut new_lst)
    print(new_lst)
    print("START")
    for x in range(1000):
        for num in new_lst.iter():
            i := bin_search_rec(&new_lst, *num, 0, len(new_lst) - 1)
            print(i, new_lst[i], *num)
    print("END")
