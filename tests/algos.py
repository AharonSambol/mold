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


def bin_search_rec(lst: &List[int], elem: int, start: int, end: int) -> int:
    if start <= end:
        mid = (start + end) // 2
        if lst[mid] == elem:
            return mid
        elif lst[mid] < elem:
            return bin_search_rec(lst, elem, mid + 1, end)
        return bin_search_rec(lst, elem, start, mid - 1)
    return -1


def bubble_sort(lst: &mut List[int]):
    for j in range(len(lst)):
        for i in range(1, len(lst) - j):
            if lst[i-1] > lst[i]:
                temp = lst[i]
                lst[i] = lst[i-1]
                lst[i-1] = temp


def main():
    lst = [1, 2, 64, 214, 43, 56, 32, 3, 23, 5, 12, 532, 634, 2, 35, 34, 6, 391, 34, 436, 457, 37, 35, 34743, 457, 3475, 643, 7835, 43]
    new_lst: List[int] = []
    for x in range(100):
        for i in iter(lst):
            new_lst.append(*i + x)
    bubble_sort(&mut new_lst)
    print(new_lst)
    print("START")
    for x in range(10):
        for num in iter_imut(new_lst):
#             i = bin_search(&new_lst, *num)
            i = bin_search_rec(&new_lst, *num, 0, len(new_lst) - 1)
            print(i)
    print("END")