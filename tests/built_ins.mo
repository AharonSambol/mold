def main():
    imut lst := [1,2,3,4]
    print(min(lst))
    # print(max(iter_imut(lst)))
    print(len(&lst))
    #{
    print(abs(-3.2))
    print(sum(lst))
    a = 2
    b = 4
    print(pow(a, b cast u32, 1000u32))
    for i in reversed(iter_imut(lst)):
        print(i)

    b := 6 not in lst
    print(b)