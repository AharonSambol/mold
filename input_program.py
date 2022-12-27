def main():
#     s := generic_test(m"12")
    s := [1, 2, 3, 9]
    sm := 0
    for i in s:
        sm = sm + i
    print(sm)
#     s := [1, 2, 3]
#     s.append(4)
#     dprint(s)

# def generic_test<T>(a: T) -> T:
#     return a
#     for i in input.split("  "):
#         dprint(i)
#     mx := [0, 0, 0, 0]

#     sm := 0
#     for i in input.split('\n'):
#         if i == "":
#             i := 1
#             while i < 4 and sm > mx[i]:
#                 mx[i - 1] = mx[i]
#                 i = i + 1
#             mx[i - 1] = sm
#             sm = 0
#         else:
#             sm = sm + int(i)
#     print(mx[1] + mx[2] + mx[3])

# struct A<T>:
#     b: T
#     def into_iter() -> T:
#         pass