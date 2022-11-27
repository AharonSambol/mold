def main():
    a := [1, 2, 3, 3]
    for i in a:
        print(i)
#     print(res)

def fib(x: int) -> int:
    if x == 0 or x == 1:
        return x
    return fib(x - 1) + fib(x - 2)

