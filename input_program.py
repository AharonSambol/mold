def main():
    res := fib(39)
    print(res)

def fib(x: int) -> int:
    if x == 0 or x == 1:
        return x
    return fib(x - 1) + fib(x - 2)

