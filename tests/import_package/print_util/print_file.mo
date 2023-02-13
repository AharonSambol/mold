def pt():
    print("wow!")
    print(&[1, 2][0])
    func(1)
    func(1.1)

def func(a: int | float) -> float:
    return 2.3

struct A:
    def __init__(self, a: int):
        self.a = a
    def pt(self):
        print(self.a)