enum Enm:
    a,
    b(int, bool),

def f(a: &(int | bool)):
    pass

def pt(enm: Enm):
    match enm:
        case Enm.a:
            print("a")
        case Enm.b(x, y):
            print("b:", x, ",", y)

def pt_of(val: int | bool):
    match val:
        case bool as b:
            print("bool: ", b)
        case int as i:
            print("int: ", i)

def tst(x: int | bool | str):
    pass

def main():
    ww: int | bool = false
    ww: bool | int = false
    tst(ww)
    a: List[int | bool] = [1, False]
    f(&a[0])
    pt_of(4)
    pt_of(false)
    pt(Enm.a)
    pt(Enm.b(1, false))
