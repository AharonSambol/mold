def main():
    a := St { 1, Inner{ 2, false } }
    p := a.par2.param
    a.par2.param = 22
    dprint(a, a.par2, p)



struct Inner:
    param: int
    par2: bool
    def a():
        pass

struct St:
    param: int
    par2: Inner
    static def do_something(a: int, b: int) -> int:
        return a + b

    def wow():
        param = par2.param