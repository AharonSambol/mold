def main():
    a := St { 1, Inner{ 2, false } }
#     x := a.wow().param
#     p := ((a.par2.param) + 1)
    p := ((a.par2).param + 1)



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

    def wow() -> Inner:
        param = par2.param
        return Inner { 100, false}