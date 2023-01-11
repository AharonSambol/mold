trait Printable:
    def to_str() -> str

struct St(Printable):
    inner: str
    def Printable::to_str() -> str:
        return inner.clone()

struct Wow(Printable):
    def Printable::to_str() -> str:
        return "wow"
    static def new() -> Wow:
        return Wow { }

TRAIT P:
    def p() -> i8
TRAIT P2:
    def p() -> i8
trait Duck:
    def quack()
    def swim()
struct D:
    def quack():
        pass
    def P::p() -> i8:
        return 1
    def P2::p() -> i8:
        return 2
struct D2:
    def quack():
        pass
    def swim():
        pass


def f(a: Printable):
    print(a.to_str())

def main():
    a: Wow = Wow {}
    v: List[Wow] = [Wow { }, a]
    for i in v:
        f(i)
    v: List[Printable] = [St { "st" }, a]
    for i in v:
        f(i)
    a: Printable = (func(1))
    f(func())

def func(i: i8) -> Wow:
    return Wow {}
