trait P:
    def p() -> i8

trait P2:
    def p() -> i8

TRAIT Duck:
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

def f(s: Set[P]):
    pass

def main():
    a := {1, 2, 3, 4}
    b := { 1: "1" }
    a := [1, 2, 3]

