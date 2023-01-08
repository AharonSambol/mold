trait P:
    def p() -> i8

struct D(P):
    def P::p() -> i8:
        return 8

def f(s: Set[P]):
    pass

def main():
    a := {1, 2, 3, 4}
    b := { 1: "1" }
    a := [1, 2, 3]

