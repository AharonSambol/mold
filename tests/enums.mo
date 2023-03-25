enum Color:
    RED(int, float)
    BLUE(bool)
    BLACK

enum Gen[T]:
    Opt1(T)
    Opt2

def main():
    a = Color.RED(11, 2.9)
    a = Color.BLUE(False)
    a = Color.BLACK
    b = Gen.Opt1(1)
    b: Gen[int] = Gen.Opt2
