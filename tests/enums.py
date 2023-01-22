enum Color:
    RED(int, float)
    BLUE(bool)
    BLACK

def main():
    a := Color.RED(11, 2.9)
    a := Color.BLUE(False)
    a := Color.BLACK
    b := Option.Some(1)
    b : Option[int] = Option.None
