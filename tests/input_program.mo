# trait CC:
#     type Item
#     def a(self) -> Item
# struct AA(Display):
#     type CC.Item = i32
#     def a(self) -> i32:
#         return 1
#     def __init__(self):
#         pass
#
#     def rng(a: int, b: int) -> Iterator[Item=I]:
#         return [a].into_iter()
#
# trait It:
#    type Inner
#     def next(self) -> Inner
#
# trait AAAA:
#     type WWW
#     def __next__(self: &mut Self) -> i32
# struct Vc:
#     type WWW
#     def __init__(self):
#         pass
#     def __next__(self: &mut Self) -> i32:
#         return 1
#
# type i = int

# type IList = List[i]


def print_board(board: &List[str]):
	res = ""	 # first we make an empty string
	for x in range(9):
		element: str = clone(&board[x])
		if element == " ":  # if the element is not set (to X or O)
			# then add the number of the position
			res = res + "*"
		else:   # otherwise add the shape to the result
			res += element
		if x % 3 == 2:  # if it’s 2, 5 or 8 then add an enter after it
			res += "\n"
		else:   # otherwise add a pipe
			res += " | "
	print(res)  # and finally print out our string
	# we don’t need to return anything from this function

def get_winner(board: &List[str]) -> str:
	for shape in ["X", "O"]:
		for r in range(3):
			if board[r * 3] == shape and board[r * 3 + 1] == shape and board[r * 3 + 2] == shape:
				return shape
		for r in range(3):
			if board[r] == shape and board[r + 3] == shape and board[r + 6] == shape:
				return shape
			if (board[0] == shape and board[4] == shape and board[8] == shape) or (board[2] == shape and board[4] == shape and board[6] == shape):
				return shape
	return ""	# no one won
def get_input() -> int:
    return 1

def main():
    board =  ["X", "O", "X", " ", "O", " ", " ", "O", " "]
    print_board(&board)
    print("winner:", get_winner(&board))

    for i in range(9):
    	pos = get_input()
    	if i % 2 == 0:
    		board[pos] = "X"
    	else:
    		board[pos] = "O"
    	print_board(&board)
    	# this will return the shape of the winner, if there is none it will return an empty string ""
    	winner = get_winner(&board)
    	if winner != "":
    		print(winner, "won!")
    		return
    print("it's a draw")

    # {
    o: int | None = None
    op = &o
    match op:
        case int as i:
            print(i)
        case None:
            print("None")
