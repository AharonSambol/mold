
def print_board(board: &List[str]):
	res = ""
	for x in range(9):
		element: str = clone(&board[x])
		if element == " ":
			res += str(x + 1)
		else:
			res += element
		if x % 3 == 2:
			res += "\n"
		else:
			res += " | "
	print(res)

def get_winner(board: &List[str]) -> str:
	for shape in ["\x1b[6;30;41mX\x1b[0m", "\x1b[6;30;42mO\x1b[0m"]:
		for r in range(3):
			if board[r * 3] == shape and board[r * 3 + 1] == shape and board[r * 3 + 2] == shape:
				return shape
		for r in range(3):
			if board[r] == shape and board[r + 3] == shape and board[r + 6] == shape:
				return shape
			if (board[0] == shape and board[4] == shape and board[8] == shape) or (board[2] == shape and board[4] == shape and board[6] == shape):
				return shape
	return ""

def get_input(board: &List[str]) -> int:
	pos = input("enter move: ")
	while pos not in ["1", "2", "3", "4", "5", "6", "7", "8", "9"] or board[int(&pos) - 1] != " ":
		print("that’s an invalid move")
		pos = input("enter move: ")
    pos := int(&pos) - 1
	return pos

def main():
    board =  [" ", " ", " ", " ", " ", " ", " ", " ", " "]
    print_board(&board)

    for i in range(9):
    	pos = get_input(&board)
    	if i % 2 == 0:
    		board[pos] = "\x1b[6;30;41mX\x1b[0m"
    	else:
    		board[pos] = "\x1b[6;30;42mO\x1b[0m"
    	print_board(&board)
    	winner = get_winner(&board)
    	if winner != "":
    		print(winner, "won!")
    		return
    print("it's a draw")
