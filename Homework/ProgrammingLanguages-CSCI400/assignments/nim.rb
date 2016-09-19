#!/usr/bin/ruby

class Nim
	def initialize
		@boards = [[4,3,7],[1,3,5,7]]
		@players = []
	end
	def choose_board(boardno)
		boardtmpl=@boards[boardno]
		@board =[]
		for i in 0..(boardtmpl.length-1)
			line = []
			for j in 0..(boardtmpl[i]-1)
				line << "X"
			end
			@board<<line
		end
	end
	def play
		gameOver= -1
		until gameOver > -1
			gameOver=play_round
		end
		print("The #{@players[gameOver].name} has won.\n")
	end
	def play_round
		for i in (0..1)
			@board=@players[i].take_turn(@board)
			if !@board.any?
				return i
			end
		end
		return -1
	end
	def debug_dump
		print @boards
		print @players
		print @board
	end
	def add_player(player)
		@players<<player
	end

end

class Player
	def print_board(board)
		for i in 0..(board.length-1)
			print("Row #{i}: ")
			board[i].each {|entry| print entry}
			print("\n")
		end
	end
end

class Human_player < Player
	def name
		return "Human"
	end
	def take_turn(board)
		print_board(board)
		print("select row: (0-#{board.length-1})")
		rowchoice=STDIN.gets.chomp()
		until /\A\d+\Z/.match(rowchoice) and (0..(board.length-1)).member?(rowchoice.to_i) and board[rowchoice.to_i].any?
			print("\nInvalid choice. Please enter an integer between 0 and #{board.length-1} (please also do not choose an empty row)")
			print("\nselect row: ")
			rowchoice=STDIN.gets.chomp
		end
		print("\n")
		rowchoice = rowchoice.to_i
		return board
	end
end

class Smart_computer < Player
	def name
		return "Smart_computer"
	end
	def take_turn(board)
		return board
	end
end

class Dumb_computer < Player
	def name
		return "Dumb_computer"
	end
	def take_turn(board)
		return board
	end
end


print("Please select board configuration: (0: [4,3,7], 1: [1,3,5,7]): ")
boardchoice = STDIN.gets().chomp()

until /0|1/.match(boardchoice)
	print("\nInvalid choice. Please enter a '1' or a '0'.\n")
	print("Please select board configuration: ")
	boardchoice = STDIN.gets().chomp()
end
print("\n")

boardchoice=boardchoice.to_i

print("Please select opponent AI type ('Smart_computer' or 'Dumb_computer'): ")
ai_choice=STDIN.gets().chomp()

until ai_choice == "Smart_computer" or ai_choice == "Dumb_computer"
	print("\nInvalid choice. Please enter either 'Smart_computer' or 'Dumb_computer'.\n")
	print("Please select opponent AI type: ")
	ai_choice=STDIN.gets().chomp()
end

print("\nYou have chosen #{boardchoice} and #{ai_choice}\n")

game=Nim.new
game.choose_board(boardchoice)
game.add_player(eval("#{ai_choice}.new"))
game.debug_dump
print("\n")
game.add_player(Human_player.new)
game.play