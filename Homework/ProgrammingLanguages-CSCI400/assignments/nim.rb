#!/usr/bin/ruby

#Reflection - We fetch all subclasses of Player that end in
#'_computer' and then use 'eval()' to construct the ai based
#on user input

#Author: Brennan W. Fieck
#Collaborators: Nick Zustak
#For what it's worth, I don't think it said anywhere,
#but we assumed this was a misere game, because
#it seemed at first glance that otherwise it'd be trivial.
#Not sure anymore if that's true.


#This class handles running the game and determining
#win conditions (or more accurately losing conditions)
class Nim

	#starts with just the two boards defined
	#in the prompt, and no players
	def initialize
		@boards = [[4,3,7],[1,3,5,7]]
		@players = []
	end

	#this just chooses the board to use
	#and fills it with sticks
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

	#keep playing until a loser is found
	def play
		gameOver= -1
		until gameOver > -1
			gameOver=play_round
		end
		print("The #{@players[gameOver].class.name} has lost.\n")
	end

	#play a single round of the game, checking if
	#a player takes the last stick
	def play_round
		for i in (0..1)
			@players[i].take_turn(@board)
			gameOver=true
			for x in @board
				if x.any?
					gameOver=false
					break
				end
			end
			if gameOver
				return i
			end
		end
		return -1
	end

	#I just used this for debugging purposes, pay it no mind
	def debug_dump
		print @boards
		print @players
		print @board
	end

	#Add a single player to the game
	def add_player(player)
		@players<<player
	end

end


#Something for computers to inherit from
#also knows how to print the board since
#currently there's no reason a game between
#two humans can't be played
class Player

	#print the board
	def print_board(board)
		for i in 0..(board.length-1)
			print("Row #{i}: ")
			board[i].each {|entry| print entry}
			print("\n")
		end
	end

	#I use this to generate the list of available ai's
	def self.descendants
    	ObjectSpace.each_object(Class).select { |klass| klass < self }
  	end
end


#The human player type, which can take a turn interactively
#and not much else.
class Human < Player

	#Take user input to define a turn
	def take_turn(board)

		#Show them the board and ask for a row selection
		#and of course pedantically check that they enter
		#a real number, that that number is an actual row
		#number, and that the row they select is not empty
		print_board(board)
		print("select row (0-#{board.length-1}): ")
		rowchoice=STDIN.gets.chomp()
		until /\A\d+\Z/.match(rowchoice) and (0..(board.length-1)).member?(rowchoice.to_i) and board[rowchoice.to_i].any?
			print("\nInvalid choice. Please enter an integer between 0 and #{board.length-1} (please also do not choose an empty row)")
			print("\nselect row: ")
			rowchoice=STDIN.gets.chomp
		end
		print("\n")
		rowchoice = rowchoice.to_i


		#same as above, but with number of sticks this time
		print("Choose number of sticks to take (1-#{board[rowchoice].length}): ")
		stickchoice=STDIN.gets.chomp
		until /\A\d+\Z/.match(stickchoice) and (1..(board[rowchoice].length)).member?(stickchoice.to_i)
			print("\nInvalid choice. Please enter an integer between 1 and #{board[rowchoice].length}.\n")
			print("Choose number of sticks to take: ")
			stickchoice=STDIN.gets.chomp
		end
		print("\n")
		stickchoice=stickchoice.to_i

		#now #JustDoIt
		board[rowchoice].pop(stickchoice)
	end
end


#This AI will theoretically win every game
class Smart_computer < Player

	#Take a single turn
	def take_turn(board)

		#first XOR everything to see what needs to be done
		nim_sum=0
		board.each{|row| nim_sum ^= row.length}

		#Now reduce a row where the XOR of that row length
		#with nim_sum is the amount to reduce it to (in 
		#the case that said amount is actually less than 
		#the current number of sticks)
		rowchoice = 0
		stickchoice = 1
		for i in (0..(board.length-1))
			if nim_sum ^ board[i].length < board[i].length
				stickchoice = board[i].length - (nim_sum ^ board[i].length)
				rowchoice = i
				break
			end
		end

		#Now we need to check if this move would leave no heap of size >= 2
		allUnderTwo = true
		for i in (0..(board.length-1))
			heap_size = board[i].length
			if i == rowchoice
				heap_size -= stickchoice
			end
			if heap_size >= 2
				allUnderTwo = false
				break
			end
		end

		if allUnderTwo

			#in this case, leave an odd number of heaps with one stick
			#first, count the rows that would have only one stick after this move
			onlyOne = 0
			for i in (0..(board.length-1))
				numsticks = board[i].length
				if i == rowchoice
					numsticks -= stickchoice
				end
				if numsticks == 1 
					onlyOne += 1
				end
			end

			#if there's an odd number, we're all good, else
			#there's work to do
			if onlyOne % 2 == 0

				#easy fix, don't take as many sticks
				if board[rowchoice].length > 2
					stickchoice -= 1
				else
					#or take all the sticks
					stickchoice = board[rowchoice].length
				end
			end
		end
		board[rowchoice].pop(stickchoice)
		print("Smart_computer took #{stickchoice} sticks from row #{rowchoice}\n")
	end
end


#This computer just takes random amounts of sticks
#from random (non-empty) heaps
class Dumb_computer < Player

	#Take one turn
	def take_turn(board)

		#pick a random, non-empty row
		rows = Array.new
		for rowno in (0..(board.length-1))
			if board[rowno].length > 0
				rows << rowno
			end
		end
		rowchoice = rand(rows.length)

		#pick a random number of sticks and pop 'em
		stickchoice = 1+rand(board[rowchoice].length-1)
		board[rows[rowchoice]].pop(stickchoice)
		print("Dumb_computer took #{stickchoice} sticks from row #{rows[rowchoice]}.\n")
	end
end


#select board configuration, assume the user has
#severe brain damage
print("Please select board configuration: (0: [4,3,7], 1: [1,3,5,7]): ")
boardchoice = STDIN.gets().chomp()

until /0|1/.match(boardchoice)
	print("\nInvalid choice. Please enter a '1' or a '0'.\n")
	print("Please select board configuration: ")
	boardchoice = STDIN.gets().chomp()
end
print("\n")

boardchoice=boardchoice.to_i


#this was ridiculously difficult, I shouldn't have to make a
#separate array to get a string representation of the class
#names, but ruby refused to do it any other way.
#Anyway, just tryna get the user to select an AI
print("Please select opponent AI type, options below: \n")
ai_classes=Player.descendants.each.select{|klass| /.*_computer\Z/.match(klass.name)}
ai_types=Array.new
ai_classes.each{|klass| ai_types << klass.name.to_s}
puts ai_types
ai_choice=STDIN.gets().chomp()

until ai_types.member?(ai_choice)
	print("\nInvalid choice. Please enter a valid AI type.\n")
	print("Please select opponent AI type: ")
	ai_choice=STDIN.gets().chomp()
end

print("\nYou have chosen #{boardchoice} and #{ai_choice}\n")

#Now set everything up and play the game
game=Nim.new
game.choose_board(boardchoice)
game.add_player(Human.new)
game.add_player(eval("#{ai_choice}.new"))
print("\n")
game.play