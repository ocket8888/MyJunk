#!/usr/bin/ruby
require "./words.rb"

#This class handles playing a game of hangman, it depends on
#the Words class, so that it can load a word list and get a
#random one to use as the word to guess.
#Properties:
# => @words 		The Words object the game uses
# => @theWord		The word the player attempts to guess
# => @currentWord	The current word that the player has guessed; unknown letters render as underscores
# => @guesses 		The list of already-guessed letters
# => @trys			The number of remaining guesses
class Hangman


	#Initializes a new hangman game, reading from a file
	#(optionally) specified by arg0 to find a list of words
	#and optionally using arg1 to set the number of allowed tries
	def initialize(arg0, arg1)
		@words = Words.new
		@words.load(arg0)
		@theWord = @words.getWord
		@currentWord = String.new
		@guesses = Array.new
		@theWord.each_char {|c| @currentWord << "_"}
		@trys = if (arg1 == nil || arg1 == "" || arg1 == 0 || arg1 =="0") then 6 else arg1 end
	end

	#Main function that plays the game, returns true if the
	#player won the game, or false if they lose or quit.
	def play
		dump
		until @trys == 0 || @currentWord == @theWord
			playRound
		end
		return @currentWord == @theWord
	end


	#Plays a single round of the game, handling guess input
	#and passing off control to word guessing and guess handling
	#subroutines as appropriate
	def playRound
		display
		print "Enter guess (!q to quit, !g to guess word): "
		guess = STDIN.gets.chomp
		print "\n"
		if guess == "!q"
			print "quitting...\n"
			@trys = 0
			return
		elsif guess == "!g"
			guessWord
			@trys = 0
			return
		end

		until guess.length == 1
			print "You can only guess one character at a time!\nEnter guess: "
			guess = STDIN.gets.chomp
			print "\n"
		end
		guess = guess.downcase
		@guesses << guess

		handleGuess( guess )
	end



	#I use this to debug, has no real purpose
	def dump
		print "\n\nThe Word: ",  @theWord, "\nGuesses: ", @guesses, "\nCurrent Word: ", @currentWord, "\nTries: ", @trys, "\n\n"
	end


	#Displays the current word and guesses, as well as tries remaining.
	def display
		print "==========================================\n"
		print "==\t\t", @currentWord, "\t\t\t==\n"
		print "==========================================\n"

		if @guesses.length > 0
			print "You have guessed: "
			@guesses.each {|c| print c, ", "}
			print "\n"
		end
		print "You have ", @trys, " guesses remaining.\n"
	end

	#Handles when a player wants to guess the word, sets @currentWord to whatever they
	#guess (since the game is over anyway)
	def guessWord
		print "Guessing word\n"
	end


	#Handles a guessed character (c), including determining if it's correct and 
	#subtracting from @trys if it isn't, otherwise placing the guessed character
	#at the places it belongs in @currentWord.
	def handleGuess(c)
		if (@theWord.include?(c))
			theLetters = @theWord.chars
			theLetters.each_index {|i| if( theLetters[i] == c ) then @currentWord[i] = c end}
		else
			@trys-=1
		end
	end
end

game = Hangman.new(ARGV[0], ARGV[1])

ending = if (game.play) then "Congratulations, you won!\n" else "You have lost!\n" end

print ending