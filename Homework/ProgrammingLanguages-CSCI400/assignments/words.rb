#This class handles loading and random sampling from a list of words.
#If a filename is not provided, attempts to read from "otherWords.txt"
#Properties:
# => @wordList 		The list of loaded words
class Words
  
	#loads a list of words from a file.
	#If no filename is specified, attempts to load "otherWords.txt"=end
	def load(filename = "otherWords.txt")
		@wordList = Array.new
		File.foreach(filename) { |line| line.split().each {|word| @wordList << word}}
	end


	#Gets a random word from the list of words.
	#Requires that @wordList instance variable is defined (i.e. call `load` first)
	def getWord()
		return @wordList.sample
	end
end