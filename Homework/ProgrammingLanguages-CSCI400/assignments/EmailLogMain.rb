#!/usr/bin/ruby




#######################################
#####                             #####
#####  PREAMBLE AND DEFINITIONS   #####
#####                             #####
#######################################

require 'date'


	###################################
	###        Email Object         ###
	###################################

#The Definiton of an Email:
Email = Struct.new(:id, :time, :size, :from, :to)



	###################################
	###        Function Defs        ###
	###################################

#Does nothing more than print the list of emails
#passed to it, in a nicer format than the object
#dump that is ruby's default.
def printemails(email_list)
	email_list.each{|email|
		print "Email from #{email.from} "
		print "to #{email.to.join(', ')}, "
		print "on #{email.time.strftime('%A the %-dth of %b, %Y at %l:%M:%S %p')}."
		print " Size: #{email.size} kB,"
		print " Message Id: '#{email.id}'\n"
	}
end





	###################################
	###     EmailLogMain Object     ###
	###################################


#This class handles searching through
#email log files to construct a legible
#set of the messages sent as reported by
#the mail daemon therein.
class EmailLogMain

	###        static members       ###

	#This regex pulls the message's
	#unique identifier out of a line
	@@idRegEx = /(\d|[A-Z]){11}/

	#This regex pulls the day of the month
	#and time of day out of a logfile line
	@@datetimeRegEx = /(\d{2} \d{2}:\d{2}:\d{2})/

	#This regex pulls a size value out of
	#a logfile line
	@@sizeRegEx = /size=(\d+)/

	#This regex pulls a from address
	#out of a logfile line
	@@fromRegEx = /from=<(.+)>/

	#This regex pulls a message id
	#out of a logfile line
	@@midRegEx = /message-id=<(.+)>/

	#This regex pulls a to address out
	#of a logfile line
	@@toRegEx = / to=<(\S+)>/



	###          methods            ###

	#Loads the initial data. It will
	#read from the provided file, or
	#if no file is provided it will
	#attempt to read "mail.log" in the
	#working directory.
	def initialize(fname)
		@raw_data = IO.read(fname)
		@messages = Hash.new
	end


	#This method will find all of the unique
	#identifiers in the logfile then store
	#them to a hash. No, it's not the most
	#efficient way and yes, I could just
	#build the hash as I parse, but I don't
	#wanna. So there.
	def buildIDhash
		@raw_data.split("\n").each{|line|
			idstr = @@idRegEx.match(line)[0]
			@messages.store(idstr, Email.new) unless @messages.has_key?(idstr)
		}
	end


	#this is a utility method used by `parseLog`
	#to pull a date out of a line
	#It's useful to note that all dates are in
	#December (presumably last year)
	def getDate(line)
		dategrp = @@datetimeRegEx.match(line)
		if dategrp != nil
			datestr = dategrp[0]
			datearr = datestr.split(" ")
			timearr = datearr[1].split(":")
			return DateTime.new(2015, 12, datearr[0].to_i, timearr[0].to_i, timearr[1].to_i, timearr[2].to_i)
		end
		return nil
	end

	#this is a utility method used by `parseLog`
	#to pull a size out of a line
	def getSize(line)
		sizestr = @@sizeRegEx.match(line)
		return sizestr != nil ? sizestr[1].to_i : nil
	end

	#this is a utility method used by `parseLog`
	#to pull a from address out of a line
	def getFrom(line)
		fromstr = @@fromRegEx.match(line)
		return fromstr != nil ? fromstr[1] : nil
	end

	#this is a utility method used by `parseLog`
	#to pull a message id out of a line
	def getmId(line)
		midstr = @@midRegEx.match(line)
		return midstr != nil ? midstr[1] : nil
	end

	#this is a utility method used by `parseLog`
	#to pull a message id out of a line
	def getTo(line)
		tostr = @@toRegEx.match(line)
		return tostr == nil ? nil : tostr[1]
	end

	#This method handles getting all that sweet data
	#it expects that the hash of ids has already been
	#constructed.
	def parseLog
		@raw_data.split("\n").each{|line|
			
			lineid = @@idRegEx.match(line)[0]#exists on every line, if the regex is good, no need for a nil check

			#Get a date/time from the line if one exists
			#and if we do not already have a date/time
			if @messages[lineid].time == nil
				date = getDate(line)
				@messages[lineid].time = date unless date == nil
			end

			#Get a size from the line if one exists
			#and if we do not already have a size
			if @messages[lineid].size == nil
				size = getSize(line)
				@messages[lineid].size = size unless size == nil
			end

			#Get a from address from the line
			#if one exists and we don't already
			#have one
			if @messages[lineid].from == nil
				from = getFrom(line)
				@messages[lineid].from = from unless from == nil
			end

			#Get a message id if one exists, and
			#if it's not already defined on this
			#Email object
			if @messages[lineid].id == nil
				mid = getmId(line)
				@messages[lineid].id = mid unless mid == nil
			end

			#Get a to address if one exists,
			#and isn't already in the list of
			#to addresses on this Email
			to = getTo(line)
			if to != nil
				if @messages[lineid].to == nil
					@messages[lineid].to = Array.new(1,to)
				else
					@messages[lineid].to << to unless @messages[lineid].to.include?(to)
				end
			end
		}
	end


	#member variable accessors
	def raw_data
		return @raw_data
	end

	def messages
		return @messages
	end

	def messageCount
		return @messages.length
	end
end





#######################################
#####                             #####
#####       SCRIPT EXECUTION      #####
#####                             #####
#######################################

fname = "mail.log"
#read from file on input if it exists
if ARGV.length == 1
	fname = ARGV[0]
elsif ARGV.length !=0
	STDERR.puts ("What are you trying to do?")
	STDERR.puts ("Look man, there're only two ways to")
	STDERR.puts ("run this program. Like this:")
	STDERR.puts ("\t\$ EmailLogMain.rb")
	STDERR.puts ("... or like this:")
	STDERR.puts ("\t\$ EmailLogMain.rb <mail logfile name>")
	STDERR.puts ("Got it?\nGood.\nNow try again.")
	exit!
end

#parse the input file, whatever it wound up being
#error checking, blah blah blah....
begin
	email = EmailLogMain.new(fname)
rescue
	STDERR.puts ("File '#{fname}' could not be read!")
	if fname == "mail.log"
		STDERR.puts ("Default file 'mail.log' could not be found.")
		STDERR.puts ("Please ensure the file exists and that you 
			have permission to read it.")
		STDERR.puts ("Alternatively, pass a valid filename as an argument.")
		exit!
	end
	begin
		print("Attempting to read default file ('mail.log')...\n")
		email = EmailLogMain.new("mail.log")
	rescue
		STDERR.puts ("Default file 'mail.log' could not be found.")
		STDERR.puts ("Please ensure the file exists and that you 
			have permission to read it.")
		STDERR.puts ("Alternatively, pass a valid filename as an argument.")
		exit!
	end
end

email.buildIDhash
email.parseLog
printemails(email.messages.values)
puts "\t\t\t\t\t==========================================="
puts "\t\t\t\t\t===\t#{email.messageCount} total messages found.\t==="
puts "\t\t\t\t\t==========================================="