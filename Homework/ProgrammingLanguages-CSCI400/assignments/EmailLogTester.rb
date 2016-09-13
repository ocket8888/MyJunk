#!/usr/bin/ruby
require 'minitest/autorun'
require './EmailLogMain.rb'

#This class tests various line parsing methods
#as well as the count of messages of the
#EmailLogMain.rb
class EmailLogTester < MiniTest::Test
	#set up the email object we're testing
	def setup
		@email = EmailLogMain.new("mail.log")
		@email.buildIDhash
		@email.parseLog
	end

	#make sure we found all the messages
	def test_messageCount
		assert(@email.messageCount == 61)
	end

	#ensure we can properly obtain a "to" address
	def test_getTo
		teststr = "Dec 11 06:25:05 toilers postfix/smtp[4419]: B6C161B2004: to=<frodobaggins@mines.edu>, orig_to=<root>, relay=smtp.Mines.EDU[138.67.1.48]:25, delay=0.15, delays=0.05/0.01/0.05/0.04, dsn=2.0.0, status=sent (250 2.0.0 rBBDP5ji010691 Message accepted for delivery)"
		assert(@email.getTo(teststr) == "frodobaggins@mines.edu")
		teststr = "This doesn't have a 'to', but it does have an orig_to=<me@site.ext>"
		assert_nil(@email.getTo(teststr))
	end

	#ensure we can properly get a size from a line
	def test_getSize
		teststr = "Dec 11 06:25:05 toilers postfix/qmgr[1387]: 999FB1B2003: from=<root@toilers.Mines.EDU>, size=5936, nrcpt=1 (queue active)"
		assert(@email.getSize(teststr) == 5936)
		teststr = "This email doesn't have a proper 'size'. And that's it!"
		assert_nil(@email.getSize(teststr))
	end

	#ensure we can properly parse out a message id
	def test_getmId
		teststr = "Dec 11 05:18:07 toilers postfix/cleanup[3933]: BFD2F1B2003: message-id=<20131211121807.BFD2F1B2003@toilers.Mines.EDU>"
		assert(@email.getmId(teststr) == "20131211121807.BFD2F1B2003@toilers.Mines.EDU")
		teststr = "Dec 11 05:14:03 toilers postfix/cleanup[3933]: BFD2F1B2003: removed"
		assert_nil(@email.getmId(teststr))
	end

	#ensure we can properly obtain a "from" address
	def test_getFrom
		teststr = "Dec 11 06:25:05 toilers postfix/qmgr[1387]: 999FB1B2003: from=<root@toilers.Mines.EDU>, size=5936, nrcpt=1 (queue active)"
		assert(@email.getFrom(teststr) == "root@toilers.Mines.EDU")
		assert_nil(@email.getFrom("foo bar"))
	end

	#ensure we properly set a DateTime object based on input
	def test_getDate
		teststr = "Dec 11 05:18:07 toilers postfix/cleanup[3933]: BFD2F1B2003: message-id=<20131211121807.BFD2F1B2003@toilers.Mines.EDU>"
		assert(@email.getDate(teststr) == DateTime.new(2015, 12, 11 ,5, 18, 7))
		teststr = "Dec toilers postfix/cleanup[3933]: BFD2F1B2003: message-id=<20131211121807.BFD2F1B2003@toilers.Mines.EDU>"
		assert_nil(@email.getDate(teststr))
	end
end