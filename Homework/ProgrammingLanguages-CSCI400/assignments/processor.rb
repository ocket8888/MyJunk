#!/usr/bin/ruby
require 'singleton'
require './rule_set.rb'

class Processor
  include Singleton
  @rules
  @currentchoice
  
  def initialize
  	@currentchoice = -1
	@rules = [];
	end
  
  def menu
	puts "<Main Menu>"
	puts "1. Load Rules"
	puts "2. Process orders"
	puts "3. End"
	puts "Your option: "
	@currentchoice = STDIN.gets.chomp.to_i
	end
	
  def process_order(text)
  	puts "processing order for a: #{text}"
	if ! RuleSet.instance.rules.has_key? text
		RuleSet.instance.rules.store(text, Product.new(text))
		end
		RuleSet.instance.rules[text].each{|process| RuleSet.instance.instance_eval process}
	end


  def activate_membership
	puts "---- Activating membership\n"
	end
  
  def make_slip(text)
	puts "---- Creating #{text} packing slip"
	end
  
  def pay_bill(text)
	puts "---- Paying #{text}"
	end
	
  def execute_order(option)
    puts "Order was #{option}"
    if option == 1
		puts "Enter the filename (with extension) of the rules file: "
		path = STDIN.gets.chomp
		RuleSet.instance.load_ruleset(path)
		menu
		end
	if option == 2
		choice = '0'
		until choice == 'D' || choice == 'd'
			puts "Enter the product type or 'D'(done) to end: "
			choice = STDIN.gets.chomp
			process_order(choice)
			end
		menu
		end
	#else Add error handling
	end

  def run_system
	until @currentchoice == 3
		menu
		execute_order(@currentchoice)
	end
	end
  
 end
 
 

 
 class Product
	@name
	
	def initialize(name)
		@name = name
	end
	
	def name
		@name
	end
	
end
 
 
 

  