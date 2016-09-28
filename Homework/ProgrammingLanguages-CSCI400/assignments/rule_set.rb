# this singleton class will hold all of the product rules

require 'singleton'
require './processor.rb'

class RuleSet
  include Singleton
  attr_accessor :lastproduct
  attr_accessor :rules
  
  def initialize
	@rules = Hash.new {|h,k| h[k] = Array.new }
	@lastproduct = Product.new('ProductNotFound')
	end
  
  def goto_product(text)
	@rules.each do |key, arr|
		if key.name == text
			@lastproduct = key
		else
		@lastproduct = Product.new(text)
			end
		end
	end
	
  def add_method(text)
	@rules[lastproduct] << text
	end
	
  
  def load_ruleset(text)
	load text
	end
	
end

 
def product(text)
  RuleSet.instance.goto_product text
end

def activate
  RuleSet.instance.add_method 'activate_membership'
end

def packing_slip(text)
  RuleSet.instance.add_method 'make_slip #{text}'
  end
  
 def pay(text)
  RuleSet.instance.add_method 'pay_bill #{text}'
  end