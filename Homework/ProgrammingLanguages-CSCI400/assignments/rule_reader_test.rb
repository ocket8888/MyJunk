=begin
	First step in creating the DSL - just respond to the methods. Learnding!
=end

def product(text)
  puts "Just read a product: #{text}"
end

def packing_slip(text)
  puts "Just read packing slip: #{text}"
end

def activate
	puts "Activate"
end 

load 'rulesTest.txt'

