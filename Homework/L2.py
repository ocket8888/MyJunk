#!/usr/bin/env python3

#This script prints the number of times a `5` appears in the digits of an integer passed as an argument
#The script will expect an argument, which also must be an integer; omission of an argument or passing a float/string WILL cause it to crash
#The integer ("n") may be either positive or negative

from sys import argv
from math import log, floor

#Declaring this as a function allows it to be used as a generator,
#which in turn allows for lazy computation of the `sum`. Otherwise,
#you'd need to iterate over `n`s digits to first create some kind
#of collection and then collapse it afterwards with `sum` (and
#that's bad because iterating lists takes linear time, but tuples
#are immutable so that adds an extra operation to each iteration).
#Or I could've just set a variable to 0 and incremented it every
#time I see a '5'. But this was more fun. 
def digits(number):
	for power in range(floor(log(number, 10))):
		yield number % (10**(power + 1)) // (10**power)

print(sum(1 for digit in digits(abs(int(argv[1]))) if digit == 5))
