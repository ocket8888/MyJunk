#!/usr/bin/python3

#imported libraries
import sys
import math
import time
import itertools
import random


#constants
NNmethod = 0
EXmethod = 1

#initialize needed values
locations = []
n = 0
interactive = False

###################################
###			FUNCTIONS			###
###################################

#calculates distance between cities
def city_distance(a, b):
	return math.sqrt((a[1]-b[1])**2 + (a[0]-b[0])**2)



#prints a route through cities
def printrt(route):
	print("Route found:")
	for city in route:
		cityno = city[2]
		print("\tCity #"+str(cityno))
	print("\tCity #1\n") #always return to the starting city


#finds the total length of a route
def routelen(route):
	totalLength = 0
	for i in range(0,len(route)):
		#if we're in the last city, we have to go back
		#to the starting city
		if i == len(route) - 1:
			totalLength += city_distance(route[i], route[0]) 
			return totalLength
		totalLength += city_distance(route[i], route[i+1])



#Nearest Neighbor solution
def NN(cities, route):

	#base case; route is complete
	if len(cities) == 1:
		return


	#remove the current city from the list of cities, 
	#then initialize the next city as something "infinitely"
	#far away
	currentCity = route[len(route)-1]
	cities.remove(currentCity)
	mindist = 999999
	nextCity = []

	#find the nearest city and add it to the route
	for city in cities:
		thisdist = city_distance(city, currentCity)
		if thisdist < mindist:
			nextCity = city
			mindist = thisdist
	route.append(nextCity)

	#recursively find a route to the nearest neighbor
	NN(cities, route)

#Exhaustive Method solution
def EX(cities):
	#since we know we're starting from the first city,
	#don't include it in the permutations
	startingCity = cities.pop(0)

	#setup
	allroutes = itertools.permutations(cities)
	minlength = 9999999
	shortestRoute = []

	#find the shortest route
	for route in allroutes:
		#restore starting point (without editing the
		#thing we're iterating over, which could be bad)
		path = list(route)
		path.insert(0, startingCity)

		#compare the length of this route to the current
		#minimum
		length = routelen(route)
		if length < minlength:
			shortestRoute = path
			minlength = length
	return shortestRoute

#runs each algorithm and times it
def time_and_run(algorithm, city_list):
	#setup
	route = [city_list[0]]
	cities = list(city_list) #let's not modify the cities list


	#run NN and time it
	if algorithm == 0:
		before = time.time()
		NN(cities, route)
		elapsed = time.time() - before
		printrt(route)
		return elapsed

	#run EX and time it
	elif algorithm == 1:
		before = time.time()
		shortestPath = EX(cities)
		elapsed = time.time() - before
		printrt(shortestPath)
		return elapsed


#takes the average of three runs of an algorithm for one input
def avg_run_time(algorithm, city_list):
	#ensure n is set properly
	n = len(city_list)

	#Prompts
	if algorithm == 0:
		algorithmName = "Nearest Neighbor"
	else:
		algorithmName = "Exhaustive"

	print("Finding average runtime for '" + algorithmName + "' algorithm ..")

	#collect data
	runtime = time_and_run(algorithm, city_list)
	runtime += time_and_run(algorithm, city_list)
	runtime += time_and_run(algorithm, city_list)
	runtime /= 3

	#print data
	print("Average run time for n="+str(n)+" using "+algorithmName+\
" algorithm: "+str(runtime)+" seconds")



#######################################
###			SCRIPT EXECUTION		###
#######################################


### Parse command line options ###

	#check for interactive option
if sys.argv.count('-i'):
	interactive = True
	
	#print help text
if sys.argv.count('-h') > 0:
	print("TSP-Code.py - A testing suite for \
algorithms that attempt to solve the \
Traveling Salseperson Problem.")
	print("Usage:\tTSP-Code.py")
	print("\tTSP-Code.py [-i] -f <filename>")
	print("\tTSP-Code.py -h")
	print("\tTSP-Code.py -i")
	print("\tTSP-Code.py [-i] -s [ < filename ]")
	print("\tTSP-Code.py -t")
	print("==== OPTIONS ====")
	print("\tf: Reads the next string as a filename and attempts to \
run each algorithm on its contents")
	print("\th: Print this help message and exit.")
	print("\ti: Interactive mode; waits for keyboard input after each\
test")
	print("\ts: Reads input on stdin for testing data")
	print("\tt: Runs test suite and exits (minimal, only meant for dev use)")
	print("If no input is specified, simply runs random tests.")
	sys.exit(0)

#test some functions
elif sys.argv.count("-t"):
	exit_code = 0
	test_cities = [(0,0,1), (0,1,2), (0,2,3)]
	test_len = city_distance(test_cities[0], test_cities[1])
	if test_len != 1:
		exit_code += 1
		print("city_distance test:\tfailed!\n\tExpected output: 1\
\n\tActual output: "+str(test_len))
	else:
		print("city_distance test:\tpassed!")
	test_len = routelen(test_cities)
	if test_len != 2:
		exit_code +=1
		print("routelen test:\t\tfailed!\n\tExpected output: 3\
\n\tActual output: "+str(test_len))
	else:
		print("routelen test:\t\tpassed!")
	sys.exit(exit_code)

	#read input from stdin
elif sys.argv.count("-s"):
	cities = sys.stdin.read()
	firstline = True
	index = 0
	for city in cities.split('\n'):
		if firstline:
			firstline = False
			n = int(city) #I don't think I'll ever use this
		else:
			coords = city.split()
			locations.append((int(coords[0]), int(coords[1]), index))
		index += 1
	print("Running algorithms on user input...")
	avg_run_time(NNmethod, locations)
	if interactive:
		input("Press <Enter> to continue...")
	avg_run_time(EXmethod, locations)
	if interactive:
		input("Press <Enter> to continue...")

	#read infut from file
elif sys.argv.count("-f"):
	if len(sys.argv) - 1 <= sys.argv.index("-f"):
		print("'-f' option requires argument!\nSkipping file input...")
	else:
		try:
			infile = open(sys.argv[sys.argv.index('-f') + 1])
			cities = infile.read()
			firstline = True
			index = 0
			for city in cities.split('\n'):
				if firstline:
					firstline = False
					n = int(city) #I don't think I'll ever use this
				else:
					coords = city.split()
					locations.append((int(coords[0]), int(coords[1]), index))
				index += 1
			print("Running algorithms on user input...")
			time_elapsed = time_and_run(NNmethod, locations)
			if interactive:
				input("Press <Enter> to continue...")
			time_elapsed = time_and_run(EXmethod, locations)
			if interactive:
				input("Press <Enter> to continue...")
		except FileNotFoundError:
			print("File '"+sys.argv[sys.argv.index('-f') + 1]+"' not found.\
\nSkipping file input...")



### Run random-data tests ###

#I use n values 5, 6, 7, 8
for n in range(5,9):
	#ensure locations is clear
	locations = []
	for index in range(1,n+1):
		x = random.randint(1,100)
		y = random.randint(1,100)
		location = (x, y, index)
		locations.append(location)
	avg_run_time(NNmethod, locations)
	if interactive:
		input("Press <Enter> to continue...")
	avg_run_time(EXmethod, locations)
	if interactive:
		input("Press <Enter> to continue...")