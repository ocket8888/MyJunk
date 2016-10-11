#!/usr/bin/python3

####################################################################
###		Author: Brennan W. Fieck	Title: algobowl.py 			 ###
###				COPYRIGHT: None									 ###
###																 ###
###		Group Members: Brennan W. Fieck, Vanessa Something,		 ###
###	  Nicholas Zustak, That Other Guy							 ###
###																 ###
###		Description:											 ###
###			This python3 script divides n points into k 		 ###
### subgroups. At the time of this writing, there are two		 ###
### distinct algorithms used, the 'furthest' and the 'minmax'	 ###
### strategies. More information is available in the writeup.	 ###
### For testing purposes, the format of the various objects used ###
### by this script are described below.							 ###
###																 ###
###		INPUT FILES:											 ###
###  The first line of real data should be only the number of	 ###
###  points contained therein, with no trailing or leading 		 ###
###  whitespace. The second line should be only the number of 	 ###
###  groups to divide said points into, again with no trailing   ###
###  or leading whitespace. Each of the following n lines		 ###
###  should take the form of three integers, separated by a      ###
###  space, with no trailing or leading whitespace. If the       ###
###  input file contains empty lines before and/or after the     ###
###  real data, everything should function normally, but this    ###
###  has not been tested. Below is an example input file:		 ###
### 			5												 ###
### 			2 												 ###
###				1 0 0											 ###
### 			2 0 0											 ###
### 			3 0 0											 ###
### 			8 0 0											 ###
### 			9 0 0											 ###
###																 ###
###  	POINTS:													 ###
###  For the purpose of understanding the output of the `dump()` ###
###  function, points take the form (<index>, (<x>,<y>,<z>)),    ###
###  where `<index>` is the place at which the point appeared    ###
###  in the input (e.g. the index of the first point is 1). The  ###
###  second entry in this tuple is the x/y/z coordinates of the  ###
###  point. Below is an example of the points produced by the    ###
###  above input file example: 									 ###
###		[(1, (1, 0, 0)), (2, (2,0,0)), (3, (3, 0, 0)), (4, (     ###
###		8, 0, 0)), (5, (9, 0, 0))]								 ###
###																 ###
###		GROUPS OF POINTS:										 ###
###  Again, for the purpose of understanding debugging output,   ###
###  groups of points take the form [<max>, <points>], where     ###
###  `max` holds the maximum internal distance of the group 	 ###
###  (note that this is not properly calculated at all points    ###
###  during execution), and `points` is a list of the points     ###
###  that are contained within the group. Below is an example    ###
###  the output of `dump()` with regard to the state of the 	 ###
###  groups after a run of the 'furthest' algorithm on the 		 ###
###  above input file:											 ###
###    [[6, [(5, (9, 0, 0)), (4, (8, 0, 0)), (3, (3, 0, 0))]],   ###
###    [1, [(1, (1, 0, 0)), (2, (2, 0, 0))]]]					 ###
####################################################################

#######################
###		IMPORTS		###
#######################
import argparse									#Parses command-line arguments
from os.path import isfile						#Checks file system for existance of files
from copy import copy							#Performs shallow copies of iterables
from random import shuffle						#Randomizes iterables
from math import floor 							#Used to get an integer value for n/k
from sys import stdin 							#Used to read from wherever file pointer 0 currently points to
from itertools import permutations as permute	#Used to get all permutations of the points for brute Force
from itertools import combinations, chain		#Used to get every possible way to divide a list for Brute Force method

#######################
###		GLOBALS		###
#######################
points = []														#Holds the list of all points
points_group = []												#Holds the sorted groups and their respective maximum internal distances
k,n = -1,-1														#The number of groups and the number of points, respectively
strats = {"furthest": 0, "simple": 1, "minmax": 2, "brute": 3}	#Valid, yet mad strats for solving this problem

#######################
###    FUNCTIONS    ###
#######################

#Dumps current state of n, k, points, and the point groups
#to the terminal. Debugging purposes only.
def dump():
	print("n="+str(n)+"\nk="+str(k)+"\nPOINTS\n------")
	print(points)
	print("\nGROUPS\n--------")
	print(points_group)

#Finds the distance between points a and b
def dist(a, b):
	return abs(a[1][0]-b[1][0])+abs(a[1][1]-b[1][1])+abs(a[1][2]-b[1][2])

#Prints the output in the required format
def output():
	print(max([x[0] for x in points_group]))
	for group in points_group:
		print(" ".join([str(x[0]) for x in group[1]]))

#Finds the maximum internal distance for each group (used
#by the 'fursthest' algorithm, since it's agnostic of each
#group's respective maximum internal distances)
def findMaxGroupDists():
	for group in points_group:
		group[0] = max_internal_dist(group[1])

#Yields every possible way of splitting a list K ways
def split_list(lst, K):
    for splits in combinations(range(1, len(lst)), K-1):
        result = []
        prev = None
        for split in chain(splits, [None]):
            result.append(lst[prev:split])
            prev = split
        yield result

#Finds the maximum internal distance for any list of
#point objects
def max_internal_dist(pts):
	inDist = 0
	for i in range(0,len(pts)):
		pt = pts[i]
		for j in range(i+1,len(pts)):
			tmp_dist = dist(pt, pts[j])
			if tmp_dist > inDist:
				inDist = tmp_dist
	return inDist
		

#######################
###  MAIN EXECUTION ###
#######################

#parse arguments from the command-line; determine input file
parser = argparse.ArgumentParser(description='Divides n points into k subgroups, minimizing interior distance.',epilog='If no input file is specified, the program will read from stdin. If no strategy is specified, the program will attempt to determine the appropriate one based on the size of the input. NOTE: In the worst case for algorithms selected by n-value, execution can take up to 20s (on my machine)')

parser.add_argument('-f', dest='infile', type=str, default=False, help='Specify a file containing a definition for a set of points')

parser.add_argument('-s', dest='strategy', type=str, default=False, help='Specify a strategy for the algorithm. Valid values are "furthest", "minmax", and "simple".')

args = parser.parse_args()

file = stdin

if args.infile:
	if not isfile(args.infile):
		print("ERROR: No such file '"+args.infile+"'")
		exit(-1)
	file = open(args.infile)

#read in data from the input file, parse it properly as indexed
#points and construct k empty groups
try:
	raw_data = file.read().strip().split("\n")
	n, k = int(raw_data.pop(0)), int(raw_data.pop(0))
	parsedData = [[int(a) for a in b.split(" ")] for b in raw_data]
	points = []
	for i in range(0,len(parsedData)):
		points.append((i+1,tuple(parsedData[i])))
	points_group = [[0,list()] for x in range(0,k)]
except Exception as e:
	print("Bad point set: "+str(e))
	exit(-2)

#Decide on the algorithm to use, trying the one passed via
#command-line if any. n-value bounds are determined by when
#the algorithm takes longer than 20s to execute minmax appears
#to be strictly better than furthest for all n and k in terms
#of both execution time and minimum internal distance
if args.strategy:
	strategy = args.strategy
	if strategy not in strats.keys():
		print("Invalid strategy: '"+strategy+"', valid values are 'furthest', 'minmax' or 'simple'.")
		exit(-3)
elif n < 8:
	strategy = "brute"
elif n < 5000:
	strategy = "minmax"
else:
	strategy = "simple"

strat = strats[strategy]


#'furthest' algorithm, finds the two points in the given points
#with the greatest mutual distance and assigns them to different
#groups (assuming k>1, if k==1 they will be in the same group)
if strat == 0:
	groupNo = 0 #Stores the index of the group we're appending to

	#Find a home for each point
	while len(points) > 0:
		pointsCopy = copy(points)
		furthest = (-1,[]) #stores the two most mutally distant points, and their mutual distance

		#For each remaining point, compare it to every other point
		#to find the two with the greatest mutual distance
		while len(pointsCopy) > 0:
			compPoint = pointsCopy.pop()

			#If there are an odd number of points, we'll eventually
			#have only one point to place, in which case placement
			#is trivial.
			if len(pointsCopy) == 0 and furthest[0] == -1:
				furthest = (0, [compPoint])
				break

			#In general, compare this point to every other point
			for point in pointsCopy:
				distance = dist(compPoint, point)
				if distance > furthest[0]:
					furthest = (distance, [compPoint, point])

		#This can only be entered if we're on the last of an odd-numbered
		#set of points, so that means placement is trivial and we're done.
		if len(furthest[1])==1:
			points_group[groupNo][1].append(furthest[1][0])
			points.pop()
			break

		#in general, place each of the mutually most-distant points into
		#different groups, and start off with the next group for the next
		#iteration
		points_group[groupNo][1].append(furthest[1][0])
		points.remove(furthest[1][0])
		groupNo += 1
		groupNo = groupNo % k
		points_group[groupNo][1].append(furthest[1][1])
		points.remove(furthest[1][1])
		groupNo += 1
		groupNo = groupNo % k

	#This algorithm is agnostic of any properties of the groups, so the
	#maximum internal distances thereof have not been properly maintained;
	#this function rectifies that
	findMaxGroupDists()



#'minmax' algorithm, for each point finds the group where it will increase
#the maximum internal distance by the smallest amount.
elif strat == 2:

	#this algorithm has issues with highly ordered data, so we randomize it.
	shuffle(points)

	#Find a home for each point
	while len(points) > 0:
		placeMe = points.pop()
		max_dist = (999999,0) #holds (smallest maximum distance found, index of group with smallest maximum distance)
		shortcut = False

		#Compare each point to each group to find the potential change in
		#maximum internal distance
		for i in range(0,len(points_group)):

			#Always place into an empty list if possible, since this will
			#always be the smallest increase in internal distance anyway
			if len(points_group[i][1]) == 0:
				points_group[i][1].append(placeMe)
				shortcut = True
				break

			#In general, the point to be placed must be compared to each
			#point in this group to find the maximum mutual distance
			tmp_max_dist = 0
			for point in points_group[i][1]:
				tmp_dist = dist(placeMe, point)
				if tmp_dist > tmp_max_dist:
					tmp_max_dist = tmp_dist
			if tmp_max_dist < max_dist[0]:
				max_dist = (tmp_max_dist, i)

		#If we short-circuited to place into an empty list earlier, this
		#doesn't need to be done. In general, however, put the point into
		#the group found to have the smallest increase in internal distance
		#due to that placement.
		if not shortcut:
			points_group[max_dist[1]][1].append(placeMe)
			if max_dist[0] > points_group[max_dist[1]][0]:
				points_group[max_dist[1]][0] = max_dist[0]


#'simple' algorithm, just divides the input into k groups.
elif strat == 1:

	#rotate through groups and append a point to that group
	groupNo = 0
	while len(points) > 0:
		points_group[groupNo][1].append(points.pop())
		groupNo = (groupNo + 1) % k
	findMaxGroupDists()

#'brute' algorithm, uses brute force to find the optimum
#way to divide the points
elif strat == 3:
	sol = [9999999,[]] #holds the maximum of each groups maximum internal distance, as well as the list of lists of points that represent the solution

	#For each permutation of points, find every combination of groupings
	for perm in permute(points):
		lstperm = list(perm)

		#This technically generates duplicate solutions, but what're you gonna do, ya'know?
		for grouping in list(split_list(lstperm, k)):
			max_dist = max([max_internal_dist(group) for group in grouping])
			if max_dist < sol[0]:
				sol = [max_dist, grouping]

	#Because of how hard it is to iterate over every point,
	#on top of the "hardness" of computing the solution in
	#the first place via brute force, we'll take some liberties
	#in optimizing output for this method.
	print(sol[0])
	for group in sol[1]:
		print(" ".join([str(poit[0]) for poit in group]))
	exit(0)

#print the required output
output()