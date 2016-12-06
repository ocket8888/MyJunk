#!/usr/bin/python3

#imports and checks for proper imports
from sys import argv, exit, stderr
from math import log10
import pygame as pg
from pygame.locals import *
if not pg.font:
	print("Warning: fonts disabled.", file=stderr)

#ArgumentParser is overkill for this
if len(argv) < 2:
	print("Dude. Input file?",file=stderr)
	exit(1)



###################
##   Constants   ##
###################
screenSize = (760, 760)
unvisitedKind = 0
visitedKind = 1
startEndKind = 2
winnerKind = 3
circleColor = { unvisitedKind: (243, 229, 171), \
                visitedKind: (255, 26, 0), \
                startEndKind: (211, 19, 249), \
                winnerKind: (51, 255, 0)}
#During setup, these keep track of what has already
#been drawn, and `drawnCircles also stores the xy
#coordinates of the bugs so they can be updated later
drawnCircles = dict()
drawnEdges = set()
fontColor = (0, 0, 0)
#using numbers makes these much easier to deal with
directions = { 'N':0, 'NE':1, 'E':2, 'SE':3, 'S':4, \
               'SW':5, 'W':6, 'NW':7}


########################
###  Main Execution  ###
########################

#read in the data and extract the value of n.
raw_data = open(argv[1]).read().strip().split("\n")
n = int(raw_data.pop(0))

#This initializes a hash map of bug-id keys that correspond
#to arrays of vertexes that it shares direct connecting edges with.
#The order of this list of values indicates the direction of the edge;
#see the 'directions' hash map constant for exactly what value
#corresponds to what direction.
#Since bug id's start at 0 and from there increment to n+1, I let a
#value of -1 represent a non-existent edge.
bugs = dict([(x,[-1, -1, -1, -1, -1, -1, -1, -1]) for x in range(0,n+2)])

#This bit parses out the edges from the input file and assigns them
#to the appropriate vertex pairs.
for line in raw_data:
	edge = line.strip().split(" ")
	edge = (int(edge[0]), int(edge[1]), directions[edge[2]])
	bugs[edge[0]][edge[2]] = edge[1]
	bugs[edge[1]][(edge[2]+4) % 8] = edge[0]

#This holds the set of visited vertices, necessary for the
#display as well as the actual algorithm
visited = set()

#initialize window and draw the graph
pg.init()
font = pg.font.SysFont("Impact", 15)
screen=pg.display.set_mode(screenSize)
screen.fill((255,255,255))

#Draws the graph (I assume that the starting point is nearly the
#northeastern-most vertex in the graph)
initGraph(50, 100)
pg.display.update()

#This reverses the output of the algorithm to obtain a
#form that makes more intuitive sense
solution = findPath(0)[::-1]

#Highlights the solution in green on the display
highlight(solution[1:len(solution)-2:])

#Prints the solution in the required format
print(" ".join([str(x) for x in solution]))

#This infinite loop just ensures that the display stays open
#until you close the window.
while True:
	for event in pg.event.get():
		#don't even ask
		if event.type==pg.QUIT:
			pg.display.quit()
			pg.quit()
			exit(0)
			sys.exit(0)