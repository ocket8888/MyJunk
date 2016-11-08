#!/usr/bin/env python3

import sys
from copy import deepcopy
import argparse

#Simple function that determines if going to
#fromEvent leaves enough time to reach toEvent
def canGetTo(fromEvent, toEvent):
	timeDiff = toEvent[0] - fromEvent[0]
	distDiff = abs(toEvent[1] - fromEvent[1])
	return (timeDiff > 0 and distDiff <= timeDiff)

#Read from a file if given, or just from stdin otherwise
parser=argparse.ArgumentParser(description="solves telescope problems")
parser.add_argument('infile', nargs='?',\
	help='specifies an input file (reads from stdin if not used)')
args=parser.parse_args()
infile=sys.stdin
if len(sys.argv) > 1:
	infile=open(args.infile)

#Parse events from the input
events = infile.read().split(" ")
events = [(i+1, int(events[i])) for i in range(0,len(events))]

#Build an initial set of reachable events
initialEvents = []
for event in events:
	if canGetTo((0,0), event):
		initialEvents.append(event[0])
initialEvents = set(initialEvents)

#The last event; anywhere we go has to be
#able to reach here
endEvent = events.pop()

#If there's not enough time to get to the last
#event from the starting position, nothing can
#be done
if endEvent[0] not in initialEvents:
	print("Invalid input", file=sys.stderr)
	sys.exit(1)
initialEvents=initialEvents-set([endEvent[0]])

#Holds the maximum-length paths and their lengths
#indexed by the time of the event that starts
#that path
calculatedMaxPaths = dict()

#Recursively finds the longest path from `start`
#to the endEvent
def GetPath(start, availableEvents):

	#Figure out which events can be reached from
	#here (and also don't take us away from being
	#able to reach the endEvent)
	reachableEvents = []
	for eventNo in availableEvents:
		if canGetTo(start,events[eventNo-1]) and\
		   canGetTo(events[eventNo-1],endEvent):
			reachableEvents.append(eventNo)
	reachableEvents=set(reachableEvents)

	#For each reachable event, find the path starting
	#from that event and ending at the endEvent
	#that has the maximum length
	maxPath=[endEvent]
	maxlen=1
	for eventNo in reachableEvents:
		if eventNo not in calculatedMaxPaths:
			newPath=GetPath(events[eventNo-1],\
				reachableEvents-set([eventNo]))
			calculatedMaxPaths[eventNo]=(len(newPath),newPath)
		if calculatedMaxPaths[eventNo][0]>maxlen:
			maxPath=calculatedMaxPaths[eventNo][1]
			maxlen=calculatedMaxPaths[eventNo][0]

	#return a path that starts at `start`, travels
	#a maximum distance, and finally ends at the
	#endEvent
	maxPath.insert(0,start)
	return maxPath

#`(0,0)` is a fake event that marks the telescope's
#starting position, use it to generate the maximum
#length path, then remove it before printing the
#solution
maxPath=GetPath((0,0), initialEvents)
maxPath.pop(0)
print(" ".join([str(x[0]) for x in maxPath]))