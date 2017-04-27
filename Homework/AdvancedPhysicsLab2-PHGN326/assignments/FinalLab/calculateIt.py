#!/usr/bin/env python3

from sys import argv
import numpy

data = open(argv[1]).read().strip().split("\n")

datalist = list()
datasec, collecting = False, False
for line in data:
	if line.strip() == "$DATA:":
		datasec = True
	elif datasec:
		datasec = False
		collecting = True
	elif collecting:
		line = line.strip()
		if line == "$ROI:":
			break
		datalist.append(int(line))

numpy.seterr(all="print")

#Some constants
Nbeam = 118.331
Ntargets = 2.39602*(10**24)
a = 1.217224

def dOmega(theta1, theta2):
	return 2.0*numpy.pi*numpy.sin((theta1+theta2)/2.0)*(theta2-theta1)

def AngleFromBin(channel):
	return numpy.arccos(1.0-(((1.0/(1.0-(channel/662.0)))-1.0)/a))

def Ndet(binA, binB):
	return sum(datalist[binA:binB+1])/1200.0
	#return sum(datalist[binA:binB+1])/(numpy.pi*((1.75*2.54/2)**2))/1200.0

def crossSection(bin1, bin2):
	return Ndet(bin1, bin2)/(Nbeam*Ntargets*dOmega(AngleFromBin(bin1), AngleFromBin(bin2)))

def Theoretical(angle):
	part1 = ((numpy.cos(angle)**2.0)+1)*0.5*((2.8/(10.0**13))**2)
	part2 = ((4*a*(numpy.sin(angle/2.0)**4)+1)/(((numpy.cos(angle)**2)+1)*((2*a*(numpy.sin(angle/2.0)**2))+1)))+1
	part3 = 1.0/((2*a*(numpy.sin(angle/2.0)**2))+1)

	return part1*part2*part3


for groupSize in [3, 5, 10]:
	print("---------- Group Size " + repr(groupSize) + " ----------")
	for channel in range(0, 477, groupSize):
		upperBound = channel+groupSize-1
		if upperBound > 476:
			break
		avgAngle = (AngleFromBin(channel)+AngleFromBin(upperBound))/2
		print(repr(avgAngle) + "," + repr(Theoretical(avgAngle)) + repr(crossSection(channel, upperBound)))


