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
	return sum(datalist[binA:binB+1])/(numpy.pi*((1.75*2.54/2)**2))/1200.0

def crossSection(bin1, bin2):
	return Ndet(bin1, bin2)/(Nbeam*Ntargets*dOmega(AngleFromBin(bin1), AngleFromBin(bin2)))


for groupSize in [3, 5, 10]:
	print("---------- Group Size " + repr(groupSize) + " ----------")
	for channel in range(0, 478, groupSize):
		if channel+groupSize-1 > 477:
			break
		print((channel, channel+groupSize-1, crossSection(channel, channel+groupSize-1)))

