#!/usr/bin/env python3

import seaborn as sns
from matplotlib import pyplot as plt
from numpy import array as na
import argparse

parser=argparse.ArgumentParser(description="Simulates photon counting from SPIFI data.")
parser.add_argument('-t', type=float, default=0.0, dest='threshold', help="Specifies a threshold level above which to look for peaks. Default: 0.")
parser.add_argument('-a', action='store_true', help="Sets the program to auto-generate 'sinc' function-like input; if not given looks for input on stdin.")
args=parser.parse_args()

if args.a:
	from math import sin
	#for my sanity, returns 0 whenever the input is 0
	def sinc(x):
		if hasattr(x, '__iter__'):
			return [(sin(y)/float(y)) if not y==0 else 0 for y in x]
		return sin(x)/float(x) if not x==0 else 0
	yvals=[y**2 for y in sinc([x/2.0 for x in range(-200, 201)])]
else:
	from sys import stdin
	yvals = [float(x) for x in stdin.read().strip().split()]


sns.set(style='darkgrid')

peaks = list()
for i in range(1, len(yvals)-1):
	val = yvals[i]
	if val>args.threshold and yvals[i-1]<val and yvals[i+1]<val:
		peaks.append(i)

binSize = 0
for peak in range(0, len(peaks)-1):
	diff = peaks[peak+1] - peaks[peak]
	if diff > binSize:
		binSize = diff

bins = list()
for dataSet in range(0, len(yvals)-(len(yvals) % binSize), binSize):
	bins.append(sum(yvals[dataSet:dataSet+binSize-1]))

bins.append(sum(yvals[len(yvals)-(len(yvals) % binSize):]))

onePhoton = min(filter(lambda x: x>0, bins))

bins = [int(round(x/onePhoton)) for x in bins]

plt.plot([x for x in range(int(-(len(yvals)-1)/2),int((len(yvals)-1)/2)+1)], yvals, '-o')
plt.show()
sns.barplot(x=na(range(0, len(bins))), y=na(bins))
plt.show()
exit(0)