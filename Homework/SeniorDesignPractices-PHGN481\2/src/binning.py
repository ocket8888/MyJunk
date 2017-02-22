#!/usr/bin/env python3

import seaborn as sns
import matplotlib.pyplot as plt
from numpy import array as na
from sys import exit as exit
from math import sin

#for my sanity, returns 0 whenever the input is 0
def sinc(x):
	if hasattr(x, '__iter__'):
		return [(sin(y)/float(y)) if not y==0 else 0 for y in x]
	return sin(x)/float(x) if not x==0 else 0
		

threshold = 0

sns.set(style='darkgrid')

xvals=[x/2.0 for x in range(-200, 200)]

yvals=list(map(lambda v: v if v>threshold else 0, [y**2 for y in sinc(xvals)]))

peaks = list()
for i in range(1, len(yvals)-1):
	val = yvals[i]
	if yvals[i-1]<val and yvals[i+1]<val:
		peaks.append(i)

binSize = 0
for peak in range(0, len(peaks)-1):
	diff = peaks[peak+1] - peaks[peak]
	if diff > binSize:
		binSize = diff
print(binSize)

bins = list()
for dataSet in range(0, len(yvals)-(len(yvals) % binSize), binSize):
	bins.append(sum(yvals[dataSet:dataSet+binSize-1]))

bins.append(sum(yvals[len(yvals)-(len(yvals) % binSize):]))
print(len(bins))
onePhoton = min(filter(lambda x: x>0, bins))
print(len(bins))
bins = [int(round(x/onePhoton)) for x in bins]

sns.countplot(bins)
plt.show()
exit(0)