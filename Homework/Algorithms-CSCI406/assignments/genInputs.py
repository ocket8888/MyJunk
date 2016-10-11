#!/usr/bin/python3
from sys import argv
from random import randint as rand
from math import floor
n = 10
k = rand(1, floor(n/2)+1)
if len(argv) == 2:
	n = int(argv[1])
	k = rand(1, floor(n/2)+1)
elif len(argv) == 3:
	n=int(argv[1])
	k=int(argv[2])
points = []
for i in range(0,n):
	points.append((rand(-50, 50), rand(-50, 50), rand(-50, 50)))
print(str(n)+"\n"+str(k)+"\n"+"\n".join([ " ".join([str(y) for y in x]) for x in points]))
