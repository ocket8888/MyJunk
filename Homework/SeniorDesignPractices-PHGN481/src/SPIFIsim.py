#!/usr/bin/python3
import argparse
import os.path
import seaborn as sns
import pandas as pd
import array as arr
import matplotlib.pyplot as plt
import numpy as np

def w_t(t):
	return np.cos(2*np.pi*t/10)

def modulation(w, x, t, freq):
	return w(t)*(1.0+np.cos((1.0/freq)*x*t))

def initial(x, freq):
	return np.cos(x/freq)

def incident_field(A, mod, initial, obj, x, t, w, mod_freq, wave_freq):
	return A*mod(w,x,t,mod_freq)*initial(x, wave_freq)*obj

parser = argparse.ArgumentParser(description='Simulates SPIFI microscopy scanning',epilog='The -o option expects that input files begin with a single line of the form "n m" to specify n rows and m columns for the object. Each of the next n lines must consist of m space-separated positive floating-point numbers (no greater than 100) corresponding to the "thickness" of the object at that point, with no empty lines or terminating newline character. Objects MUST be rectangular. If no input file is specified, the program will run with an empty object.')

parser.add_argument('-o', dest='objfile', type=str, default=False, help='Specify a file containing a definition for an object.')

args = parser.parse_args()

m,n = 10, 10
objectArr = [[1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,],
			 [1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,]]
if args.objfile:
	#read in object
	if not os.path.isfile(args.objfile):
		print("ERROR: Not a file: "+args.objfile)
		exit(-1)

	#begin parse
	try:
		raw_obj = open(args.objfile).read().split("\n")
		n, m = [int(entry) for entry in raw_obj.pop(0).split(" ")]
		parsed_obj = [[(100.0-float(y))/100.0 for y in x.split(" ")] for x in raw_obj]
		objectArr = parsed_obj
	except Exception:
		print(Exception.message)
		exit(-1)

print(objectArr)

#ensure sanity of object
if len(objectArr) != n:
	print("Corrupt object, possibly bad input file. Terminating...")
	exit(-2)
for row in objectArr:
	if len(row) != m:
		print("Corrupt object, possibly bad input file. Terminating...")
		exit(-2)
	for thickness in row:
		if thickness > 1 or thickness < 0:
			print("Invalid thickness value: "+str(thickness)+". Values may only be in the range [0-99].")
			exit(thickness)


sns.set(style="ticks")

#set up some constants (allow c=1)
Amp = 10.0
omega = 1.0
modulation_frequency = 1.0
detector_distance = 1.0
object_distance =0.75

times=range(0,100)
yvals=range(0,n)
zvals=range(0,m)

Intensities=dict()

for time in times:
	Intensities[time] = list()
	for y in yvals:
		Intensities[time].append(list())
		for z in zvals:
			Intensities[time][y].append(abs(incident_field(Amp, modulation, initial, objectArr[y][z], detector_distance, float(time), w_t, modulation_frequency, omega))**2)
		Intensities[time][y]=np.array(Intensities[time][y])
	Intensities[time] = np.array(Intensities[time])
print(Intensities[10])


vmin=0
vmax=Amp

#data_set = {"Intensity":Intensities[0]}
sns.set(style="whitegrid")
panda_intensity=pd.DataFrame({"t": np.array(times), "Intensity": np.array([Intensities[t][0][0] for t in times])})
print("Panda intensity:\n")
print(panda_intensity)
sns.residplot(x=np.array(times), y=np.array([Intensities[t][5][5] for t in times]))
plt.show()
panda_data=pd.DataFrame(Intensities[15])
#panda_data[panda_data.columns]=panda_data[panda_data.columns].astype(float)
print("Panda data:\n")
print(panda_data)
print(len(Intensities[0][0]))
sns.heatmap(panda_data, vmin=vmin, vmax=vmax)
plt.show()