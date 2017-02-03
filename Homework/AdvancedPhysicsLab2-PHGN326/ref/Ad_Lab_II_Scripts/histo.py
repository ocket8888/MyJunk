# Simply Histograming Program
# Dan Shields, Colorado School of Mines
# dshields@mymail.mines.edu
# 2017/01/01

# Developed for use by the students of Ad Lab II at Colorado School of Mines
# This program is not a final product and may contain errors
# Written for Python 3.X

import numpy as np
import matplotlib.pyplot as plt

##############################################################################
################################# USER INPUT #################################
##############################################################################

x = [0 for x in range(0,7)]+[1 for x in range(0,24)]+[2 for x in range(0,22)]+[3 for x in range(0,32)]+[4 for x in range(0,10)]+[5,5,5,5]+[6,6,6]

plt.title('Histogram of Counts per second')
plt.xlabel('Counts')
plt.ylabel('Probability')


savefig = 'histo.pdf'

##############################################################################
################################## PROGRAM ###################################
##############################################################################

n, bins, patches = plt.hist(x, 7, normed=1, facecolor='green', alpha=0.75)


plt.axis([0,6, 0, 0.5])
plt.grid(True)

# Plot Post-Formatting & Save
fig = plt.gcf()
fig.set_size_inches(7.5,5.)
plt.savefig(savefig, bbox_inches=0, dpi=600)

plt.show()