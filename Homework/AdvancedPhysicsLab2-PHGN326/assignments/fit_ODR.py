# 2-D (Non)Linear Regression Fitting Program
# David Schmdit, Colorado School of Mines
# January 2013

# Developed for use by the students of Advanced Lab II at Colorado School of Mines during the Spring 2014 semester
# This program is not a final product and may contain errors

import numpy as np
import matplotlib.pyplot as plt
import scipy.odr.odrpack as odrpack

print '\nORTHOGONAL DISTANCE REGRESSION\n'

# Data Import
filename = './data2.txt' # Specify the import file name (including location if it is not in the same directory as the program
x, sx, y, sy = np.loadtxt(filename,unpack=True, usecols=[0,1,2,3]) # Specify which columns of the import file to use and save each desired column as an array

# Model Definition
def model(P, x): # Define the model to be used to fit the data. The first input is the array of parameters (to be fit), and the second is the independent variable, x.
    return P[0]*x**2. + P[1]*x + P[2] # Provide the functional form of the model, including the parameters, as shown.

# Format Model & Data for Orthogonal Distance Regression
mymodel = odrpack.Model(model) # Provide the model for the orthogonal distance regression
mydata = odrpack.RealData(x, y, sx=sx, sy=sy) # Provide the x-data, y-data, and uncertainty for each for the orthogonal distance regression


# Perform Orthogonal Distance Regression
myodr = odrpack.ODR(mydata, mymodel, beta0=[0., 0., 0.]) # Specify the formatted model and data (above), in addition to the initial guesses for the parameters.  The length of the parameter array, beta0, must equal the number of paramters in the model.


# Extract Procedural Summary and Results
myoutput = myodr.run()

# Print ODR Status and Results
print 'Return Reason:\n', myoutput.stopreason, '\n' # Reason for return from ODR fit
print 'Estimated Parameters:\n', myoutput.beta, '\n' # Values for fitted parameters
print 'Parameter Standard Errors:\n', myoutput.sd_beta, '\n' # Standard error in fitted parameter values
print 'Covariance Matrix:\n', myoutput.cov_beta, '\n' # Covariance matrix for the fit

# Chi^2 Calculation
chi2 = 0.
for i in range(len(y)):
    residual = y[i] - model(myoutput.beta, x[i]) # Calculating residual (difference between model predicted values and actual data)
    sigma = (sx[i]**2. + sy[i]**2.)**0.5 # Calculating the uncertainty associated with the data point
    chi2 += (residual / sigma)**2. # Calculating contribution to chi^2 from each point

# Reduced Chi^2 Calculation
ndof = len(y) - len(myoutput.beta) # Degrees of freedom (points - parameters in fit)
redchi2 = chi2 / ndof # Reduced chi^2 (chi^2 / degrees of freedom)

print 'Degrees of Freedom:\t', ndof
print 'Chi-Square:\t\t', chi2
print 'Reduced Chi-Square:\t', redchi2

# Plot Formatting Preparation
space = 0.2 # Desired space on either side of x and y maxima/minima as fraction of respective ranges
xspan = x.max() - x.min() # Range of x values
yspan = y.max() - y.min() # Range of y values
xspace = space * xspan # Space on either side of x min/max
yspace = space * yspan # Space on either side of y min/max
plt.xlim(x.min()-xspace,x.max()+xspace) # Specifying x-plot range by substracting/adding space to either side of x-range
plt.ylim(y.min()-yspace,y.max()+yspace) # Specifying y-plot range by substracting/adding space to either side of y-range

# Plotting
plt.errorbar(x, y, fmt = 'ro', xerr = sx, yerr = sy) # Plotting x and y data with error  bars
xarray = np.linspace(x.min()-xspace, x.max()+xspace, 500) # Creating a dense array of x-values for plotting the model
plt.plot(xarray, model(myoutput.beta, xarray)) # Plotting model predicted values using the dense array of x-values
plt.xlabel('x-label (units)', fontsize = 14) # Labeling the x-axis
plt.ylabel('y-label (units)', fontsize = 14) # Labeling the y-axis
# plt.plot(xarray, model2(parameterlist2, xarray)) # If plotting a second model, procedure remains the same

# Plot Post-Formatting & Save
fig = plt.gcf() # Returns the current figure
fig.set_size_inches(7.5,5.) # Sets figure dimensions (in inches)
plt.savefig('./myfigure.pdf', bbox_inches=0, dpi=600) # Saves figure as a pdf. bbox_inches option dictates the amount of white space around the plot's edges, dpi is dots per inch

# Plot On-Screen Display
plt.show() # Displays the current figure on the screen