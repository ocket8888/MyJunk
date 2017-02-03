# 2-D (Non)Linear Regression Fitting Program
# Dan Shields, Colorado School of Mines
# dshields@mymail.mines.edu
# 2017/01/01

# Developed for use by the students of Ad Lab II at Colorado School of Mines
# This program is not a final product and may contain errors
# Written for Python 3.X

import numpy as np
import matplotlib.pyplot as plt
import scipy.odr.odrpack as odrpack
# More info on ODR usage: https://docs.scipy.org/doc/scipy/reference/odr.html

##############################################################################
################################# USER INPUT #################################
##############################################################################

# Data Import
# Specify the import file name and location
filename = './e_vs_adc.txt' 
# Specify which columns of the import file to use 
# and save each desired column as an array
# File format used here is CSV tab delinated: x \t delta_x \t y \t delta_y 
x, sx, y, sy = np.loadtxt(filename,unpack=True, usecols=[0,1,2,3]) 

# Model Definition
# Define the model to be used to fit the data. 
# The first input is the array of parameters (to be fit)
# The second is the independent variable, x.

def model1(P, x): 
    return P[0]*x + P[1]
beta_guess1=[0., 0.]

# Data point to fit with model [point, error]
mystery_ADC = [1.5, 1]

# Plot Formatting
title = 'Energy vs. ADC bin for 137Cs, 60Co, 40K'
xlabel = 'ADC (bins)'
ylabel = 'Energy (keV)'

# Space on either side of x and y max/min as fraction of respective ranges
space = 0.5 

#filename and destination to save fig
savefig = 'energy_fit.pdf'

##############################################################################
################################## PROGRAM ###################################
##############################################################################

print('\nORTHOGONAL DISTANCE REGRESSION\n')

# Format Model & Data for Orthogonal Distance Regression
# Provide the x-data, y-data, and uncertainty for each for the ODR
mydata = odrpack.RealData(x, y, sx=sx, sy=sy) 
# Provide the model for the orthogonal distance regression
mymodel1 = odrpack.Model(model1)



# Perform Orthogonal Distance Regression
# NOTE: beta0 (the parameter guess) must  = # parameters in the model
myodr1 = odrpack.ODR(mydata, mymodel1, beta0=beta_guess1)

# Extract Procedural Summary and Results

# M is an ODR model that is run and prints results.
# Returns the ORD output data
def run_model(M): 
    myoutput = M.run()

    print('Return Reason:\n', myoutput.stopreason, '\n')
    print('Estimated Parameters:\n', myoutput.beta, '\n')
    print('Parameter Standard Errors:\n', myoutput.sd_beta, '\n')
    print('Covariance Matrix:\n', myoutput.cov_beta, '\n')

    # Chi^2 Calculation
    chi2 = 0.
    for i in range(len(y)):
        residual = y[i] - model1(myoutput.beta, x[i])
        sigma = (sx[i]**2. + sy[i]**2.)**0.5
        chi2 += (residual / sigma)**2. 

    # Reduced Chi^2 Calculation
    n_deg_of_freedom = len(y) - len(myoutput.beta) 
    red_chi2 = chi2 / n_deg_of_freedom

    print('Degrees of Freedom:\t', n_deg_of_freedom)
    print('Chi-Square:\t\t', chi2)
    print('Reduced Chi-Square:\t', red_chi2)

    return myoutput


print('MODEL1:\n\n')
myoutput1 = run_model(myodr1)

mystery_E = [0,0]
mystery_E[0] = model1(myoutput1.beta, mystery_ADC[0])
#NOTE: this is a filler for a better way to get the error on y!!!
mystery_E[1] = 0.0001 

print('\nModel mystery peak Energy = ', mystery_E,  '\n')

# Plot Formatting Preparation
xspan = x.max() - x.min()
yspan = y.max() - y.min()
xspace = space * xspan
yspace = space * yspan

plt.xlim(x.min()-xspace,x.max()+xspace)
plt.ylim(y.min()-yspace,y.max()+yspace)

# Plotting
# Creating a dense array of x-values for plotting the models
xarray = np.linspace(x.min()-xspace, x.max()+xspace, 500)

# Model Plots
plt.plot(xarray, model1(myoutput1.beta, xarray)) 

#Data plots
plt.errorbar(x, y, fmt = 'ro', xerr = sx, yerr = sy)

plt.errorbar(mystery_ADC[0], mystery_E[0] , fmt = 'go', xerr = mystery_ADC[1]
, yerr = mystery_E[1])

plt.title(title)
plt.xlabel(xlabel, fontsize = 14) # Labeling the x-axis
plt.ylabel(ylabel, fontsize = 14) # Labeling the y-axis


# Plot Post-Formatting & Save
fig = plt.gcf()
fig.set_size_inches(7.5,5.)
# Saves figure as a pdf.
# bbox_inches option = plot's edges, dpi is dots per inch
plt.savefig(savefig, bbox_inches=0, dpi=600)

# Plot On-Screen Display
plt.show() # Displays the current figure on the screen

