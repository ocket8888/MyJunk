import matplotlib.pyplot as plt

directory_in = './'
filename = 'background_5mins' # Specify the import file name of SPE file
extention_in = '.Spe'

directory_out = './'
extention_out = '.pdf'


plt.title('Spectrum of 137Cs with 5 minuets live time')
plt.xlabel('ADC bin', fontsize = 14) # Labeling the x-axis
plt.ylabel('Hits', fontsize = 14) # Labeling the y-axis

#plt.xlim(0,1000) # Specifying x-plot range 

with open(directory_in+filename+extention_in) as inspec:
    _hit = []
    for line in inspec:
        if not "0 2047\n" in line:
            continue
        for line in inspec:
            if "$ROI:\n" in line:
                break
            _hit.append(int(line))
    _bin = list(range(0,len(_hit)))
#data=zip(_bin,_hit)
#print(list(data))

plt.fill(_bin,_hit, "o")


plt.savefig(directory_out + filename + extention_out, bbox_inches=0, dpi=600)

plt.show() # Displays the current figure on the screen