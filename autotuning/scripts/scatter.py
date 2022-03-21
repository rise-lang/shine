# scatter_plotting.py

import csv
import os
import sys

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.pyplot import cm
import numpy as np

plt.style.use('seaborn')  # to get seaborn scatter plot

path = sys.argv[1]
log = False

def parse_log(arg):
    if(arg == 'log'):
        return True
    else:
        return False


if(len(sys.argv) > 2):
    log = parse_log(sys.argv[2])

def collect_points(input_file):
    global log
    y = []
    x = []
    ifd = open(str(path + '/' + input_file), mode='r')
    # print("input_file: " + str(path + '/' + input_file))

    csv_reader = csv.reader(ifd, delimiter=',')

    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            # skip header
            line_count += 1
        else:
            line_count += 1
    
            if(str(row[11]) == 'True'):
                x.append(line_count)
                if(log == True):
                    y.append(np.log10(float(row[10])))
                else:
                    y.append(float(row[10]))

            else:
                x.append(line_count)
                if(log == True):
                    y.append(float(-1))
                else:
                    y.append(float(-1000))
           
    return (x, y)
           
# add support for multiple files at once
files = os.listdir(path)

i = 0
colors = ['black', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']

for f in files:
    if(f[-3:] == 'csv'):
        (x, y) = collect_points(f)
        plt.scatter(x, y,  s=5, alpha=0.5, c=colors[i%8], edgecolor='black', linewidth=1)
        i += 1

plt.title(str(path))
plt.tight_layout()
plt.show()



# add bar plotting to check distribution of performance values 


