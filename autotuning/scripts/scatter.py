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

def get_runtime_index(reader):
    # get index from header

    # get header
    header = None
    for row in reader: 
        header = row
        break

    # search for runtime in header  
    runtime_index = 0
    counter = 0
    for elem in header:
        if(elem == 'runtime'):
            runtime_index = counter
        counter += 1


    return runtime_index

def collect_points(input_file, invalid):
    global log
    y = []
    x = []
    ifd = open(str(path + '/' + input_file), mode='r')
    # print("input_file: " + str(path + '/' + input_file))

    csv_reader = csv.reader(ifd, delimiter=',')

    runtime_index = get_runtime_index(csv_reader)


    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            # skip header
            line_count += 1
        else:
            line_count += 1
    
            if(str(row[runtime_index+1]) == 'True'):
                x.append(line_count)
                if(log == True):
                    y.append(np.log10(float(row[runtime_index])))
                else:
                    y.append(float(row[runtime_index]))

            else:
                x.append(line_count)
                if(log == True):
                    value = np.sign(float(invalid)) * np.log10(abs(float(invalid)))
                    y.append(value)
                else:
                    value = np.sign(float(invalid)) * abs(float(invalid))
                    y.append(value)
           
    return (x, y)

def get_min_max(f):
    ifd = open(str(path + '/' + input_file), mode='r')

    csv_reader = csv.reader(ifd, delimiter=',')
    min = 2147483647
    max = 0

    for row in csv_reader:
        if line_count == 0:
            # skip header
            line_count += 1
        else:
            line_count += 1
    
            if(str(row[runtime_index+1]) == 'True'):
                # get value
                value = float(row[runtime_index])
                if(value < min):
                    min = value
                elif(value > max):
                    max = value
                else:
                    value = value

    return (min, max)



# parse value for invalid
invalid = sys.argv[2]

# parse log 
if(len(sys.argv) > 3):
    log = parse_log(sys.argv[3])


# get global runtime index
         
# add support for multiple files at once
files = os.listdir(path)

i = 0
colors = ['black', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']

for f in files:
    if(f[-3:] == 'csv'):
        (x, y) = collect_points(f, invalid)
        plt.scatter(x, y,  s=5, alpha=0.5, c=colors[i%8], edgecolor='black', linewidth=1)
        i += 1

plt.title(str(path))
plt.tight_layout()
plt.show()


# add bar plotting to check distribution of performance values 
