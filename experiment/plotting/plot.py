# plotting script for exploration

import csv
import os
import sys

import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib.pyplot import cm
import numpy as np

import argparse

from enum import Enum, auto

plt.style.use('seaborn')


parser = argparse.ArgumentParser()

parser.add_argument("--plot", type=str, help="type of plot")
parser.add_argument("--src", type=str, help="folder with csv files")
parser.add_argument("--output", type=str, nargs="?", default="scatter.pdf", help="output file")
parser.add_argument("--title", type=str, nargs="?", default="scatter", help="title of plot")
parser.add_argument("--log", action='store_true')

# parser.add_argument("-v", "--verbosity", action="count", default=0)

# parser.add_argument('--sum', dest='accumulate', action='store_const',
                    # const=sum, default=max,
                    # help='sum the integers (default: find the max)')

# helper
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

def collect_points(path, input_file, invalid, log=False):
    #global log
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

# scatter plot 
def scatter(folder, invalid, title, log=False, output="scatter.pdf"):
    print("scatter: " + str(output))



    # add support for multiple files at once
    files = os.listdir(folder)

    # todo make full coverage with colors
    i = 0
    colors = ['black', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']

    for f in files:
        if(f[-3:] == 'csv'):
            (x, y) = collect_points(folder, f, invalid, log)
            plt.scatter(x, y,  s=5, alpha=1, c=colors[i%8], edgecolor='black', linewidth=0)
            i += 1

    plt.title(str(title))
    plt.tight_layout()

    plt.savefig(output, dpi=1000)

    return 0





# main here 
args = parser.parse_args()

if "scatter" in args.plot.lower():
    # print(str(args))
    scatter(args.src, -100, args.title, args.log, args.output)



