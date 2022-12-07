#!/usr/bin/python3
from __future__ import annotations
from typing import Optional

import csv
import os
import sys

# import matplotlib.colors as mcolors
# from matplotlib.pyplot import cm

import numpy as np
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt

import seaborn as sns
import seaborn.objects as so


import argparse

from enum import Enum, auto

# plt.style.use('seaborn')

parser = argparse.ArgumentParser()

parser.add_argument("--input", type=str, help="csv file with data")
parser.add_argument("--output", type=str, nargs="?", default="plot.pdf", help="output file")
parser.add_argument("--title", type=str, nargs="?", default="exploration", help="title of plot")
parser.add_argument("--log", action='store_true')

class Plot:
  def __init__(self, input_file: str, title: str, output_file: str, plot_log: bool):
    self.input_file = input_file
    self.title = title
    self.output_file = output_file
    self.plot_log = plot_log

  def __str__(self) -> str:

    information: str = "Plot: \n"

    information += f"input_file: {self.input_file}\n"
    information += f"title: {self.title}\n"
    information += f"output_file: {self.output_file}\n"
    information += f"plot_log: {self.plot_log}\n"

    return information


# parser.add_argument("-v", "--verbosity", action="count", default=0)

# parser.add_argument('--sum', dest='accumulate', action='store_const',
# const=sum, default=max,
# help='sum the integers (default: find the max)')

# # helper
# def get_runtime_index(reader):
#     # get index from header
#
#     # get header
#     header = None
#     for row in reader:
#         header = row
#         break
#
#     # search for runtime in header
#     runtime_index = 0
#     counter = 0
#     for elem in header:
#         if (elem == 'runtime'):
#             runtime_index = counter
#         counter += 1
#
#     return runtime_index

#
# def collect_points(path, input_file, invalid, log=False):
#     # global log
#     y = []
#     x = []
#     ifd = open(str(path + '/' + input_file), mode='r')
#     # print("input_file: " + str(path + '/' + input_file))
#
#     csv_reader = csv.reader(ifd, delimiter=',')
#
#     runtime_index = get_runtime_index(csv_reader)
#
#     line_count = 0
#     for row in csv_reader:
#         if line_count == 0:
#             # skip header
#             line_count += 1
#         else:
#             line_count += 1
#
#             if (str(row[runtime_index + 1]) == 'True'):
#                 x.append(line_count)
#                 if (log == True):
#                     y.append(np.log10(float(row[runtime_index])))
#                 else:
#                     y.append(float(row[runtime_index]))
#
#             else:
#                 x.append(line_count)
#                 if (log == True):
#                     value = np.sign(float(invalid)) * np.log10(abs(float(invalid)))
#                     y.append(value)
#                 else:
#                     value = np.sign(float(invalid)) * abs(float(invalid))
#                     y.append(value)
#
#     return (x, y)


def bar(filename, invalid, title, log, output):

#     ifd = open(str(file), mode='r')

    data = {}
    with open(filename, 'r') as file:
        line_count = 0
        for line in csv.DictReader(file):
            data[line_count] = line
            line_count += 1

    # get elements for each method

    for key in data:
        print(str(key))
        print("\n")
        print(str(data[key]))

    data2 = {}

    for key in data[0]:
        if(str(key) != 'Method'):
            data2[key] = {data[0]['Method']: data[0][key], data[1]['Method']: data[1][key], data[2]['Method']: data[2][key]}

    print("data2: \n" + str(data2))

    for key in data2:
        print("key: " + str(key))
        print("data: " + str(data2[key]))

    for key in data2:
        print("\n")
        print(str(key))
        for method in data2[key]:
            data2[key][method] = np.log10(float(data2[key][method]))
#
    # get baseline
    baselineFactor = 1/float(data2['baseline']['default'])

    print("baselineFactor: " + str(baselineFactor))

    for key in data2:
        print("\n")
        print(str(key))
        for method in data2[key]:
            print("Method: " + str(method))
            print("data: " + str(data2[key][method]))
            data2[key][method] = (1/float(data2[key][method]))/baselineFactor

#     print("\nTest")

#     print(str(data2['fuse']['worst']))

    for key in data2:
        print("key: " + str(key))
        print("data: " + str(data2[key]))

#     labels = []
    labels = ['Baseline - 0', 'Fuse - 0', 'Tile - 2', 'Fission - 2', 'Reorder - 3', 'Vectorize - 4', 'Parallel - 4']
#     for key in data2:
#         labels.append(key)

    # data
    default = []
    worst = []
    tuned = []
    for key in data2:
        default.append(data2[key]['default'])
        worst.append(data2[key]['worst'])
        tuned.append(data2[key]['tuned'])

    print("labels: " + str(labels))
    print("default: " + str(default))
    print("worst: " + str(worst))
    print("tuned: " + str(tuned))

    x = np.arange(len(labels))  # the label locations
    width = 0.2  # the width of the bars

    fig, ax = plt.subplots()
    rects1 = ax.bar(x - width, default, width, label='Default')
    rects2 = ax.bar(x, worst, width, label='Worst')
    rects3 = ax.bar(x + width, tuned, width, label='Tuned')

    # Add some text for labels, title and custom x-axis tick labels, etc.
    ax.set_ylabel('Speedup over Baseline (log)')
    ax.set_title('Rewriting/Tuning')
    ax.set_xticks(x, labels)
    ax.legend()

#     ax.bar_label(rects1, padding=3)
#     ax.bar_label(rects2, padding=3)
#     ax.bar_label(rects3, padding=3)

    fig.tight_layout()

    plt.savefig(output, dpi=1000)
#     plt.show()

    exit(0)


    # speedup over baseline
    # default = (2, 2, 1, 5, 5, 10)
    # worst = (2, 2, 1, 1, 1, 4)
    # tuned = (2, 2, 1, 10, 12, 25)

#     # sum up an split to stack
#     default = (2, 2, 1, 4, 4, 6)
#     worst = (0, 0, 0, 1, 1, 4)
#     tuned = (0, 0, 0, 5, 7, 15)
#
#     colors = ['black', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']
#
#     ind = np.arange(N)  # the x locations for the groups
#     width = 0.35
#     # fig = plt.figure()
#     fig = plt.figure(figsize=(16, 9), dpi=1000)
#     ax = fig.add_axes([0, 0, 1, 1])
#     ax.bar(ind, worst, width, color=colors[0], alpha=1)
#     ax.bar(ind, default, width, color=colors[1], bottom=worst, alpha=1)
#     ax.bar(ind, tuned, width, color=colors[2], bottom=default, alpha=1)
#     ax.set_ylabel('Scores')
#     ax.set_title('Scores by group and gender')
#     # write number of tps to list
#     ax.set_xticks(ind, ('Fuse - 0', 'Tile - 2', 'Fission - 2', 'Reorder - 3', 'Vectorize - 4', 'Parallel'))
#     ax.set_yticks(np.arange(0, 30, 1))
#     ax.legend(labels=['Worst', 'Default', 'Tuned'])
    # plt.show()

    # maybe plot logarithmic

    #
    # # add support for multiple files at once
    # files = os.listdir(folder)
    # fig = plt.figure(figsize=(16, 9), dpi=1000)
    #
    # # todo make full coverage with colors
    # i = 0
    # colors = ['black', 'green', 'red', 'cyan', 'magenta', 'yellow', 'black', 'white']

    # ax = fig.add_axes([0, 0, 1, 1])
    # langs = ['C', 'C++', 'Java', 'Python', 'PHP']
    # students = [23, 17, 35, 29, 12]
    # ax.bar(langs, students)
    # plt.show()

    # for f in files:
    #     if (f[-3:] == 'csv'):
    #         (x, y) = collect_points(folder, f, invalid, log)
    #         plt.scatter(x, y, s=2, alpha=1, c=colors[i % 8], edgecolor='black', linewidth=0, label=str(f[:-4]))
    #         i += 1

    # plt.title(str(title))
    # plt.legend()
    # plt.tight_layout()
    # get current axes
#     ax = plt.gca()
#
#     # hide x-axis
#     ax.get_xaxis().set_visible(True)
#
#     # hide y-axis
#     ax.get_yaxis().set_visible(True)
#
#     plt.savefig(output, dpi=1000)
    return 0


def performance_evolution_grouped(plot: Plot) -> None:

    # todo implement plotting here 

    print(plot)


    # read in information from file 

    # numbers seem to be broken
    data = {}
    with open(plot.input_file, 'r') as file:
        line_count = 0
        for line in csv.DictReader(file):
            data[line_count] = line
            line_count += 1

    # for counter in data:
    #     print("key: " + str(counter))
    #     print("value: " + str(data[counter]))


    # group by high-level hash 
    data_by_hash = {}
    for counter in data:
        hash: str = data[counter]['high-level hash']
        data_by_hash[hash] = []

    # for hash in data_by_hash:
    #     print("hash: " + str(hash))
    #     print("list: " + str(data_by_hash[hash]))
        
    for counter in data:
        hash: str = data[counter]['high-level hash']   
        data_by_hash[hash] = data_by_hash[hash] + [[data[counter]['iteration'], data[counter]['runtime']]]

    # for hash in data_by_hash:
    #     print("hash: " + str(hash))
    #     print("list: " + str(len(data_by_hash[hash])))
        
    # work with data_by_hash
    # indiviual plot for each high-level hash 

    print("rewrites: " + str(len(data_by_hash)) + "\n")

    finished: bool = False
    test_sample = None
    for key in data_by_hash:
        if(not finished):
            finished = True
            test_sample = data_by_hash[key]

        else: 
            break

    for elem in test_sample:
        print(str(elem))

    # plot one line 

    plot_data: list[(int, float)] = []
    counter: int = 0
    for key in data_by_hash:
        for elem in data_by_hash[key]:
            #plot_data.append((int(elem[0]), float(elem[1])))
            counter += 1
            plot_data.append((counter, float(elem[1])))


    for i in plot_data:
        print(str(i))


    # plot 
    x = range(1, len(plot_data) + 1)

    y = []
    for i in plot_data:
        y = y + [np.log10(i[1])]

    # for elem in y:
        # print(str(elem))

    # plt.plot(x, y) 
    plt.scatter(x, y, s=0.5) 


    # plot global performance evolution 

    # get min from y 
    gpe: list[float] = []
    min: float = y[0]
    for i in y:
        if i <= min:
            min = i
        gpe.append(min)

    # get min so far from y 

    plt.plot(x, gpe, color='red', alpha=0.7)
    # plt.plot(x, gpe, color='red', alpha=0.7, linewidth=1)


    # plot grouped performance evolution 

    counter: int = 0

    global_min:float = y[0]

    for i in y:
        if i <= global_min:
            global_min = i

    # get global min and max
    global_max:float = y[0]

    for i in y:
        if i >= global_max:
            global_max = i

    print("global min: " + str(global_min))
    print("global max: " + str(global_max))

    # add 10 percent in both directions
    global_min -= abs(global_min * 0.1)
    global_max += abs(global_min * 0.1)

    plt.vlines(x=counter, ymin=global_min,ymax=global_max, color='black', linewidth=0.5, alpha=0.5)

    for key in data_by_hash:

        grouped_pe: list[float] = []
        grouped_x: list[int] = []
        min: float = float(data_by_hash[key][1][1])

        for elem in data_by_hash[key]:

            if float(elem[1]) <= min:
                min = float(elem[1])

            counter += 1 
            grouped_x.append(counter)
            grouped_pe.append(np.log10(min))

        plt.plot(grouped_x, grouped_pe, color='green', alpha=0.7)

        # set vertical lines 
        plt.vlines(x=counter, ymin=global_min,ymax=global_max, color='black', linewidth=0.5, alpha=0.5)



    plt.show()




    return 


# main here
args = parser.parse_args()

# set args manually 

def create_plot(parser: argparse.ArgumentParser) -> Plot:

    args = parser.parse_args()
    
    plot: Plot = Plot(
        input_file = args.input,
        title = args.title,
        output_file = args.output,
        plot_log= args.log
    )

    return plot


# plot: Plot = create_plot(parser)

plot: Plot = Plot('exploration.csv', 'plot', 'exploration.pdf', False)

performance_evolution_grouped(plot)

# bar(args.src, -1, args.title, args.log, args.output)

