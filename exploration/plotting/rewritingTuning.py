#!/usr/bin/python3
from __future__ import annotations
from typing import Optional

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

parser.add_argument("--src", type=str, help="folder with csv files")
parser.add_argument("--output", type=str, nargs="?", default="plot.pdf", help="output file")
parser.add_argument("--title", type=str, nargs="?", default="Rewriting & Tuning", help="title of plot")
parser.add_argument("--log", action='store_true')


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
    plt.tight_layout()

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


# main here
args = parser.parse_args()

bar(args.src, -1, args.title, args.log, args.output)