#!/usr/bin/python3
from __future__ import annotations
from typing import Optional

import csv
import os
import sys

import math

import numpy as np
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt

import seaborn as sns
import seaborn.objects as so

import argparse

from enum import Enum, auto

# import matplotlib.colors as mcolors
# from matplotlib.pyplot import cm

plt.style.use('seaborn')


# set parser 
parser = argparse.ArgumentParser()

# todo add all options to parser 
parser.add_argument("--input", type=str, help="csv file with data")
parser.add_argument("--output", type=str, nargs="?", default="plot.pdf", help="output file")
parser.add_argument("--title", type=str, nargs="?", default="exploration", help="title of plot")
parser.add_argument("--log", action='store_true')

# configuration class 
class Plot:
  def __init__(
    self, input_file: str, 
    title: str, 
    output_file: str, 
    plot_log: bool, 
    upper_limit: int | None = None, 
    fig_size: tuple[int, int] = (10, 10), 
    scatter: bool = False, 
    vertical: bool = False):
    self.input_file = input_file
    self.title = title
    self.output_file = output_file
    self.plot_log = plot_log
    self.upper_limit = upper_limit
    self.fig_size = fig_size
    self.scatter = scatter
    self.vertical = vertical

  def __str__(self) -> str:
    information: str = "Plot: \n"
    information += f"input_file: {self.input_file}\n"
    information += f"title: {self.title}\n"
    information += f"output_file: {self.output_file}\n"
    information += f"plot_log: {self.plot_log}\n"
    information += f"upper_limit: {self.upper_limit}\n"
    information += f"fig_size: {self.fig_size}\n"
    information += f"scatter: {self.scatter}\n"
    information += f"vertical: {self.vertical}\n"

    return information

def create_plot(parser: argparse.ArgumentParser) -> Plot:

    args = parser.parse_args()
    
    plot: Plot = Plot(
        input_file = args.input,
        title = args.title,
        output_file = args.output,
        plot_log= args.log
    )

    return plot

# helper 
def read_data(plot: Plot) -> dict[int, dict[str, str]]: 

    data: dict[int, dict[str, str]]  = {}
    with open(plot.input_file, 'r') as file:
        line_count: int = 0
        for line in csv.DictReader(file):
            data[line_count] = line
            line_count += 1
            if type(plot.upper_limit) == int and line_count >= int(plot.upper_limit):
                break

    return data

def convert_to_performance_evolution(input: list[float]) -> list[float]:
    minimum: float = input[0]
    output: list[float] = []
    for input_elem in input: 
        if input_elem <= minimum:
            minimum = input_elem
        output.append(minimum)
        
    return output

def convert_log(input: list[float]) -> list[float]:
    return list(map(lambda x: np.log10(x), input))

def group_by_hash(input: dict[int, dict[str, str]]) -> dict[str, list[tuple[str, str]]]:

    data_by_hash: dict[str, list[tuple[str, str]]] = {}

    for counter in input:
        hash: str = input[counter]['high-level hash']
        data_by_hash[hash] = []

    for counter in input:
        hash: str = input[counter]['high-level hash']   
        data_by_hash[hash].append((input[counter]['iteration'], input[counter]['runtime']))

    return data_by_hash

def performance_evolution_grouped(plot: Plot) -> None:

    print(plot)
    
    plt.clf()
    plt.figure(figsize=(plot.fig_size))

    data: dict[int, dict[str, str]]  = read_data(plot)
    data_by_hash: dict[str, list[tuple[str, str]]] = group_by_hash(data)

    # be careful, this might be unsorted? 
    # plot one line 
    plot_data: list[tuple[int, float]] = []
    counter: int = 0
    for key in data_by_hash:
        for elem in data_by_hash[key]:
            counter += 1
            plot_data.append((counter, float(elem[1])))

    # plot 
    x = range(1, len(plot_data) + 1)

    y: list[float] = []
    for i in plot_data:
        if plot.plot_log:
            y = y + [np.log10(i[1])]
        else:
            y = y + [i[1]]

    # for elem in y:
        # print(str(elem))

    # plt.plot(x, y) 
    if(plot.scatter):
        plt.scatter(x, y, s=0.5) 

    # plot global performance evolution 

    # get min from y 
    # gpe: list[float] = []
    gpe = convert_to_performance_evolution(y)
    
    # todo change this -> why? 
    plt.plot(x, gpe, color='red', alpha=0.7, linewidth=1, label='Rewriting & Tuning')
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

    global_1: list[float] = []
    global_5: list[float] = []
    global_10: list[float] = []
    global_all: list[float] = []

    if(plot.vertical):
        plt.vlines(x=counter, ymin=global_min,ymax=global_max, color='black', linewidth=0.5, alpha=0.5)

    counter2: int = 0
    for key in data_by_hash:

        grouped_pe: list[float] = []
        grouped_x: list[int] = []
        minimum: float = float(data_by_hash[key][1][1])

        counter3: int = 0
        for elem in data_by_hash[key]:
            
            # todo make this more generic 

            if counter3 <= 0:
                global_1.append(float(elem[1]))
            else:
                global_1.append(min(global_1))
                
            if counter3 <= 5:
                global_5.append(float(elem[1]))
            else:
                global_5.append(min(global_5))
                
            if counter3 <= 10:
                global_10.append(float(elem[1]))
            else:
                global_10.append(min(global_10))
                
            global_all.append(float(elem[1]))

            counter3 += 1

            # global_1.append(list(map(lambda x: float(x[1]), data_by_hash[key][:min(1, len(data_by_hash[key]))])))

            # global_1.append(min(list(map(lambda x: float(x[1]), data_by_hash[key][:min(1, len(data_by_hash[key]))]))))

            # global_5.append(min(list(map(lambda x: float(x[1]), data_by_hash[key][:min(5, len(data_by_hash[key]))]))))

            # global_10.append(min(list(map(lambda x: float(x[1]), data_by_hash[key][:min(10, len(data_by_hash[key]))]))))

            # global_all.append(min(list(map(lambda x: float(x[1]), data_by_hash[key][:len(data_by_hash[key])]))))

            if float(elem[1]) <= minimum:
                minimum = float(elem[1])

            counter += 1 
            grouped_x.append(counter)
            if plot.plot_log:
                grouped_pe.append(np.log10(minimum))
            else:
                grouped_pe.append(minimum)

        if counter2 == 0:
            # plt.plot(list(), list(), color='green', alpha=0.7, label='Tuning')
            plt.plot(grouped_x, grouped_pe, color='green', linewidth=0.7, alpha=0.7, label='Tuning')
            counter2 = 1
        else:
            plt.plot(grouped_x, grouped_pe, color='green', linewidth=0.7, alpha=0.7)


        # set vertical lines 
        if(plot.vertical):
            plt.vlines(x=counter, ymin=global_min,ymax=global_max, color='black', linewidth=0.5, alpha=0.5)

    # make log not war 
    if plot.plot_log:
        global_1 = convert_log(global_1)
        global_5 = convert_log(global_5)
        global_10 = convert_log(global_10)
        global_all = convert_log(global_all)

    global_pe_1: list[float] = convert_to_performance_evolution(global_1)
    global_pe_5: list[float] = convert_to_performance_evolution(global_5)
    global_pe_10: list[float] = convert_to_performance_evolution(global_10)
    global_pe_all: list[float] = convert_to_performance_evolution(global_all)

    plt.plot(x, global_pe_1, color='blue', alpha=0.7, label='Rewriting 0')
    plt.plot(x, global_pe_5, color='black', alpha=0.7, label='Rewriting 5')
    plt.plot(x, global_pe_10, color='yellow', alpha=0.7, label='Rewriting 10')
    plt.plot(x, global_pe_all, color='green', alpha=0.7, label='Rewriting all')

    # set labels 
    if plot.plot_log:
        plt.ylabel("Log10 Runtime (ms)")
    else:
        plt.ylabel("Runtime (ms)")

    plt.title("Rewriting & Parameter-Tuning")
    plt.xlabel("Executed Programs")

    # additional calls 
    plt.tight_layout()
    plt.legend()

    plt.savefig("exploration.pdf", dpi=1000)

    # plt.show()
    # plt.set_xticks(x, labels)
#     ax.bar_label(rects1, padding=3)
#     ax.bar_label(rects2, padding=3)
#     ax.bar_label(rects3, padding=3)

    return None


# todo adjust this
# type this properly 
# write results to file  
def data_analys(plot: Plot) -> None:

    # analysis
    # todo move analysis to own method 

    # get baseline
    # first or first five? 
    baseline_hash: str = data_by_hash[data[0]['high-level hash']]
    # print("baseline: " + str(baseline))
    baselines = []
    counter: int = 0
    for elem in baseline_hash: 
        baselines.append(float(elem[1]))
        counter += 1
        if counter >= 5:
            break

    print("baselines: " + str(sorted(baselines)))

    baseline: float = sorted(baselines)[int(len(baselines)/2)]


    # get best element in total 
    best_performance: float = 10**(float(gpe[-1]))
    speedup_total: float = baseline/best_performance


    print("baseline: " + str(baseline))
    print("best_perfromance: " + str(best_performance))
    print("speedup_total: " + str(speedup_total))

    rewriting_min: float = float(10**float(gpe[0]))
    # print("rewriting_min: " + str(rewriting_min))
    # get min for each group for first elements 
    for elem in data_by_hash:
        counter: int = 0
        random_samples: list[float] = []
        # print("elem: " + str(elem))
        for runtime in data_by_hash[elem]:
            # print(str(runtime))
            random_samples.append(float(runtime[1]))
            counter +=1 
            if counter >= 5:
                break

        # now get median 
        # random_sample: float = sorted(random_samples)[int(len(random_samples)/2)]
        random_sample: float = np.median(np.array(random_samples))
        if random_sample <= rewriting_min and random_sample > 0:
            rewriting_min = random_sample

    print("rewriting_min: " + str(rewriting_min))
    speedup_rewriting: float = baseline/rewriting_min
    print("speedup_rewriting: " + str(speedup_rewriting))

    # tuning only (first group)
    # speedup tuning only  

    # get first group 
    first_tuning_group = data_by_hash[data[0]['high-level hash']]
    # print("tuning group: " + str(first_tuning_group))


    # get speedup per group and get average of this 
    speedups_tuning = []
    for elem in data_by_hash:
        runtimes = []
        for runtime in data_by_hash[elem]:
            if float(runtime[1]) > 0:
                runtimes.append(float(runtime[1]))

        if len(runtimes) > 0:
            # get speedup 
            speedups_tuning.append(runtimes[0]/np.min(np.array(runtimes)))
            

    # print("nympy: " + str(np.array(speedups_tuning)))

    # print("speedups_tuning: " + str(speedups_tuning))
    speedup_tuning2 = np.median(np.array(speedups_tuning))



    print("\n\n \n")

    print("min: " + str(np.min(np.array(speedups_tuning))))
    print("max: " + str(np.max(np.array(speedups_tuning))))
    print("speedup_tuning2: " + str(speedup_tuning2))

    print("\n\n\n")

    tuning_min: float = float(first_tuning_group[0][1])
    for elem in first_tuning_group: 
         if float(elem[1]) <= tuning_min:
            tuning_min = float(elem[1])

    print("tuning_min: " + str(tuning_min))

    speedup_tuning: float = baseline/tuning_min
    
    print("speedup_tuning: " + str(speedup_tuning))


    # speedup per rewirte
    speedup_per_sample_rewrite: float =  speedup_rewriting**(1/len(data_by_hash))

    # speeudp per tuning sample 
    speedup_per_sample_tuning: float =  speedup_tuning**(1/len(first_tuning_group))

    # speedup per sample in total 
    speedup_per_sample_total: float =  speedup_total**(1/limit)

    print("speedup_per_sample_rewrite: " + str(speedup_per_sample_rewrite))
    print("speedup_per_sample_tuning: " + str(speedup_per_sample_tuning))
    print("speedup_per_sample_total: " + str(speedup_per_sample_total))


    # rewrite steps in total 
    print("rewrites: " + str(len(data_by_hash)))


    # get gpe 
    speedups_total: list[float] = []
    for elem in plot_data:
        if baseline/elem[1] >= 1:
            speedups_total.append(baseline/elem[1])
        else:
            speedups_total.append(1)


    total_median = np.median(np.array(speedups_total))
    total_mean = np.mean(np.array(speedups_total))
    # print("speedups_total: " + str(speedups_total))

    print("total median: " + str(total_median))
    print("total mean: " + str(total_mean))

    return 


# todo adjust this 
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
            if plot.plot_log:
                data2[key][method] = np.log10(float(data2[key][method]))
            else: 
                data2[key][method] = float(data2[key][method])
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
    if plot.plot_log:
        ax.set_ylabel('Speedup over Baseline (log)')
    else:
        ax.set_ylabel('Speedup over Baseline')

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



def main():

    # todo parse args (optionaly)

    plot: Plot = Plot('exhaustive_0.csv', 'plot', 'exploration.pdf', plot_log=True, upper_limit=10001, fig_size=(20,10))
    performance_evolution_grouped(plot)

    # bar(args.src, -1, args.title, args.log, args.output)

if __name__ == '__main__':
    main()

