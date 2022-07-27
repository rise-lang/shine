#!/bin/python3.8

import csv
import os
import sys
import copy

from matplotlib import pyplot as plt
import numpy as np
from scipy.stats import sem

# seaborn
plt.style.use('seaborn')

path = sys.argv[1]

# todo change this
output = sys.argv[1]
global_name = path.split('/')[-1]

# set global colors

# colors = ("red", "green", "yellow", "blue", "cornflowerblue", "tomato", "palegreen", "aquamarine")
colors = (
    "tab:red",
    "tab:green",
    "tab:cyan",
    "tab:olive",
    "tab:purple",
    "tab:brown",
    "tab:pink",
    "tab:blue",
    "tab:orange",
    "tab:gray",
)


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
        if ('runtime' in elem):
            runtime_index = counter
        counter += 1

    return runtime_index


def process_subfolder(sub_folder):
    files = os.listdir(sub_folder + "/" + "results")

    fileData = {}
    for f in files:
        if (f[-3:] == 'csv'):
            fileData[f] = process_file(sub_folder + "/" + "results", f)

    return fileData


def process_file(sub_folder, file):
    # print('process file: ' + str(file))
    # print('in: ' + str(sub_folder))
    ifd = open(str(sub_folder + '/' + file), mode='r')

    csv_reader = csv.reader(ifd, delimiter=',')
    runtime_index = get_runtime_index(csv_reader)

    line_count = 0

    data = []

    for row in csv_reader:
        if line_count == 0:
            # skip header
            line_count += 1
        else:
            line_count += 1

            #             print("row: " + str(line_count) + " value: " + str(row[runtime_index]))

            if (str(row[runtime_index]) == '-1'):
                data.append((False, float(2147483647)))
            elif (str(row[runtime_index + 1]) == 'False'):
                data.append((False, float(2147483647)))
            else:
                data.append((True, float(row[runtime_index])))

    return data


def getDefaults(folder):
    ifd = open(str(folder + '/' + 'default.csv'), mode='r')
    default = float(ifd.read())

    ifd = open(str(folder + '/' + 'expert.csv'), mode='r')
    expert = float(ifd.read())

    print('default: ' + str(default))
    print('expert: ' + str(expert))

    return default, expert


def plot_performance_evolution_confidence_internal(default, name, data_internal2, color, log):
    data_internal = {}
    for key in data_internal2:
        internal = []
        for elem in data_internal2[key]:
            internal.append(elem[1])

        data_internal[key] = internal

    # convert to performance evolution
    for key in data_internal:
        pe = []
        minimum = data_internal[key][0]
        for elem in data_internal[key]:
            if elem < minimum:
                minimum = elem
            pe.append(minimum)

        data_internal[key] = pe

    # convert to log
    if (log):
        default = np.log10(default)

    for key in data_internal:
        pe = []
        for elem in data_internal[key]:
            if (log):
                pe.append(np.log10(elem))
            else:
                pe.append(elem)

        data_internal[key] = pe

    # print("data_internal: " + str(data_internal))

    # get mean

    means = []
    means_preparation = []
    confidence = []

    # prepare
    counter = -1
    for key in data_internal:
        counter += 1
        means_preparation.append([])
        for elem in data_internal[key]:
            means_preparation[counter].append(elem)

    # get means
    #     map(lambda x: x*x, numbers)

    minElem = min(list(map(lambda x: len(x), means_preparation)))
    # print("type: " + str(type(means_preparation)))
    # print("minElem: " + str(minElem))
    # print("type: " + str(type(minElem)))
    #     help = 0
    #     if(len(minElem) == 1):
    #         help = minElem
    #     else:
    #         help = min(minElem)

    #     print("help: " + str(help))

    #     for i in range(len(means_preparation[0])):
    for i in range(minElem):
        means_internal = []
        means_internal2 = []
        for file in means_preparation:
            means_internal.append(file[i])
            means_internal2.append(file[i])
        #             means_internal2.append(default / file[i])

        # print("get mean from: " + str(i))
        # print(str(means_internal))
        # print("mean is: " + str(np.mean(np.array(means_internal))))

        #         means.append(default / np.mean(np.array(means_internal)))
        means.append(np.mean(np.array(means_internal)))
        #         means.append(np.mean(np.array(means_internal)))
        confidence.append(sem(means_internal2) * 1.96)
        # confidence.append(1.96 * np.std(means_internal) / np.sqrt(len(means_internal)))

    # print("means: " + str(means))
    # print("confidence: " + str(confidence))

    mins = []
    for key in data_internal:
        mins.append(len(data_internal[key]))

    minimum = min(mins)

    x = range(minimum)

    # means = means[0:2423]

    # df_se = df.groupby('order_hour_of_day').quantity.apply(sem).mul(1.96)

    # Plot
    # x = range(len(means))

    plt.ylabel("Log Runtime (ms)", fontsize=16)
    # qx = means.index
    plt.plot(x, means[0:minimum], alpha=0.8, color=color, lw=2, label=name)

    lower = []
    upper = []
    for i in range(len(means)):
        lower.append(means[i] - confidence[i])
        upper.append(means[i] + confidence[i])

    # plt.fill_between(x, lower, upper, color="#3F5D7D")
    plt.fill_between(x, lower, upper, color=color, alpha=0.2)
    return 0


def plot_performance_evolution_confidence(name, default, expert, data, log):
    # plot time series with error band
    # get performance evolution data

    plt.figure(figsize=(16, 9), dpi=1000)

    counter = 0

    keys = []
    for key in data:
        keys.append(key)

    def sorter(key):
        return data[key][0]

    keys = sorted(keys, key=sorter)

    for key in keys:
        plot_performance_evolution_confidence_internal(default, str(key), data[key][1], colors[counter % len(colors)],
                                                       log)
        counter += 1

    # Decorations
    # Lighten borders
    plt.gca().spines["top"].set_alpha(1)
    plt.gca().spines["bottom"].set_alpha(1)
    plt.gca().spines["right"].set_alpha(1)
    plt.gca().spines["left"].set_alpha(1)
    # plt.xticks(x[::2], [str(d) for d in x[::2]], fontsize=12)
    plt.title(name + " - Performance Evolution (95% confidence)", fontsize=22)
    plt.xlabel("Samples")

    s, e = plt.gca().get_xlim()
    y_low, y_up = plt.gca().get_ylim()
    plt.xlim(s, e)

    # draw expert
    #     plt.axhline(y=default / expert, color='black', linestyle='-', label='Expert')
    if (log):
        expert = np.log10(expert)
        default = np.log10(default)

    plt.axhline(y=expert, color='black', linestyle='-', label='Expert')
    plt.axhline(y=default, color='blue', linestyle='-', label='Default')

    # draw legend
    plt.legend()

    save = str(output) + "/" + str(global_name) + ".pdf"
    # print("save: " + str(save))

    if (log):
        plt.savefig(str(output) + "/" + str(global_name) + '_log.pdf', dpi=1000)
    else:
        plt.savefig(str(output) + "/" + str(global_name) + '.pdf', dpi=1000)

    return 0


# add support for multiple files at once
files = os.listdir(path)

folders = [(f.name, f.path) for f in os.scandir(path) if f.is_dir()]

data = {}


def pather(folder):
    return os.path.getmtime(folder[1])


folders = sorted(folders, key=pather, reverse=False)

for elem in folders:
    print(str(elem[0]))

print("\n\n")

counter = 0
for (name, path) in folders:
    # print('folder name: ' + name)
    #     # print('folder path: ' + path)
    # sub_path = str(path) + '/' + str(name) + '_' + 'hm'
    data[name] = (counter, process_subfolder(path))
    counter += 1

default = float(sys.argv[2])
expert = float(sys.argv[3])

log = True
plot_performance_evolution_confidence(global_name, default, expert, data, log)
