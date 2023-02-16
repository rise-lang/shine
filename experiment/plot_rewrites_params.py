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


def get_index(name, sub_folder, file):
    ifd = open(str(sub_folder + '/' + file), mode='r')
    reader = csv.reader(ifd, delimiter=',')

    # get index from header
    # get header
    header = None
    for row in reader:
        header = row
        break
    # search for runtime in header
    index = 0
    counter = 0
    for elem in header:
        if (str(name) in elem):
            index = counter
        counter += 1

    return index


def process_subfolder(sub_folder):
    files = os.listdir(sub_folder + "/" + "csv")
    # print('sub_folder: ' + str(sub_folder))
    # print(' files: ' + str(files))
    # mins = np.array(np.empty_like)
    fileData = {}
    for f in files:
        if (f[-3:] == 'csv'):
            fileData[f] = process_file(sub_folder + "/" + "csv", f)

    return fileData


def process_file(sub_folder, file):
    # print('process file: ' + str(file))
    # print('in: ' + str(sub_folder))
    ifd = open(str(sub_folder + '/' + file), mode='r')
    csv_reader = csv.reader(ifd, delimiter=',')

    runtime_index = get_index('runtime', sub_folder, file)
    rewrite_index = get_index('rewrite', sub_folder, file)

    line_count = 0

    data = []

    for row in csv_reader:
        # don't skip header
        if line_count == 0:
            # skip header
            line_count += 1
        else:
            line_count += 1

            # print("row: " + str(row[rewrite_index]))

            if (str(row[runtime_index]) == '-1'):
                # data.append((str(row[rewrite_index]), False, float(2147483647)))
                data.append((str(row[rewrite_index]), False, float(2147483647)))
            elif (str(row[runtime_index + 1]) == 'False'):
                # data.append((str(row[rewrite_index]), False, float(2147483647)))
                data.append((str(row[rewrite_index]), False, float(2147483647)))
            else:
                data.append((str(row[rewrite_index]), True, float(row[runtime_index])))

    return data


def getDefaults(folder):
    ifd = open(str(folder + '/' + 'default.csv'), mode='r')
    default = float(ifd.read())

    ifd = open(str(folder + '/' + 'expert.csv'), mode='r')
    expert = float(ifd.read())

    print('default: ' + str(default))
    print('expert: ' + str(expert))

    return default, expert


def plot_scatter_groups_internal():
    # add code here

    return 1


def plot_scatter_groups(global_name, default, expert, data, log):
    # what to do?
    # process data by grouping

    # only support one iteration? Otherwise, we would end up in different groups!
    # just consider one at first, then adapt this to multiple iterations (different colours, e.g.)

    for key in data:
        for elem in data[key][1]:
            # process data data[key][1][elem]
            print("value: " + str(data[key][1]['exhaustive_0.csv']))
            counter = 0
            x = []
            y = []
            for elem2 in data[key][1][elem]:
                counter += 1
                x.append(counter)
                if (elem2[1] == False):
                    y.append(-2)
                else:
                    y.append(np.log10(elem2[2]))
                # (counter, elem2[2])
                # print(str(elem2[2]))

            # now add vertical lines

            vertical = []
            current = data[key][1][elem][0][0]
            counter = 0
            # add at begin
            # vertical.append(counter)

            # store begin index
            # store end endix

            # ymin = min(y[begin, end])
            # ymax = max(y[begin, end])
            begin = 0
            end = 0

            plt.figure(figsize=(10, 10), dpi=1000)

            width = 0.5

            # add vertical lines
            for elem2 in data[key][1][elem]:
                if (current == elem2[0]):
                    counter += 1
                else:
                    vertical.append(counter + 0.5)
                    end = counter

                    print("min: " + str(min(y[begin:end])))
                    print("max: " + str(max(y[begin:end])))

                    plt.vlines(x=(begin + (end - begin) / 2), ymin=min(y[begin:end]) - 0.05,
                               ymax=max(y[begin:end]) + 0.05,
                               alpha=0.7,
                               linewidth=width,
                               colors='black')
                    # plt.vlines(x=(end + 0.4), ymin=min(y[begin:end]), ymax=max(y[begin:end]), alpha=0.5,
                    #            linewidth=0.5,
                    #            colors='black')

                    counter += 1
                    current = elem2[0]
                    begin = counter

            end = counter
            plt.vlines(x=(begin - 0.4), ymin=min(y[begin:end]) - 0.01, ymax=max(y[begin:end]) + 0.01, alpha=0.7,
                       linewidth=width, colors='black')
            # add line at the end
            # vertical.append(counter)

            # plt.vlines(x=vertical, ymin=min(y), ymax=max(y), alpha=0.5, linewidth=0.5, colors='black')
            plt.scatter(x, y, s=4, alpha=1, c=colors[0], edgecolor='black', linewidth=0)
            plt.title(str(path))
            plt.tight_layout()
            plt.savefig(str(output) + "/" + str(global_name) + '_scatter.pdf', dpi=1000)

    # scatter plot + vertical grouping lines
    # for iteration in data:

    # plot each point independently
    # add vertical line every time a new hash/rewrite occurs

    return 0


# add support for multiple files at once
files = os.listdir(path)

folders = [(f.name, f.path) for f in os.scandir(path) if f.is_dir()]

# print('folders: ' + str(folders))

# (default, expert) = getDefaults(path + '/' + 'manual_configs')

# data = {}
# data['default'] = {'mean': default, 'median': default, 'min': default}
# data['expert'] = {'mean': expert, 'median': expert, 'min': expert}

data = {}
# {folder: {file: {data}}}
# {folder: order, {file: data}}}

print("folders")
for elem in folders:
    print(str(elem[0]))


# print("type: " + str(type(folders)))
# os.path.getmtime(path)

def pather(folder):
    return os.path.getmtime(folder[1])


folders = sorted(folders, key=pather, reverse=False)

print("\n\n")

print("folders")
for elem in folders:
    print(str(elem[0]))

print("\n\n")

counter = 0
for (name, path) in folders:
    print('folder name: ' + name)
    #     # print('folder path: ' + path)
    # sub_path = str(path) + '/' + str(name) + '_' + 'hm'
    data[name] = (counter, process_subfolder(path))
    counter += 1

print("\n\n")

print("data")

for elem in data:
    print(elem)
    print(data[elem][0])

print("\n")

for key in data:
    print("key: " + str(key))
    # for key2 in data[key][1]:
    #     print("key2: " + str(key2))
    # print("key2: " + str(key2))
    # print("data[key]: " + str(data[key]))

# sub_path = "csv"
# data['random'] = process_subfolder(sub_path)

# print('data: ' + str(data))

# now we have data dictionary containing all values of output files

# default = 43.937866
default = 200.0
# expert = 2.64
# expert = 0.009632
expert = 0.012288

# data['default'] = {'default': default}
# data['expert'] = {'expert': expert}

# print("mean_median_min_min")
# plot_mean_median_min(global_name, default, expert, data)
#
# print("mean_median_min_var")
# plot_mean_median_min_var(global_name, default, expert, data)

print("performance_evolution")
# global_name = "exploration"
log = True
plot_scatter_groups(global_name, default, expert, data, log)
print("name: " + str(global_name))

# creating the dataset
# mean = {'rs_cot':0.8, 'rs_emb':0.7, 'bo_cot':0.95, 'atf_emb':0.85}
# median = {'rs_cot':0.75, 'rs_emb':0.65, 'bo_cot':0.98, 'atf_emb':0.88}

# methods = list(data.keys())
# # methods = ['rs_cot_harris', 'rs_emb_harris', 'bogp_cot_harris', 'bogplsp_cot_harris', 'atf_emb_harris']
#
# print("for loop")
#
# default = default
# # default
#
# means = {}
# medians = {}
# mins = {}
# for key in data:
#     # print(str(key))
#     # print(str(data[key]['mean']))
#     # print(str(data[key]['median']))
#     means[key] = default / data[key]['mean']
#     medians[key] = default / data[key]['median']
#     mins[key] = default / data[key]['min']
#     # print(str(elem['mean']))
#     # print(str(elem['median']))
#
# # valuesMedian = list(median)
#
# print('means: ' + str(means))
# print('medians: ' + str(medians))
# print('min: ' + str(mins))
#
# print("values: " + str(means.values()))

# meansvalues = []
# meansvalues.append(means['rs_cot_1024'])
# meansvalues.append(means['rs_emb_1024'])
# meansvalues.append(means['bogp_cot_1024'])
# meansvalues.append(means['bogplsp_cot_1024'])
# meansvalues.append(means['atf_emb_1024'])


# todo
# boxplot
# violinplot
# performance evolution with confidence? "How many iterations are necessary to reach 99% of performance with 90% probablity? 

# plot folder1 folder2 folder3
# foreach folder one bar (tuple (name, value) 
#   foreach file in subfolder
