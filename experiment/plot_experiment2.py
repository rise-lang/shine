import numpy as np
import matplotlib.pyplot as plt

from scipy.stats import sem

import csv
import os
import sys

import copy

path = sys.argv[1]

output = path
global_name = path.split('/')[-1]

# set global colors

# colors = ("red", "green", "yellow", "blue", "cornflowerblue", "tomato", "palegreen", "aquamarine")
colors = (
    "tab:gray",
    "tab:green",
    "tab:red",
    "tab:cyan",
    "tab:olive",
    "tab:purple",
    "tab:brown",
    "tab:pink",
    "tab:blue",
    "tab:orange",
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
        if (elem == 'runtime'):
            runtime_index = counter
        counter += 1

    return runtime_index


def process_subfolder(sub_folder):
    files = os.listdir(sub_folder)
    # print('sub_folder: ' + str(sub_folder))
    # print(' files: ' + str(files))
    # mins = np.array(np.empty_like)
    fileData = {}
    for f in files:
        if (f[-3:] == 'csv'):
            fileData[f] = process_file(sub_folder, f)

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

            if (str(row[runtime_index + 1]) == 'True'):
                data.append((True, float(row[runtime_index])))
            else:
                data.append((False, float(2147483647)))

    return data


def getDefaults(folder):
    ifd = open(str(folder + '/' + 'default.csv'), mode='r')
    default = float(ifd.read())

    ifd = open(str(folder + '/' + 'expert.csv'), mode='r')
    expert = float(ifd.read())

    print('default: ' + str(default))
    print('expert: ' + str(expert))

    return default, expert


def plot_mean_median_min_var(name, default, expert, data):
    mins = {}
    medians = {}
    means = {}
    variances = {}

    # remove invalid values
    data_internal = copy.deepcopy(data)
    for key in data_internal:
        for file in data_internal[key]:
            valid = []
            for elem in data_internal[key][file]:
                # if elem[0] == True:
                valid.append(elem[1])
            data_internal[key][file] = valid

    for key in data_internal:
        print("key: " + str(key))
        min = []
        for file in data_internal[key]:
            min.append(np.min(np.array(data_internal[key][file])))

        np_array = np.array(min)

        mins[key] = default / np.min(np_array)
        medians[key] = default / np.median(np_array)
        means[key] = default / np.mean(np_array)
        variances[key] = np.var(np_array)

        # print("key: " + str(key))
        # print("min: " + str(mins[key]))
        # print("mean: " + str(means[key]))
        # print("median: " + str(medians[key]))

    # let's process it

    # print("mins: " + str(mins))
    # print("medians: " + str(medians))
    # print("means: " + str(means))
    # print("variances: " + str(variances))

    methods = data_internal.keys()

    barWidth = 0.2

    # fig = plt.figure(figsize=(15, 5))

    # using subplots() function

    fig, ax = plt.subplots(figsize=(15, 8))

    # using the twinx() for creating
    # another axes

    # Label axes

    br1 = np.arange(len(methods))
    br2 = [x + barWidth for x in br1]
    br3 = [x + barWidth for x in br2]
    br4 = [x + barWidth for x in br3]

    # creating the bar plot
    bar1 = plt.bar(br1, mins.values(), color='red', width=barWidth, label='Total Best')
    bar2 = plt.bar(br2, medians.values(), color='blue', width=barWidth, label='Median')
    bar3 = plt.bar(br3, means.values(), color='green', width=barWidth, label='Mean')
    plt.axhline(y=default / expert, color='black', linestyle='-', label='Expert')

    # plt.legend(bbox_to_anchor=(0.2, 1.13))
    plt.legend()

    ax2 = ax.twinx()
    bar4 = plt.bar(br4, variances.values(), color='yellow', width=barWidth, label='Variance')

    # plt.xlabel("Methods")
    # plt.ylabel("Speedup (over default)")
    plt.title(str(name) + " - Achieved Performance (Speedup)")

    ax.set_xlabel('Methods')
    ax.set_ylabel('Speedup over default')
    ax2.set_ylabel('Variance')

    plt.xticks([r + barWidth for r in range(len(methods))], methods)

    # lns = bar1 + bar2 + bar3 + bar4
    # labels = [l.get_label() for l in lns]
    # plt.legend(lns, labels, loc=0)

    # plt.legend(bbox_to_anchor=(0.35, 1.065))
    plt.legend()
    # plt.show()

    print("output: " + str(output))

    plt.savefig(str(output) + '_mean_median_min_var.pdf', dpi=1000)

    return 0


def plot_mean_median_min(name, default, expert, data):
    mins = {}
    medians = {}
    means = {}

    # remove invalid values
    data_internal = copy.deepcopy(data)
    for key in data_internal:
        for file in data_internal[key]:
            valid = []
            for elem in data_internal[key][file]:
                # if elem[0] == True:
                valid.append(elem[1])
            data_internal[key][file] = valid

    for key in data_internal:
        print("key: " + str(key))
        min = []
        for file in data_internal[key]:
            min.append(np.min(np.array(data_internal[key][file])))

        np_array = np.array(min)

        mins[key] = default / np.min(np_array)
        medians[key] = default / np.median(np_array)
        means[key] = default / np.mean(np_array)

        # print("key: " + str(key))
        # print("min: " + str(mins[key]))
        # print("mean: " + str(means[key]))
        # print("median: " + str(medians[key]))

    # let's process it

    # print("mins: " + str(mins))
    # print("medians: " + str(medians))
    # print("means: " + str(means))

    methods = data_internal.keys()

    barWidth = 0.2

    # fig = plt.figure(figsize=(15, 5))

    # using subplots() function

    fig, ax = plt.subplots(figsize=(15, 8))

    # using the twinx() for creating
    # another axes

    # Label axes

    br1 = np.arange(len(methods))
    br2 = [x + barWidth for x in br1]
    br3 = [x + barWidth for x in br2]

    # creating the bar plot
    bar1 = plt.bar(br1, mins.values(), color='red', width=barWidth, label='Total Best')
    bar2 = plt.bar(br2, medians.values(), color='blue', width=barWidth, label='Median')
    bar3 = plt.bar(br3, means.values(), color='green', width=barWidth, label='Mean')
    plt.axhline(y=default / expert, color='black', linestyle='-', label='Expert')

    plt.legend()
    #    plt.legend(bbox_to_anchor=(0.2, 1.13))

    # plt.xlabel("Methods")
    # plt.ylabel("Speedup (over default)")
    plt.title(str(name) + " - Achieved Performance (Speedup)")

    ax.set_xlabel('Methods')
    ax.set_ylabel('Speedup over default')

    plt.xticks([r + barWidth for r in range(len(methods))], methods)

    # lns = bar1 + bar2 + bar3
    ##labels = [l.get_label() for l in lns]
    # plt.legend(lns, labels)

    plt.legend()

    # plt.legend(bbox_to_anchor=(0.35, 1.065))
    # plt.show()

    # print("output: " + str(output))

    plt.savefig(str(output) + '_mean_median_min.pdf', dpi=1000)

    return 0


def plot_performance_evolution_confidence_internal(default, name, data_internal2, color):
    data_internal = {}
    for key in data_internal2:
        internal = []
        for elem in data_internal2[key]:
            internal.append(elem[1])

        data_internal[key] = internal

    # convert to performance evolution
    for key in data_internal:
        pe = []
        min = data_internal[key][0]
        for elem in data_internal[key]:
            if elem < min:
                min = elem
            pe.append(min)

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
    for i in range(len(means_preparation[0])):
        means_internal = []
        means_internal2 = []
        for file in means_preparation:
            means_internal.append(file[i])
            means_internal2.append(default / file[i])

        # print("get mean from: " + str(i))
        # print(str(means_internal))
        # print("mean is: " + str(np.mean(np.array(means_internal))))

        means.append(default / np.mean(np.array(means_internal)))
        confidence.append(sem(means_internal2) * 1.96)
        # confidence.append(1.96 * np.std(means_internal) / np.sqrt(len(means_internal)))

    # print("means: " + str(means))
    # print("confidence: " + str(confidence))

    # means

    # df_se = df.groupby('order_hour_of_day').quantity.apply(sem).mul(1.96)

    # Plot
    x = range(len(means))

    plt.ylabel("Speedup (over default)", fontsize=16)
    # qx = means.index
    plt.plot(x, means, color=color, lw=2, label=name)

    lower = []
    upper = []
    for i in range(len(means)):
        lower.append(means[i] - confidence[i])
        upper.append(means[i] + confidence[i])

    # plt.fill_between(x, lower, upper, color="#3F5D7D")
    plt.fill_between(x, lower, upper, color=color, alpha=0.2)
    return 0


def plot_performance_evolution_confidence(name, default, expert, data):
    # plot time series with error band
    # get performance evolution data

    plt.figure(figsize=(16, 10), dpi=1000)

    counter = 0

    keys = []
    for key in data:
        keys.append(key)

    # print("keys: " + str(keys))
    # print("keys: " + str(sorted(keys)))

    for key in sorted(keys):
        plot_performance_evolution_confidence_internal(default, str(key), data[key], colors[counter % len(colors)])
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
    plt.axhline(y=default / expert, color='black', linestyle='-', label='Expert')

    # draw legend
    plt.legend()

    # Draw Horizontal Tick lines
    print("y_up: " + str(y_up))
    step = int(y_up)
    if y_up > 200:
        step = 50
    elif y_up > 50:
        step = 10
    elif y_up > 10:
        step = 2
    elif y_up > 5:
        step = 1
    else:
        step = 1

    print("step: " + str(step))
    for y in range(0, int(y_up) + 1, step):
        plt.hlines(y, xmin=s, xmax=e, colors='black', alpha=0.5, linestyles="--", lw=0.5)

    plt.savefig(str(output) + '_confidence.pdf', dpi=1000)

    return 0


# add support for multiple files at once
# files = os.listdir(path)

folders = [(f.name, f.path) for f in os.scandir(path) if f.is_dir()]

print('folders: ' + str(folders))

(default, expert) = getDefaults(path + '/' + 'manual_configs')

# data = {}
# data['default'] = {'mean': default, 'median': default, 'min': default}
# data['expert'] = {'mean': expert, 'median': expert, 'min': expert}

data = {}
# {folder: {file: {data}}}

for (name, path) in folders:
    # print('folder name: ' + name)
    # print('folder path: ' + path)
    sub_path = str(path) + '/' + str(name) + '_' + 'hm'
    # print('folder to process: ' + sub_path)

    # name = folder.
    if (name != 'manual_configs'):
        data[name] = process_subfolder(sub_path)

# print('data: ' + str(data))

# now we have data dictionary containing all values of output files

# data['default'] = {'default': default}
# data['expert'] = {'expert': expert}
print("mean_median_min_min")
plot_mean_median_min(global_name, default, expert, data)

print("mean_median_min_var")
plot_mean_median_min_var(global_name, default, expert, data)

print("performance_evolution")
plot_performance_evolution_confidence(global_name, default, expert, data)

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