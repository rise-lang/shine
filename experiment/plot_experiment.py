import numpy as np
import matplotlib.pyplot as plt

import csv
import os 
import sys
 
path = sys.argv[1]

output = path

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


def process_subfolder(sub_folder):
    files = os.listdir(sub_folder)
    #print('sub_folder: ' + str(sub_folder))
    #print(' files: ' + str(files))
    #mins = np.array(np.empty_like)
    mins = []
    for f in files:
        if(f[-3:] == 'csv'):
            mins.append(process_file(sub_folder, f))
         #  process 

    # mins.getmedian 
    # mins.getmean

    mins_np = np.array(mins)

    print("all: " + str(mins_np))
    print('total mins: ' + str(np.min(mins_np)))
    print('mean: ' + str(np.mean(mins_np)))
    print('median: ' + str(np.median(mins_np)))

    return (np.mean(mins_np), np.median(mins_np), np.min(mins_np))



def process_file(sub_folder, file):
    #print('process file: ' + str(file))
    #print('in: ' + str(sub_folder))
    ifd = open(str(sub_folder + '/' + file), mode='r')

    csv_reader = csv.reader(ifd, delimiter=',')
    runtime_index = get_runtime_index(csv_reader)

    min = 2147483647
    max = 0

    line_count = 0

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

    return min



# add support for multiple files at once
#files = os.listdir(path)

folders = [ (f.name, f.path) for f in os.scandir(path) if f.is_dir() ]
#folders2 = [ f.name for f in os.scandir(path) if f.is_dir() ]

print('folders: ' + str(folders))
#print('folders2: ' + str(folders2))

naive = 1000
expert = 20

data = {}
data['naive'] = {'mean' : naive, 'median': naive, 'min':naive}
data['expert'] = {'mean' : expert, 'median': expert, 'min':expert}
for (name, path) in folders:
    print('folder name: ' + name)
    print('folder path: ' + path)
    sub_path = str(path) + '/' + str(name) + '_' + 'hm'
    print('folder to process: ' + sub_path)
    #name = folder.
    (mean, median, min) = process_subfolder(sub_path)
    data[name] = {'mean': mean, 'median': median, 'min':min}
    #print('mean: ' + str(mean))
    #print('median: ' + str(median))


print('data: ' + str(data))
 
barWidth = 0.3

# creating the dataset
#mean = {'rs_cot':0.8, 'rs_emb':0.7, 'bo_cot':0.95, 'atf_emb':0.85}
#median = {'rs_cot':0.75, 'rs_emb':0.65, 'bo_cot':0.98, 'atf_emb':0.88}

methods = list(data.keys())
#methods = ['rs_cot_harris', 'rs_emb_harris', 'bogp_cot_harris', 'bogplsp_cot_harris', 'atf_emb_harris']

print("for loop")

#default = expert
default = naive


means = {}
medians = {}
mins = {}
for key in data: 
    #print(str(key))
    #print(str(data[key]['mean']))
    #print(str(data[key]['median']))
    means[key] = default/data[key]['mean']
    medians[key] = default/data[key]['median']
    mins[key] = default/data[key]['min']
    #print(str(elem['mean']))
    #print(str(elem['median']))

#valuesMedian = list(median)

print('means: ' + str(means))
print('medians: ' + str(medians))
print('min: ' + str(mins))

print("values: " + str(means.values()))

#meansvalues = []
#meansvalues.append(means['rs_cot_1024'])
#meansvalues.append(means['rs_emb_1024'])
#meansvalues.append(means['bogp_cot_1024'])
#meansvalues.append(means['bogplsp_cot_1024'])
#meansvalues.append(means['atf_emb_1024'])
  
fig = plt.figure(figsize = (15, 5))

br1 = np.arange(len(methods))
br2 = [x + barWidth for x in br1]
br3 = [x + barWidth for x in br2]
 
# creating the bar plot
plt.bar(br1, means.values(), color ='red', width = barWidth, label = 'Mean')
plt.bar(br2, medians.values(), color ='blue', width = barWidth, label = 'Median')
plt.bar(br3, mins.values(), color ='green', width = barWidth, label = 'Total Min')
 
plt.xlabel("Methods")
plt.ylabel("Speedup (naive)")
plt.title("Achieved Performance")

plt.xticks([r + barWidth for r in range(len(methods))], methods)


plt.legend()
#plt.show()

print("output: " + str(output))

plt.savefig(str(output) + '.pdf', dpi=1000)

# todo 
# boxplot
# violinplot
# performance evolution with confidence? "How many iterations are necessary to reach 99% of performance with 90% probablity? 

# plot folder1 folder2 folder3
# foreach folder one bar (tuple (name, value) 
#   foreach file in subfolder
