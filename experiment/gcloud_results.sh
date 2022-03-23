#!/bin/bash

# create output folder
mkdir -p results

# copy back
#scp -r jo@${1}:~/shine/autotuning/mm_1024 results

user="jo"

# use rsync dry run
#rsync -aunv ${user}@${1}:~/shine/autotuning/harris_test results
# use rsync
rsync -au --info=progress2 ${user}@${1}:~/shine/autotuning/${2} results



