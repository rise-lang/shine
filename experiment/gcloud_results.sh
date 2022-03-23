#!/bin/bash

# create output folder
mkdir -p results

# copy back
#scp -r jo@${1}:~/shine/autotuning/mm_1024_test results

rsync -au --info=progress2 jo@${1}:~/shine/autotuning/${2} results
