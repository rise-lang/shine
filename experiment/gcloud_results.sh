#!/bin/bash

# create output folder
mkdir -p results

# copy back
scp -r jo@34.122.44.34:~/shine/autotuning/mm_1024_test results
