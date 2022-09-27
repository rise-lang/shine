#!/bin/bash

clang -O2 autotuning/generated/mm.c runtime/ocl/buffer_zero_copy.c runtime/ocl/ocl.c -Iruntime/ -Ilib/executor/lib/Executor/include/ -o autotuning/generated/code -L /usr/lib -lm -lOpenCL -Wno-parentheses-equality

autotuning/generated/code 10
