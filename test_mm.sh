#!/bin/bash

# compile
clang autotuning/generated/mm.c runtime/ocl/buffer_zero_copy.c runtime/ocl/ocl.c -Iruntime/ -Ilib/executor/lib/Executor/include/ -o autotuning/generated/mm -L /usr/lib -lm -lOpenCL -Wno-parentheses-equality

# execute with 1 iteration
autotuning/generated/mm 1
