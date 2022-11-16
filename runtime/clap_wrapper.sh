#!/bin/bash

#LD_PRELOAD=./lib/clap/build/libClap.so "$1" "$2"

LD_PRELOAD=./lib/clap/build/libClap.dylib "$1" "$2"

