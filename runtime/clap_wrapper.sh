#!/bin/bash

LD_PRELOAD=./lib/clap/build/libClap.so "$1" "$2"
