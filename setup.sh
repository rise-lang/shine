#!/bin/bash

git submodule update --init --recursive
git submodule status

# build clap library 
mkdir -p lib/clap/build
cmake -DTRACK_EVENTS=ON -B lib/clap/build lib/clap
make -C lib/clap/build -j8
