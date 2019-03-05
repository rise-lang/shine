#!/bin/bash

git submodule init
git submodule update
git submodule status

cd lib/executor
./buildExecutor.sh