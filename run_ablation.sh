#!/bin/bash

# set iterations
if [ -z "$1" ]; then
  echo '1' > .iterations
else
  echo $1 > .iterations
fi

sbt "testOnly apps.autotuning.ablation"

