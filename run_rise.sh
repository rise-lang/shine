#!/bin/bash

# set iterations
if [ -z "$1" ]; then
  echo '1' > .iterations
else
  echo $1 > .iterations
fi

sbt "testOnly apps.autotuning.asumTuning"
#sbt "testOnly apps.autotuning.harrisTuning"
#sbt "testOnly apps.autotuning.kmeansTuning"
#sbt "testOnly apps.autotuning.mmCPU"
#sbt "testOnly apps.autotuning.mmTuning"
#sbt "testOnly apps.autotuning.scalTuning"
#sbt "testOnly apps.autotuning.stencil"
