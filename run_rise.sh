#!/bin/bash

sbt "testOnly apps.autotuning.asumTuning"
sbt "testOnly apps.autotuning.harrisTuning"
sbt "testOnly apps.autotuning.kmeansTuning"
sbt "testOnly apps.autotuning.mmCPU"
sbt "testOnly apps.autotuning.mmTuning"
sbt "testOnly apps.autotuning.scalTuning"
sbt "testOnly apps.autotuning.stencil"
