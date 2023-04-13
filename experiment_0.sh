#!/bin/bash

sbt "testOnly apps.autotuning.mmTuning"
#sbt "testOnly apps.autotuning.convolutionTuning"
sbt "testOnly apps.autotuning.kmeans"
