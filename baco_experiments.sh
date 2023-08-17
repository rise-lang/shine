#!/bin/bash

sbt "testOnly apps.autotuning.mmCPU"
sbt "testOnly apps.autotuning.mmTuning"
sbt "testOnly apps.autotuning.asumTuning"
sbt "testOnly apps.autotuning.scalTuning"
sbt "testOnly apps.autotuning.kmeans"
sbt "testOnly apps.autotuning.harrisCornerDetectionTuning"
sbt "testOnly apps.autotuning.stencilTuning"

