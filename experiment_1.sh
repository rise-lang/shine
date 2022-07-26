#!/bin/bash

sbt "testOnly apps.autotuning.harrisCornerDetectionTuning"
sbt "testOnly apps.autotuning.nnTuning"
sbt "testOnly apps.autotuning.mriqTuning"

