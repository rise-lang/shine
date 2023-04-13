#!/bin/bash

sbt "testOnly apps.autotuning.harrisCornerDetectionTuning"
sbt "testOnly apps.autotuning.scalTuning"

