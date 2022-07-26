#!/bin/bash

sbt "testOnly apps.autotuning.mmCPU"
sbt "testOnly apps.autotuning.stencilTuning"

