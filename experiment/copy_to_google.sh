#!/bin/bash

scp -i ~/.ssh/google_compute_engine gcloud_setup.sh gcloud_experiment.sh jo@${1}:~
