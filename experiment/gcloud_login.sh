#!/bin/bash

# copy scripts to machine
scp gcloud_setup.sh,gcloud_experiment.sh jo@{1}:~

# login 
gcloud beta compute ssh --zone "us-central1-a" "instance-1"  --project "compl-323211"


