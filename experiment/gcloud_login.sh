#!/bin/bash

# copy scripts to machine
scp gcloud_setup.sh,gcloud_experiment.sh jo@34.122.44.34:~

# login 
gcloud beta compute ssh --zone "us-central1-a" "instance-1"  --project "compl-323211"


