#!/bin/bash

gpu=${1}
name=${2}
zone="us-east1-c"

# check if name is provided
if [ -z "${name}" ]; then
	echo "no name provided"
	exit 0
else
	echo ${name}
fi

# create vm based on chosen gpu 
if [ ${gpu} = "K80" ]; then
	echo "K80"
	# create vm
	# Tesla K80
	# 30GB RAM
	gcloud compute instances create ${name} --project=compl-323211 --zone=${zone} --machine-type=n1-standard-8 --network-interface=network-tier=PREMIUM,subnet=default --maintenance-policy=TERMINATE --service-account=198284701395-compute@developer.gserviceaccount.com --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --accelerator=count=1,type=nvidia-tesla-k80 --create-disk=auto-delete=yes,boot=yes,device-name=instance-1,image=projects/ml-images/global/images/c0-deeplearning-common-cu113-v20211219-debian-10,mode=rw,size=50,type=projects/compl-323211/zones/us-central1-a/diskTypes/pd-balanced --no-shielded-secure-boot --shielded-vtpm --shielded-integrity-monitoring --reservation-affinity=any

elif [ ${gpu} = "A100" ]; then
	echo "A100"
	# create vm
	# Tesla A100
	# 85GB RAM 
	#
	gcloud compute instances create ${name} --project=compl-323211 --zone=us-central1-a --machine-type=a2-highgpu-1g --network-interface=network-tier=PREMIUM,subnet=default --maintenance-policy=TERMINATE --service-account=198284701395-compute@developer.gserviceaccount.com --scopes=https://www.googleapis.com/auth/devstorage.read_only,https://www.googleapis.com/auth/logging.write,https://www.googleapis.com/auth/monitoring.write,https://www.googleapis.com/auth/servicecontrol,https://www.googleapis.com/auth/service.management.readonly,https://www.googleapis.com/auth/trace.append --accelerator=count=1,type=nvidia-tesla-a100 --create-disk=auto-delete=yes,boot=yes,device-name=instance-2,image=projects/ml-images/global/images/c0-deeplearning-common-cu113-v20211219-debian-10,mode=rw,size=50,type=projects/compl-323211/zones/us-central1-a/diskTypes/pd-balanced --no-shielded-secure-boot --shielded-vtpm --shielded-integrity-monitoring --reservation-affinity=any
else
	echo "error"
fi
