
# run test
cd shine
git checkout autotuning_benchmarks
sbt "testOnly apps.autotuning.mmTuning"

# save results
# scp back 
