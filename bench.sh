#!/bin/bash

NAMES=("baseline"
"blocking"
"vectorization"
"loop-perm"
"array-packing"
"cache-blocks"
"parallel")

for name in "${NAMES[@]}"
do
	echo $name
	time ~/.local/bin/sbt "runMain benchmarks.eqsat.mm ${name}" > "results/${name}"
done
