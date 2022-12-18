#!/bin/bash

set -x
set -e


for v in 22.5 22.7
do
  cd compile.gpu_d.$v
  module load nvhpc/$v
  make main.x
  module purge
  cd ..
  cd ../data
  ../src/compile.gpu_d.$v/main.x --nproma  32 --ngpblk 1 --ncount 1 --save --check > ../src/run.$v.txt 2>&1
  cd ../src
done


exec vim -d run.*.txt

