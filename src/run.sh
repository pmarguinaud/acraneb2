#!/bin/bash

set -x
set -e

cd ../data
../src/compile/main.x  --nproma 32 --ngpblk 4 --ncount 1 --check --save

for f in *.dat
do
  diff $f.ref $f
done


