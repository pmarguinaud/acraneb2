#!/bin/bash

set -x
set -e

cd ../data



for arch in cpu gpu
do

  ../src/compile.$arch/main.x  --nproma 32 --ngpblk 4 --ncount 1 --save > stdeo.$arch 2>&1
  
  for f in *.dat
  do
    mv $f $f.$arch
  done

done



exit

for f in *.dat
do
  diff $f.ref $f
done


