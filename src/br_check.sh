#!/bin/bash

set -x

for x in SIN_ COS_ ASIN ATAN EXP_ LOG_
do
  echo "==> $x <=="

  for p in d s
  do
    diff gpu_${p}0/Z$x.b.dat cpu_${p}0/Z$x.b.dat
    diff gpu_${p}0/Z$x.b.dat gpu_${p}0/Z$x.B.dat
  done

done
