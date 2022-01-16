#!/bin/bash

for x in SIN_ COS_ ASIN ATAN EXP_ LOG_
do
  echo "==> $x <=="
  diff gpu_d0/Z$x.b.dat cpu_d0/Z$x.b.dat
  diff gpu_d0/Z$x.b.dat gpu_d0/Z$x.B.dat
done
