#!/bin/bash

set -x

for x in PFRSO PFRSODS PFRSOLU PFRSOPS PFRTH PFRTHDS
do
  echo "==> $x <=="
  diff cpu_d0/$x.dat gpu_d0/$x.dat
  diff cpu_s0/$x.dat gpu_s0/$x.dat
done
