#!/bin/bash

for x in PFRSO PFRSODS PFRSOLU PFRSOPS PFRTH PFRTHDS
do
  echo "==> $x <=="
  diff cpu_d0/$x.dat gpu_d0/$x.dat
done
