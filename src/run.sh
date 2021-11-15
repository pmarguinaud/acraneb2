#!/bin/bash
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:1


set -x
set -e

cd /gpfswork/rech/jau/ufh62jk/acraneb2/openacc-kernels/src

export NV_ACC_CUDA_HEAPSIZE=64Mb

cd ../data

for arch in gpu
do

# ../src/compile.$arch/main.x  --nproma 128 --ngpblk 160 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 640 --ncount 10 --save --check  
  
  for f in *.dat
  do
    mv $f $f.$arch
  done

done

