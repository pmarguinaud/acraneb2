#!/bin/bash
#SBATCH --nodes=1
#SBATCH -p ndl
#SBATCH --time 00:25:00
#SBATCH --exclusive

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim-sp-fixstack/src

export NV_ACC_CUDA_HEAPSIZE=64Mb

cd ../data

for arch in gpu_d 
do
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk   1 --ncount  1 --check  
done

