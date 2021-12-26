#!/bin/bash
#SBATCH --nodes=1
#SBATCH -p ndl
#SBATCH --time 00:25:00
#SBATCH --exclusive

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim/src

export NV_ACC_CUDA_HEAPSIZE=64Mb

cd ../data

for arch in gpu
do

# ../src/compile.$arch/main.x  --nproma 128 --ngpblk 160 --ncount 10 --save --check  

  nvidia-smi
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1281 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1280 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  960 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  640 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  480 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  320 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  160 --ncount 10 --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk   80 --ncount 10 --save --check  
  
done

