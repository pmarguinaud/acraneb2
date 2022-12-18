#!/bin/bash
#SBATCH --nodes=1
#SBATCH -p ndl
#SBATCH --time 00:25:00
#SBATCH --exclusive

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim-sp/src

export NV_ACC_CUDA_HEAPSIZE=64Mb

cd ../data


for v in $(cat ../src/list)
do
  ../src/compile.gpu_d.$v/main.x  --nproma  32 --ngpblk   1 --ncount  1 --save --check  
done


exit

#for arch in gpu_s gpu_d 
for arch in gpu_d 
do

  ../src/compile.$arch/main.x  --nproma  32 --ngpblk   1 --ncount  1 --save --check  

  exit

  nvidia-smi
  if [ "$arch" = "gpu_s" ]
  then
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 3840 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 2560 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1600 --ncount 10 # --save --check  
  fi
# ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1281 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1280 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  960 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  640 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  480 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  320 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  160 --ncount 10 # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk   80 --ncount 10 # --save --check  
  
done

