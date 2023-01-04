#!/bin/bash
#SBATCH --nodes=1
#SBATCH -p ndl
#SBATCH --time 00:25:00
#SBATCH --exclusive

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim-sp-stack-alloc/src

nvidia-smi

cd ../data

../src/compile.gpu_d/main.x  --nproma  32 --ngpblk   1 --ncount  1 --save --check  --alloc 1

for alloc in 1 2 3 4 5
do

if [ "x$alloc" = "x3" ]
then
  unset NV_ACC_CUDA_HEAPSIZE
  heapsize=16000
elif [ "x$alloc" = "x4" ]
then
  unset NV_ACC_CUDA_HEAPSIZE
  heapsize=24000
else
  export NV_ACC_CUDA_HEAPSIZE=64Mb
  heapsize=0
fi

#for arch in gpu_s gpu_d 
for arch in gpu_d 
do

# ../src/compile.$arch/main.x  --nproma  32 --ngpblk   1 --ncount  1 --save --check  --alloc $alloc --heapsize $heapsize
# ../src/compile.$arch/main.x  --nproma  32 --ngpblk   2 --ncount  1 --save --check  --alloc $alloc --heapsize $heapsize
# ../src/compile.$arch/main.x  --nproma  32 --ngpblk  10 --ncount  1 --save --check  --alloc $alloc --heapsize $heapsize

  if [ "$arch" = "gpu_s" ]
  then
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 3840 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 2560 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1600 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  fi
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk   80 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  160 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  320 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  480 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  640 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  960 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1280 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
# ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1281 --ncount 10 --alloc $alloc --heapsize $heapsize # --save --check  
  
done | grep ZTC > ../src/acraneb2.$alloc.dat

done
