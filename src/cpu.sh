#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --export="NONE"
#SBATCH -p normal256

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim-sp/src

ulimit -s unlimited
export OMP_STACKSIZE=8Gb

cd ../data

for arch in cpu_s cpu_d
do

  cp ../src/linux_bind.txt .
  export OMP_NUM_THREADS=64
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 3840 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 2560 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1280 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  960 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  640 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  480 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  320 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk  160 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk   80 --ncount 10  # --save --check  

  ../src/compile.$arch/main.x  --nproma  16 --ngpblk 3840 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk 2560 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk 1280 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk  960 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk  640 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk  480 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk  320 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk  160 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  16 --ngpblk   80 --ncount 10  # --save --check  

  ../src/compile.$arch/main.x  --nproma  64 --ngpblk 3840 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk 2560 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk 1280 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk  960 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk  640 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk  480 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk  320 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk  160 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma  64 --ngpblk   80 --ncount 10  # --save --check  

  ../src/compile.$arch/main.x  --nproma 128 --ngpblk 3840 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk 2560 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk 1280 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk  960 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk  640 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk  480 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk  320 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk  160 --ncount 10  # --save --check  
  ../src/compile.$arch/main.x  --nproma 128 --ngpblk   80 --ncount 10  # --save --check  

done

