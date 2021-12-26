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
  export OMP_NUM_THREADS=1
  ../src/compile.$arch/main.x  --nproma  1 --ngpblk 1 --save --check  

done

