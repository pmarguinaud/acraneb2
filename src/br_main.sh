#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --export="NONE"
#SBATCH -p ndl

module load nvhpc

set -x
set -e

cd /scratch/work/marguina/acraneb2/single-directive-redim-sp-bitrep/src

ulimit -s unlimited
export OMP_STACKSIZE=8Gb

for arch in gpu_d0 cpu_d0 gpu_s0 cpu_s0
do
  \rm -rf $arch
  mkdir -p $arch
  cd $arch
  ../compile.gpu_d0/br_main.x
  cd ..
done

