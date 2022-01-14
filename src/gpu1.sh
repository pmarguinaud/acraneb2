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


for arch in gpu_s gpu_d
do
  ./scripts/compile.pl --arch $arch --update --compile
done

cd ../data

for arch in gpu_d gpu_s
do

  cp ../src/linux_bind.txt .
  export OMP_NUM_THREADS=1
  ../src/compile.$arch/main.x  --nproma  32 --ngpblk 1 --save --check > ../src/$arch.eo 2>&1
  mkdir -p $arch
  mv *.dat $arch/.

done

