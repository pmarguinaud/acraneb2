#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2


cd /gpfswork/rech/jau/ufh62jk/acraneb2/openacc-kernels-reshape/src
module load nvidia-compilers/21.5

set -x
set -e

./scripts/compile.pl --arch gpu --update  --compile 


cd ../data

#xport PGI_ACC_NOTIFY=1

#../src/compile.gpu/main.x --nproma    32 --ncount  1 --ngpblk   1 --check; exit

 ../src/compile.gpu/main.x --nproma    32 --ncount 10 --ngpblk 640 --check; exit

../src/compile.gpu/main.x --nproma  4096 --ncount 10 --ngpblk   5 --check
../src/compile.gpu/main.x --nproma  2048 --ncount 10 --ngpblk  10 --check
../src/compile.gpu/main.x --nproma  1024 --ncount 10 --ngpblk  20 --check
../src/compile.gpu/main.x --nproma   512 --ncount 10 --ngpblk  40 --check
../src/compile.gpu/main.x --nproma   256 --ncount 10 --ngpblk  80 --check
../src/compile.gpu/main.x --nproma   128 --ncount 10 --ngpblk 160 --check
../src/compile.gpu/main.x --nproma    64 --ncount 10 --ngpblk 320 --check
../src/compile.gpu/main.x --nproma    32 --ncount 10 --ngpblk 640 --check

