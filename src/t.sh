#!/bin/bash
#SBATCH --export=NONE
#SBATCH --nodes=1
#SBATCH --account=hun@gpu
#SBATCH --time 00:25:00
#SBATCH --exclusive
#SBATCH --gres=gpu:2


cd /gpfswork/rech/jau/ufh62jk/acraneb2/openacc-kernels/src
module load nvidia-compilers/21.5

set -x
set -e

git=/gpfsstore/rech/jau/ufh62jk/install/git-2.32.0/bin/git

echo "============="


./scripts/compile.pl --arch gpu --update  --compile


if [ 0 -eq 1 ]
then

cd compile.gpu
status=$($git status --porcelain)
if [ "x$status" != "x" ]
then
$git commit -a -m "Compiles"
fi
$git log | head -1
cd ..
fi

cd ../data

for i in $(seq 5)
do
../src/compile.gpu/main.x --nproma 20480 --ncount 10 --ngpblk 1 --check
done

echo "============="

for i in $(seq 5)
do
../src/small_kernels/main --nproma 20480 --ncount 10 --ngpblk 1 --check
done
