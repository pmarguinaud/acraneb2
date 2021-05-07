#!/bin/bash

# modules on nori
module load foss/2019b

# compile
make -C src -f Makefile.gnu

# environment settings
export OMP_NUM_THREADS=4
ulimit -s unlimited

# create/clean/goto work directory
mkdir -p work
cd work
rm -f main acraneb2.in acraneb2.out

# bring executable
ln -sf ../src/main

# bring datafiles
ln -sf ../data/acraneb2.* .

# run!
time ./main --nproma 32 --ngpblk 16 --nlev 87 --check
 
