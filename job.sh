#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --gpus=1
#SBATCH --time 00:30:00

# or launch in an interactive job with
# srun --pty --nodes=1 --ntasks-per-node=1 --cpus-per-task=10 --gres=gpu:1 --hint=nomultithread

set -x

hostname

# slurm job for profiling acraneb2 on joltik
module swap cluster/joltik
module load NVHPC/20.9


# environment settings
ulimit -c 0
ulimit -s unlimited
export OMP_NUM_THREADS=1
export PGI_ACC_CUDA_HEAPSIZE=$((1024*1024*1024*8))

# debugging
#export NVDEBUG=1
#export NVCOMPILER_ACC_DEBUG=1

# create/clean/goto work directory
cd /user/gent/407/vsc40744/test/git/stack_driver/acraneb2/work

# clean up
rm report_acraneb2_*

# bring executable
ln -sf ../src/main

# bring datafiles
ln -sf ../data/acraneb2.* .

# plain run
if [[ 1 == 1 ]]; then
	date
	./main --nproma 32 --ngpblk 640 --nlev 87 --ncount 10
fi

# run with nsys profiler (high-level)
if [[ 1 == 1 ]]; then
	date
	rm report_acraneb2_nsys*
	nsys profile -t openacc,cuda -f true -o report_acraneb2_nsys ./main --nproma 32 --ngpblk 640 --nlev 87 --ncount 10
fi

# run with ncu profiler (low-level) (not available on joltik)
if [[ 0 == 1 ]]; then
	date
	rm report_acraneb2_ncu*
	ncu --set full -o report_acraneb2_ncu ./main --nproma 32 --ngpblk 640 --ncount 1
fi

# run with nvprof profiler
if [[ 1 == 1 ]]; then
	date
	rm report_acraneb2_nvprof*
	nvprof --print-gpu-trace --log-file report_acraneb2_nvprof ./main --nproma 32 --nlev 87 --ngpblk 640 --ncount 1
fi

date
