#!/bin/bash

set -x
set -e

make

cd ../data
../src/main  --nproma 32 --ngpblk 4 --ncount 1 --check 
