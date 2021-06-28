#!/bin/bash

set -x
set -e

./scripts/compile.pl --arch gpu --update  --compile

cd ../data
../src/compile.gpu/main.x --nproma 20480 --ncount 10 --ngpblk 1 --check
