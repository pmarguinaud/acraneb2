#!/bin/bash

set -x
set -e


if [ 0 -eq 1 ]
then

for f in acraneb2.F90 acraneb_solvt.F90 acraneb_coeft.F90 acraneb_transt.F90
do
  ./scripts/copyIfNewer.pl compile.gpu/$f compile.cpu/$f
done

fi



for arch in cpu gpu
do
  ./scripts/compile.pl --arch $arch --update --compile
done

cd ../data

for arch in cpu gpu
do

  ../src/compile.$arch/main.x  --nproma 32 --ngpblk 4 --ncount 1 --save --check  #> stdeo.$arch 2>&1
# ../src/compile.$arch/main.x  --nproma 32 --ngpblk 1 --ncount 1 --save --check  #> stdeo.$arch 2>&1
# ../src/compile.$arch/main.x  --nproma  1 --ngpblk 1 --ncount 1 --save > stdeo.$arch 2>&1
  
  for f in *.dat
  do
    mv $f $f.$arch
  done

done

exit 

vimdiff  -c 'set diffopt+=iwhite'  stdeo.*

