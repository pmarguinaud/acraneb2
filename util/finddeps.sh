#!/bin/bash

# find dependencies (modules), generating makefile rules
# probably not failsafe at all, but it does the trick for now

echo "# to generate this file, run ../util/finddeps.sh > Makefile_deps.mk in the src/ directory"

for ff in $(ls *.F90); do
	# find modules used in this source file
	mods=$(grep -oi "^\s*use\s*[a-z0-9_]*" ${ff} | awk -F' ' '{print $2}')
	ffo=""
	if [[ ! -z ${mods} ]]; then
		for mod in ${mods}; do
			# find source file defining this module
			fff=$(grep -i "^\s*module\s*${mod}\s*$" $(ls *.F90) | awk -F':' '{print $1}')
			if [[ -z ${fff} ]]; then
				echo "#WARNING: no source file found containing module ${mod}"
			else
				ffo="${ffo} ${fff%.*}.o"
			fi
		done
	fi
	# write makefile rule
	echo -e "${ff%.*}.o: ${ffo} ${ff}\n\t\$(FC) -c \$(FCOPTS) ${ff}"
done
