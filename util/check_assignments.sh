#!/bin/bash

# script to check if all calculations (i.e. assignments) are inside horizontal loops

ffs="ac_cloud_model2.F90 acraneb_coefs.F90 acraneb_coeft.F90 acraneb_solvs.F90 acraneb_solvt3.F90 acraneb_solvt.F90 acraneb_transs.F90 acraneb_transt.F90 acraneb2.F90"
#ffs=acraneb2.F90

for ff in ${ffs}; do
	echo $ff
	vv=$(cat ../src/${ff} |  awk \
	'BEGIN {IGNORECASE = 1;skip=0;} \
		{ 
			if ($0 ~ "^[\t ]*DO[\t ]*JLON") {
				level=0;
				skip=1;
			}
			if ( skip == 1 ) {
				if ($0 ~ "^[\t ]*DO") {
					level=level+1;
				}
				
				if ($0 ~ "^[\t ]*ENDDO") {
					level=level-1;
				}
				
				if ( level==0 ) {
					skip=0;
				}
			}

			if ( skip==0 ) print $0;

		}' | \
	grep -v -i "^[ \t]*DO" | \
	grep -v -i "^[ \t]*REAL" | \
	grep -v -i "^[ \t]*INTEGER" | \
	grep -v -i "^[ \t]*LOGICAL" | \
	grep -v -i "^[ \t]*CHARACTER" | \
	grep -v -i "^[ \t]*TYPE" | \
	grep -v -i "^[ \t]*CALL" | \
	grep -v -i "^[ \t]*ELSE" | \
	grep -v -i "THEN[ \t]*!.*$" | \
	grep -v -i "THEN[ \t]*$" | \
	grep -v -i "^[ \t]*ASSOCIATE" | \
	grep -v -i "^[ \t]*END" | \
	grep -v -i "^[ \t]*!" | \
	grep -v -i "^[ \t]*$" | \
	grep -v -i "^[ \t]*&" | \
	grep '=' | awk -F'=' '{print $1}' | grep -i -o "[a-z_][a-z0-9_]*")
	
	for v in ${vv}; do
		echo
		echo "  ${v}"
		dd=$(cat ../src/${ff} | grep -i "${v}" | grep '::')
		if [[ -z ${dd} ]]; then
			dd=$(cat ../src/${ff} | grep -i "${v}" | grep '^[ \t]*&' | head -n1)
		fi
		echo "    ${dd}"
	done
	
	echo

done
