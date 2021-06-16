#!/bin/bash

# script to add openacc directives to ALL horizontal loops and put horizontal loops outside of vertical loops

#ffs="fit1.F90"
#ffs="ac_cloud_model2.F90"
#ffs="ac_cloud_model2.F90 acraneb_coefs.F90 acraneb_coeft.F90 acraneb_solvs.F90 acraneb_solvt3.F90 acraneb_solvt.F90 acraneb_transs.F90 acraneb_transt.F90 acraneb2.F90"

ffs="acraneb2.F90"

for ff in ${ffs}; do
	cat ../src_nokernels/${ff} |  awk \
	'BEGIN {ll=0;ls=-999;IGNORECASE = 1; skip=0; level=0; level_start=-999;start_kernel=0;end_kernel=0;in_kernel=0} \
		{ \
			if ($0 ~ "^[\t ]*DO") {
				level=level+1;
			}
			if ($0 ~ "^[\t ]*ENDDO") {
				level=level-1;
			}
			if ( in_kernel == 0 ) {
				if ($0 ~ "! nokernel$") {}
				else {
					if ($0 ~ "^[\t ]*DO[\t ]*JLEV") {
						in_kernel=1;
						level_start=level;
						start_kernel=1;
					}
					if ($0 ~ "^[\t ]*DO[\t ]*JLON") {
						in_kernel=1;
						start_kernel=1;
						level_start=level;
					}
				}
			}
			
			if (level < level_start) {
				end_kernel=1;
				in_kernel=0;
				level_start=-999;
			}

			if ( start_kernel == 1 ) print "!$acc kernels"
			print $0
			if ( end_kernel == 1 ) print "!$acc end kernels"
			start_kernel=0;
			end_kernel=0;
			\
		}' > ../src/${ff}

done
