#!/bin/bash

# script to add openacc directives to ALL horizontal loops and swap order of horizontal and vertical loops

#ffs="acraneb2.F90"
ffs="ac_cloud_model2.F90 acraneb_coefs.F90 acraneb_coeft.F90 acraneb_solvs.F90 acraneb_solvt3.F90 acraneb_solvt.F90 acraneb_transs.F90 acraneb_transt.F90 acraneb2.F90"


for ff in ${ffs}; do
	if [[ ../src_nokernels/${ff} -nt ../src/${ff} ]]; then
	
		cat ../src_nokernels/${ff} |  awk \
		'BEGIN {ll=0;ls=-999;IGNORECASE = 1; skip=0; level=0; hlevel=-999; start_kernel=0;end_kernel=0;start_hloop=0;end_hloop=0;do_ignore=0;} \
			{ \
				if ($0 ~ "^!\\$thor start ignore" ) do_ignore=1;
				if ($0 ~ "^!\\$thor end ignore" ) do_ignore=0;

				if ( do_ignore == 1 ) {
					print $0;
				} else {
				if ($0 ~ "^[\t ]*DO[\t ]*JLON") {
					hlevel=level;
					start_hloop=1;
				}
				if ($0 ~ "^[\t ]*DO") {
					level=level+1;
					if ( level == 1 ) start_kernel=1;
				}
				if ($0 ~ "^[\t ]*ENDDO") {
					level=level-1;
					if ( level == 0) end_kernel=1;
					if ( level == hlevel ) {
						end_hloop=1;
						hlevel=-999;
					}
				}
				
				if (level < level_start) {
					end_kernel=1;
					level_start=-999;
				}

				if ( start_kernel == 1 ) print "!$acc loop vector\ndo jlon=kidia,kfdia\n"
				if ( start_hloop==0 && end_hloop==0 ) {
					print $0;
				} else {
					print "! removed hloop : " $0;
				}
				if ( end_kernel == 1 ) print "enddo\n!$acc end loop"
				start_kernel=0;
				end_kernel=0;
				start_hloop=0
				end_hloop=0;
				}
				\
			}' > ../src/${ff}
	fi
done
