PROGRAM MAIN

#include "acraneb2.intfb.h"

INTEGER :: JBLK, JLON

!$acc parallel loop gang vector private (JLON,JBLK) collapse (2)
DO JBLK=1, 1
  DO JLON = 1, 32
  CALL ACRANEB2(JLON,JLON,32,1,87)
  ENDDO
ENDDO
!$acc end parallel loop

END PROGRAM MAIN
