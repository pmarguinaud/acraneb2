MODULE WRAPPER_MOD

USE PARKIND1

IMPLICIT NONE

INTERFACE WR

  MODULE PROCEDURE WR1, WR2, WR3

END INTERFACE

CONTAINS

SUBROUTINE WRAPPER(KLON,KLEV,KGPBLK,KCOUNT,LCHECK,LSAVE)

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1 ,ONLY : JPIM     ,JPRB

USE YOMRIP   ,ONLY : TRIP
USE YOERDI   ,ONLY : TERDI

USE PREPARE_ACRANEB2_MOD
USE CHECK_ACRANEB2_MOD

use omp_lib

IMPLICIT NONE

! arguments
INTEGER(KIND=JPIM), INTENT(IN) :: KLON
INTEGER(KIND=JPIM), INTENT(IN) :: KLEV
INTEGER(KIND=JPIM), INTENT(IN) :: KGPBLK
INTEGER(KIND=JPIM), INTENT(IN) :: KCOUNT
LOGICAL, INTENT(IN) :: LCHECK, LSAVE

#include "acraneb2.intfb.h"

! acraneb arguments
TYPE(TERDI)        :: YDERDI
TYPE(MODEL_PHYSICS_MF_TYPE) :: YDML_PHY_MF
TYPE(TRIP)         :: YDRIP
INTEGER(KIND=JPIM) :: KIDIA 
INTEGER(KIND=JPIM) :: KFDIA 
INTEGER(KIND=JPIM) :: KTDIA 
INTEGER(KIND=JPIM) :: KJN
INTEGER(KIND=JPIM) :: KSTEP
REAL(KIND=JPRB) :: PAPRS(KLON,0:KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PAPRSF(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PCP(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PR(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PDELP(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PNEB(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PQ(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PQCO2(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PQICE(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PQLI(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PQO3(KLON,0:KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PT(KLON,KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PALB(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PALBDIR(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PEMIS(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PGELAM(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PGEMU(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PMU0(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PMU0LU(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PTS(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PDECRD(KLON,KGPBLK)
REAL(KIND=JPRB) :: PCLCT(KLON,KGPBLK)
REAL(KIND=JPRB) :: PGDEOSI(KLON,0:KLEV,2,KGPBLK)
REAL(KIND=JPRB) :: PGUEOSI(KLON,0:KLEV,2,KGPBLK)
REAL(KIND=JPRB) :: PGMU0(KLON,0:0,KGPBLK)
REAL(KIND=JPRB) :: PGMU0_MIN(KLON,KGPBLK)
REAL(KIND=JPRB) :: PGMU0_MAX(KLON,KGPBLK)
REAL(KIND=JPRB) :: PGDEOTI(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGDEOTI2(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGUEOTI(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGUEOTI2(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGEOLT(KLON,KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGEOXT(KLON,KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGRPROX(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGMIXP(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGFLUXC(KLON,0:KLEV,KGPBLK)
REAL(KIND=JPRB) :: PGRSURF(KLON,KGPBLK)
REAL(KIND=JPRB) :: PSDUR(KLON,KGPBLK)
REAL(KIND=JPRB) :: PFRSO(KLON,0:KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PFRTH(KLON,0:KLEV,KGPBLK) 
REAL(KIND=JPRB) :: PFRSODS(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PFRSOPS(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PFRSOLU(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PFRTHDS(KLON,KGPBLK) 
REAL(KIND=JPRB) :: PDAER(KLON,KLEV,6,KGPBLK) 

real(kind=8) :: ts,te
REAL(KIND=8) :: TSC, TEC, TSD, TED, ZTC, ZTD

INTEGER :: JCOUNT, JBLK, JLON


CALL PREPARE_ACRANEB2(YDERDI,YDRIP,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KJN,KSTEP, &
 & KGPBLK, &
 & PAPRS,PAPRSF,PCP,PR,PDELP,PNEB,PQ,PQCO2,PQICE,PQLI,PQO3,PT, &
 & PALB,PALBDIR,PEMIS,PGELAM,PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT, &
 & PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, &
 & PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
 & PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR, &
 & PFRSO,PFRTH, &
 & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS, &
 & PDAER)

ts=omp_get_wtime()

ZTD = 0.
ZTC = 0.

DO JCOUNT=1,KCOUNT


#ifdef UNDEF

!$OMP PARALLEL DO PRIVATE(JBLK)
DO JBLK=1,KGPBLK
  CALL ACRANEB2( &
   & YDERDI,YDRIP,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KJN,KSTEP, &
  ! - INPUT 2D
   & PAPRS(:,:,JBLK),PAPRSF(:,:,JBLK),PCP(:,:,JBLK),PR(:,:,JBLK),PDELP(:,:,JBLK),PNEB(:,:,JBLK), &
   & PQ(:,:,JBLK),PQCO2(:,:,JBLK),PQICE(:,:,JBLK),PQLI(:,:,JBLK),PQO3(:,:,JBLK),PT(:,:,JBLK), &
  ! - INPUT 1D
   & PALB(:,JBLK),PALBDIR(:,JBLK),PEMIS(:,JBLK),PGELAM(:,JBLK), &
   & PGEMU(:,JBLK),PMU0(:,JBLK),PMU0LU(:,JBLK),PTS(:,JBLK),PDECRD(:,JBLK),PCLCT(:,JBLK), &
  ! - INPUT/OUTPUT
   & PGDEOSI(:,:,:,JBLK),PGUEOSI(:,:,:,JBLK),PGMU0(:,:,JBLK),PGMU0_MIN(:,JBLK),PGMU0_MAX(:,JBLK), &
   & PGDEOTI(:,:,JBLK),PGDEOTI2(:,:,JBLK),PGUEOTI(:,:,JBLK),PGUEOTI2(:,:,JBLK),PGEOLT(:,:,JBLK),PGEOXT(:,:,JBLK), &
   & PGRPROX(:,:,JBLK),PGMIXP(:,:,JBLK),PGFLUXC(:,:,JBLK),PGRSURF(:,JBLK),PSDUR(:,JBLK), &
  ! - OUTPUT 2D
   & PFRSO(:,:,JBLK),PFRTH(:,:,JBLK), &
  ! - OUTPUT 1D
   & PFRSODS(:,JBLK),PFRSOPS(:,JBLK),PFRSOLU(:,JBLK),PFRTHDS(:,JBLK), &
  ! - INPUT 2D x 6
   & PDAER(:,:,:,JBLK))
ENDDO
!$OMP END PARALLEL DO

#else

    TSD = OMP_GET_WTIME ()

!$acc data &
!$acc   copyin  (YDERDI,YDRIP,YDML_PHY_MF,PAPRS,PAPRSF,PCP,PR,PDELP,PNEB,PQ,PQCO2,PQICE,PQLI,PQO3,PT, &
!$acc            PALB,PALBDIR,PEMIS,PGELAM,PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT,PDAER) &
!$acc   copy    (PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, &
!$acc            PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
!$acc            PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR) &
!$acc   copyout (PFRSO,PFRTH,PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS) 

    TSC = OMP_GET_WTIME ()

  CALL ACRANEB2( &
   & YDERDI,YDRIP,YDML_PHY_MF,KIDIA,KFDIA,KGPBLK,KLON,KTDIA,KLEV,KJN,KSTEP, &
  ! - INPUT 2D
   & PAPRS,PAPRSF,PCP,PR,PDELP,PNEB, PQ,PQCO2,PQICE,PQLI,PQO3,PT, &
  ! - INPUT 1D
   & PALB,PALBDIR,PEMIS,PGELAM, PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT, &
  ! - INPUT/OUTPUT
   & PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
   & PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR, &
  ! - OUTPUT 2D
   & PFRSO,PFRTH, &
  ! - OUTPUT 1D
   & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS, &
  ! - INPUT 2D x 6
   & PDAER)

    TEC = OMP_GET_WTIME ()

!$acc end data

    TED = OMP_GET_WTIME ()

    ZTC = ZTC + (TEC - TSC)
    ZTD = ZTD + (TED - TSD)

#endif

ENDDO

te=omp_get_wtime()
write (*,'(A,F8.2,A)') 'elapsed time : ',te-ts,' s'
write (*,'(A,F8.4,A)') '          i.e. ',1000.*(te-ts)/(KLON*KGPBLK)/KCOUNT,' ms/gp'

PRINT *, " ZTD = ", ZTD, ZTD / REAL (KLON*KGPBLK*KCOUNT, JPRB)
PRINT *, " ZTC = ", ZTC, ZTC / REAL (KLON*KGPBLK*KCOUNT, JPRB)


! check output
IF ( LCHECK ) THEN
	CALL CHECK_ACRANEB2(KLON,KLEV, KGPBLK, &
	 & PFRSO,PFRTH, &
	 & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS)
ENDIF

IF (LSAVE) THEN
CALL WR ('PFRSO.dat',   PFRSO)
CALL WR ('PFRTH.dat',   PFRTH)
CALL WR ('PFRSODS.dat', PFRSODS)
CALL WR ('PFRSOPS.dat', PFRSOPS)
CALL WR ('PFRSOLU.dat', PFRSOLU)
CALL WR ('PFRTHDS.dat', PFRTHDS)
ENDIF

END SUBROUTINE WRAPPER

SUBROUTINE WR1 (CDFILE, P1)

CHARACTER (LEN=*) :: CDFILE
REAL(KIND=JPRB)   :: P1 (:)

INTEGER :: J1

OPEN (77, FILE=TRIM (CDFILE), FORM="FORMATTED")

DO J1 = 1, SIZE (P1, 1)
WRITE (77, '(I5,E25.17)') J1, P1 (J1)
ENDDO

CLOSE (77)

END SUBROUTINE

SUBROUTINE WR2 (CDFILE, P2)

CHARACTER (LEN=*) :: CDFILE
REAL(KIND=JPRB)   :: P2 (:,:)

INTEGER :: J1, J2

OPEN (77, FILE=TRIM (CDFILE), FORM="FORMATTED")

DO J1 = 1, SIZE (P2, 1)
DO J2 = 1, SIZE (P2, 2)
WRITE (77, '(I5,I5,E25.17)') J1, J2, P2 (J1, J2)
ENDDO
ENDDO

CLOSE (77)

END SUBROUTINE

SUBROUTINE WR3 (CDFILE, P3)

CHARACTER (LEN=*) :: CDFILE
REAL(KIND=JPRB)   :: P3 (:,:,:)

INTEGER :: J1, J2, J3

OPEN (77, FILE=TRIM (CDFILE), FORM="FORMATTED")

DO J1 = 1, SIZE (P3, 1)
DO J2 = 1, SIZE (P3, 2)
DO J3 = 1, SIZE (P3, 3)
WRITE (77, '(I5,I5,I5,E25.17)') J1, J2, J3, P3 (J1, J2, J3)
ENDDO
ENDDO
ENDDO

CLOSE (77)

END SUBROUTINE

END MODULE WRAPPER_MOD
