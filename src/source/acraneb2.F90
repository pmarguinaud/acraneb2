!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB2( &
#ifdef UNDEF
! - INPUT
 & YDERDI,YDRIP,YDML_PHY_MF,&
#endif
 & KIDIA,KFDIA,KLON,KTDIA,KLEV&
#ifdef UNDEF
 & ,KJN,KSTEP, &
! - INPUT 2D
 & PAPRS,PAPRSF,PCP,PR,PDELP,PNEB,PQ,PQCO2,PQICE,PQLI,PQO3,PT, &
! - INPUT 1D
 & PALB,PALBDIR,PEMIS,PGELAM,PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT, &
! - INPUT/OUTPUT
 & PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, &
 & PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
 & PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR, &
! - OUTPUT 2D
 & PFRSO,PFRTH, &
! - OUTPUT 1D
 & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS, &
! - INPUT 2D x 6
 & PDAER, YDSTACK&
#endif
& )
!$acc routine (ACRANEB2) seq

! Purpose:
! --------
!   ACRANEB2 - Computes radiative fluxes and associated surface diagnostics.
!   Upgraded and modularized version of original ACRANEB routine.

! Interface:
! ----------
! INPUT:
!   KIDIA     - initial index for horizontal loops
!   KFDIA     - final index for horizontal loops
!   KLON      - horizontal dimension of arrays
!   KTDIA     - initial index for vertical loops (usually 1)
!   KLEV      - vertical dimension of full level arrays
!   KJN       - maximum number of day/night intervals within NPROMA slice
!   KSTEP     - current timestep
!   PAPRS     - half level pressure
!   PAPRSF    - full level pressure
!   PCP       - specific heat of moist air at constant pressure
!   PR        - gas constant of moist air
!   PDELP     - pressure thickness of the layer
!   PNEB      - cloud fraction
!   PQ        - specific humidity
!   PQCO2     - specific mass of CO2 with respect to dry air
!   PQICE     - specific mass of cloud ice
!   PQLI      - specific mass of cloud water
!   PQO3      - specific mass of ozone with respect to dry air
!               (element 0 contains ozone column above model top)
!   PT        - temperature
!   PALB      - diffuse surface albedo
!   PALBDIR   - direct (parallel) surface albedo
!   PEMIS     - surface emissivity
!   PGELAM    - longitude
!   PGEMU     - sine of latitude
!   PMU0      - cosine of solar zenithal angle (instantaneous value)
!   PMU0LU    - cosine of lunar zenithal angle (instantaneous value)
!   PTS       - surface temperature
!   PDECRD    - decorrelation depth for cloud overlaps [Pa]
!   PCLCT     - total cloud cover
! INPUT/OUTPUT (for solar/thermal intermittency):
!   PGDEOSI   - min/max descending incremental optical depths, solar
!   PGUEOSI   - min/max ascending incremental optical depths, solar
!   PGMU0     - cosine of solar zenith angle, approximate actual value
!   PGMU0_MIN - cosine of solar zenith angle, min value
!   PGMU0_MAX - cosine of solar zenith angle, max value
!   PGDEOTI   - descending incremental optical depths, dB/dT(T0) weights
!   PGDEOTI2  - descending incremental optical depths, B weights with
!               linear T_e correction
!   PGUEOTI   - ascending incremental optical depths, dB/dT(T0) weights
!   PGUEOTI2  - ascending incremental optical depths, B weights with
!               linear T_e correction
!   PGEOLT    - local optical depths, dB/dT(T0) weights
!   PGEOXT    - maximum optical depths for EBL-EAL, dB/dT(T0) weights
!   PGRPROX   - correction term for adjacent exchanges
!   PGMIXP    - non-statistical weights for bracketing
!   PGFLUXC   - out of bracket part of EBL, RESP. EBL-EAL flux
!   PGRSURF   - corrective ratio for surface CTS contribution
!   PSDUR     - sunshine duration in seconds
! OUTPUT:
!   PFRSO     - net solar   flux (positive downwards)
!   PFRTH     - net thermal flux (positive downwards)
!   PFRSODS   - downward diffuse           solar flux at surface
!   PFRSOPS   - downward direct (parallel) solar flux at surface
!   PFRSOLU   - downward direct + diffuse  lunar flux at surface
!   PFRTHDS   - downward diffuse         thermal flux at surface
! INPUT again:
!   PDAER     - layer optical depths of 6 aerosol types
 
! Externals:
! ----------
!   AC_CLOUD_MODEL2
!   ACRANEB_COEFS
!   ACRANEB_COEFT
!   ACRANEB_SOLVS
!   ACRANEB_SOLVT
!   ACRANEB_SOLVT3
!   ACRANEB_TRANSS
!   ACRANEB_TRANST

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.-F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral      Removed obsolete keys LREWS, LRSTAB.
!                         Removed obsolete solar eclipse computation.
!                         Externalized code of gaseous transmissions
!                         computation, preparation of adding system
!                         coefficients and adding system solvers.
!   2011-06, R. Brozkova  Intermittency for thermal gaseous transmissions.
!   2013-11, J. Masek     New gaseous transmissions and statistical model,
!                         intermittency for bracketing weights, cleaning.
!   2014-11, J. Masek     Solar band computation intermittency.
!   2016-04, J. Masek     True direct solar flux, exponential-random cloud
!                         overlap.
!   2016-09, J. Masek     Revised true direct solar flux, proper calculation
!                         of sunshine duration.
!   2017-09, J. Masek     Optimized SW/LW intermittency in the last timestep,
!                         prevented extrapolation of SW gaseous transmissions.
! End Modifications
!-------------------------------------------------------------------------------

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1 ,ONLY : JPIM     ,JPRB     ,JPRD
USE YOMCST   ,ONLY : RSIGMA   ,RG       ,RPI      ,RDAY

USE YOMRIP   ,ONLY : TRIP
USE YOERDI   ,ONLY : TERDI
USE STACK_MOD
#include "stack.h"

!-------------------------------------------------------------------------------

IMPLICIT NONE

#ifdef UNDEF
TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(INOUT):: YDML_PHY_MF
TYPE(TRIP)        ,INTENT(INOUT):: YDRIP
#endif
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLON 
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV 
#ifdef UNDEF
INTEGER(KIND=JPIM),INTENT(IN) :: KJN
INTEGER(KIND=JPIM),INTENT(IN) :: KSTEP

REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PCP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PR(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PNEB(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQ(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQCO2(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQICE(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQLI(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PQO3(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PALB(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PALBDIR(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PEMIS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PGELAM(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PGEMU(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PMU0(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PMU0LU(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PTS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PDECRD(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PCLCT(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOSI(KLON,0:KLEV,2)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOSI(KLON,0:KLEV,2)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0(KLON,0:YDML_PHY_MF%YRPHY%NSORAYFR-1)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0_MIN(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMU0_MAX(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGDEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGUEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGEOLT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGEOXT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGRPROX(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGMIXP(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGFLUXC(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(INOUT) :: PGRSURF(KLON)
REAL(KIND=JPRB),INTENT(INOUT) :: PSDUR(KLON)
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSO(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRTH(KLON,0:KLEV) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSODS(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSOPS(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRSOLU(KLON) 
REAL(KIND=JPRB),INTENT(OUT)   :: PFRTHDS(KLON) 
REAL(KIND=JPRB),INTENT(IN)    :: PDAER(KLON,KLEV,6)
TYPE(STACK) :: YDSTACK, YLSTACK 
#endif

INTEGER :: JLON, JLEV1, JLEV2

JLON = KIDIA


IF (JLON==1) PRINT *, __FILE__, ':', __LINE__, " JLEV1 = ", KTDIA-1, KLEV
  DO JLEV1=KTDIA-1,KLEV    ! initial half level

IF (JLON==1) PRINT *, __FILE__, ':', __LINE__, " JLEV2 = ", JLEV1+1,KLEV
    DO JLEV2=JLEV1+1,KLEV  ! final half level
IF (JLON==1) THEN
  PRINT *, __FILE__, ':', __LINE__
  PRINT *, " JLEV2 = ", JLEV2
  PRINT *, " JLEV1+1 = ", JLEV1+1
  PRINT *, JLEV2 == JLEV1+1
ENDIF

    ENDDO
  ENDDO

END SUBROUTINE ACRANEB2
