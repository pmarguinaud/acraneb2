!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB2( &
! - INPUT
 & YDERDI,YDRIP,YDML_PHY_MF,KIDIA,KFDIA,KLON,KTDIA,KLEV,KJN,KSTEP, &
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
 & PDAER, YDSTACK)
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

TYPE(TERDI)       ,INTENT(INOUT):: YDERDI
TYPE(MODEL_PHYSICS_MF_TYPE),INTENT(INOUT):: YDML_PHY_MF
TYPE(TRIP)        ,INTENT(INOUT):: YDRIP
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLON 
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA 
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV 
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

#ifdef UNDEF


!-------------------------------------------------------------------------------

temp (REAL(KIND=JPRB), ZBB, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPCUN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFPNUN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFMN, (KLON,0:KLEV))

! CLOUD GEOMETRY COEFFICIENTS
temp (REAL(KIND=JPRB), ZB1, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB2, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB3, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZB4, (KLON,KLEV))

! MATRIX COEFFICIENTS
temp (REAL(KIND=JPRB), ZA1C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1CUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA2C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA3C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA4C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA5C, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA1NUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA2N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA3N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA4N, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZA5N, (KLON,KLEV))

temp (REAL(KIND=JPRB), ZQLI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZQICE, (KLON,KLEV))    

temp (REAL(KIND=JPRB), ZFRTH, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZFRTH, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZFMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZGEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZLEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZXEPC, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4G, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4L, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDA4X, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZDAMP, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTDC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTDN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTMC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZZTMN, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZZTRTH, (KLON,0:KLEV))

temp (REAL(KIND=JPRB), ZMU0I, (KLON))
temp (REAL(KIND=JPRB), ZDM0I, (KLON))
temp (REAL(KIND=JPRB), ZDM0I_MIN, (KLON))
temp (REAL(KIND=JPRB), ZDM0I_MAX, (KLON))
temp (REAL(KIND=JPRB), ZRSURF, (KLON))
REAL(KIND=JPRB) :: ZDRB,ZDRS,ZFLUXE&
  &,ZNMNB,ZNMNH,ZNMXB,ZNMXH&
  &,ZII0,ZMU0,ZMU00,&
  &ZWEIGHT,&
  &ZTRB,ZTRS,ZZTRB,ZZTRS&
  &,ZFRB,ZFRS,ZZFRB,ZZFRS

REAL(KIND=JPRB) :: ZCOLAT,ZCOLON,ZSILON
REAL(KIND=JPRB) :: ZTAUC,ZTAUCUN,ZFRSOPS_TRUE
temp (REAL(KIND=JPRB), ZFRSOPS_C, (KLON))
temp (REAL(KIND=JPRB), ZFRSOPS_CUN, (KLON))
REAL(KIND=JPRB) :: ZFRSOPS_UN

temp (REAL(KIND=JPRB), ZPNER0, (KLON,KLEV,KLEV))
temp (REAL(KIND=JPRB), ZPNER1, (KLON,KLEV,KLEV))
temp (REAL(KIND=JPRB), ZFLUXD, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXL, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXR, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZFLUXC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZRPROX, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZMIXP, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZTAUD, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTAUL, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZTAU, (KLON,0:KLEV,0:KLEV))

! CLOUD/AEROSOL OPTICAL COEFFICIENTS
temp (REAL(KIND=JPRB), ZBSFSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFTI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZBSFTN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOASI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOASN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOATI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOATN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODTI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEODTN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSAI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSAN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSBI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSBN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSNUN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSIDIR, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSN, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZUSI, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOSADIR, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO3SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO4SA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO1TA, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEO2TA, (KLON,KLEV))

! GASEOUS OPTICAL DEPTHS
temp (REAL(KIND=JPRB), ZDEOSA, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOSI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOSI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTI2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTI, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTI2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZEOLT, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOXT, (KLON,KLEV))
temp (REAL(KIND=JPRB), ZEOTI, (KLON,0:KLEV))

temp (LOGICAL, LLMASKS, (KLON))

! LOCAL INTEGER SCALARS
INTEGER(KIND=JPIM) :: IAUCR,ILEV,ICALS,ICALT,ISTEP

#endif

INTEGER(KIND=JPIM) :: JAE,JLEV,JLEV1,JLEV2,JLON,JN,JSTEP

#ifdef UNDEF

! LOCAL LOGICAL SCALARS
LOGICAL :: LLAUTO,LLFDIA,LLIDIA,LLREWS

! LOCAL REAL SCALARS
REAL(KIND=JPRB) :: ZARGLI,ZTRLI,ZEARRT,ZDEBL,ZW,&
  & ZCLOV,ZCNEB,ZCOVSR,ZCOVSR_NEW,ZSIVSR,ZCODT,ZSIDT,&
  & ZEPS1,ZEPSAL,ZIEART,ZSOLLEV,ZEPSNEB,ZSECUR,ZSIGMA,ZUNSCALE,&
  & ZEFFE2,ZTARE2,ZDEL1,ZDEL2,ZFTPP,ZTARSP,ZUSA,ZZEO2TA,ZZEO2SA

#endif

#ifdef UNDEF

!-------------------------------------------------------------------------------

#include "ac_cloud_model2.intfb.h"
#include "acraneb_transs.intfb.h"
#include "acraneb_transt.intfb.h"
#include "acraneb_coefs.intfb.h"
#include "acraneb_coeft.intfb.h"
#include "acraneb_solvs.intfb.h"
#include "acraneb_solvt.intfb.h"
#include "acraneb_solvt3.intfb.h"
#include "abor1.intfb.h"

YLSTACK=YDSTACK

alloc (ZBB)
alloc (ZFDC)
alloc (ZFDN)
alloc (ZFMC)
alloc (ZFMN)
alloc (ZFPC)
alloc (ZFPCUN)
alloc (ZFPN)
alloc (ZFPNUN)
alloc (ZZFDC)
alloc (ZZFDN)
alloc (ZZFMC)
alloc (ZZFMN)
alloc (ZB1)
alloc (ZB2)
alloc (ZB3)
alloc (ZB4)
alloc (ZA1C)
alloc (ZA1CUN)
alloc (ZA2C)
alloc (ZA3C)
alloc (ZA4C)
alloc (ZA5C)
alloc (ZA1N)
alloc (ZA1NUN)
alloc (ZA2N)
alloc (ZA3N)
alloc (ZA4N)
alloc (ZA5N)
alloc (ZQLI)
alloc (ZQICE)
alloc (ZFRTH)
alloc (ZZFRTH)
alloc (ZZZFDC)
alloc (ZZZFDN)
alloc (ZZZFMC)
alloc (ZZZFMN)
alloc (ZGEPC)
alloc (ZLEPC)
alloc (ZXEPC)
alloc (ZDA4G)
alloc (ZDA4L)
alloc (ZDA4X)
alloc (ZDAMP)
alloc (ZTDC)
alloc (ZTDN)
alloc (ZTMC)
alloc (ZTMN)
alloc (ZZTDC)
alloc (ZZTDN)
alloc (ZZTMC)
alloc (ZZTMN)
alloc (ZZZTDC)
alloc (ZZZTDN)
alloc (ZZZTMC)
alloc (ZZZTMN)
alloc (ZZTRTH)
alloc (ZMU0I)
alloc (ZDM0I)
alloc (ZDM0I_MIN)
alloc (ZDM0I_MAX)
alloc (ZRSURF)
alloc (ZFRSOPS_C)
alloc (ZFRSOPS_CUN)
alloc (ZPNER0)
alloc (ZPNER1)
alloc (ZFLUXD)
alloc (ZFLUXL)
alloc (ZFLUXR)
alloc (ZFLUXC)
alloc (ZRPROX)
alloc (ZMIXP)
alloc (ZTAUD)
alloc (ZTAUL)
alloc (ZTAU)
alloc (ZBSFSI)
alloc (ZBSFSN)
alloc (ZBSFTI)
alloc (ZBSFTN)
alloc (ZEO1TI)
alloc (ZEO1TN)
alloc (ZEO2TI)
alloc (ZEO2TN)
alloc (ZEOASI)
alloc (ZEOASN)
alloc (ZEOATI)
alloc (ZEOATN)
alloc (ZEODSI)
alloc (ZEODSN)
alloc (ZEODTI)
alloc (ZEODTN)
alloc (ZUSAI)
alloc (ZUSAN)
alloc (ZUSBI)
alloc (ZUSBN)
alloc (ZEO2SN)
alloc (ZEO2SI)
alloc (ZEO1SN)
alloc (ZEO1SI)
alloc (ZEOSN)
alloc (ZEOSNUN)
alloc (ZEOSI)
alloc (ZEOSIDIR)
alloc (ZUSN)
alloc (ZUSI)
alloc (ZEOSA)
alloc (ZEOSADIR)
alloc (ZEO1SA)
alloc (ZEO2SA)
alloc (ZEO3SA)
alloc (ZEO4SA)
alloc (ZEO1TA)
alloc (ZEO2TA)
alloc (ZDEOSA)
alloc (ZDEOSI)
alloc (ZUEOSI)
alloc (ZDEOTI)
alloc (ZDEOTI2)
alloc (ZUEOTI)
alloc (ZUEOTI2)
alloc (ZEOLT)
alloc (ZEOXT)
alloc (ZEOTI)
alloc (LLMASKS)


#endif

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
