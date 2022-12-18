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
INTEGER(KIND=JPIM) :: JAE,JLEV,JLEV1,JLEV2,JLON,JN,JSTEP

! LOCAL LOGICAL SCALARS
LOGICAL :: LLAUTO,LLFDIA,LLIDIA,LLREWS

! LOCAL REAL SCALARS
REAL(KIND=JPRB) :: ZARGLI,ZTRLI,ZEARRT,ZDEBL,ZW,&
  & ZCLOV,ZCNEB,ZCOVSR,ZCOVSR_NEW,ZSIVSR,ZCODT,ZSIDT,&
  & ZEPS1,ZEPSAL,ZIEART,ZSOLLEV,ZEPSNEB,ZSECUR,ZSIGMA,ZUNSCALE,&
  & ZEFFE2,ZTARE2,ZDEL1,ZDEL2,ZFTPP,ZTARSP,ZUSA,ZZEO2TA,ZZEO2SA

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



JLON = KIDIA

!-------------------------------------------------------------------------------


!     I - CALCUL DES PARAMETRES DERIVES, CONSTANTES DE SECURITE (POUR
!     L'EPAISSEUR EN PRESSION DE LA COUCHE "AU DESSUS DU SOMMET DU
!     MODELE", L'HUMIDITE SPECIFIQUE ET LA CORRECTION POUR EVITER UNE
!     SOLUTION ANALYTIQUE RESONNANTE ENTRE RAYONNEMENT PARALLELE ET
!     RAYONNEMENT DIFFUS) AINSI QU'ARGUMENT LIMITE POUR EVITER
!     "L'UNDERFLOW" DANS LES CALCULS DE TRANSMISSION.

!     I - COMPUTATION OF DERIVED PARAMETERS, SECURITY CONSTANTS (FOR THE
!     PRESSURE THICKNESS OF THE LAYER "ABOVE THE MODEL'S TOP, THE
!     SPECIFIC HUMIDITY AND THE CORRECTION TO AVOID A RESONNANT
!     ANALYTICAL SOLUTION BETWEEN PARALLEL AND SCATTERED RADIATION) AS
!     WELL AS A LIMIT ARGUMENT TO AVOID "UNDERFLOW" IN THE COMPUTATIONS
!     OF TRANSMISSIVITY.
!-------------------------------------------------------------------------------

! SECURITY CONSTANTS.
ZEPSNEB=1.E-12_JPRB
ZSECUR=4._JPRB*RSIGMA*RG*YDML_PHY_MF%YRPHY2%TSPHY
ZEPS1=1.E-03_JPRB
ZEPSAL=1.E-04_JPRB
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI=EXP(ZARGLI)

!     II - PRELIMINARY CALCULATIONS.
!-------------------------------------------------------------------------------

! What to compute for gases in solar band:
!   ICALS = 0 => transmissions for current sun elevation
!   ICALS = 1 => transmissions for min/max sun elevation, plus
!                interpolated transmissions between min/max values
!   ICALS = 2 => interpolated transmissions between min/max values only
IF (YDML_PHY_MF%YRPHY%NSORAYFR == 1) THEN
  ICALS=0
ELSEIF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NSORAYFR) == 0) THEN
  IF ( KSTEP == YDRIP%NSTOP ) THEN
    ICALS=0
  ELSE
    ICALS=1
  ENDIF
ELSE
  ICALS=2
ENDIF

! What to update for gases in thermal band:
!   ICALT = 0 => nothing
!   ICALT = 1 => transmissions
!   ICALT = 2 => transmissions and non-statistical weights for bracketing
ICALT=0
IF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NTHRAYFR) == 0) THEN
  ICALT=1
  LLAUTO=.FALSE.
ENDIF
IF (YDML_PHY_MF%YRPHY%NRAUTOEV > 0) THEN
  IF (MOD(KSTEP,YDML_PHY_MF%YRPHY%NTHRAYFR*YDML_PHY_MF%YRPHY%NRAUTOEV) == 0.AND. &
   & (KSTEP < YDRIP%NSTOP.OR.YDRIP%NSTOP == 0)) THEN
    ICALT=2
    LLAUTO=.TRUE.
  ENDIF
ENDIF

!     II.1 - SET QL AND QI DEFINITIONS TO THEIR VALUE INSIDE THE CLOUD.
!     COMPUTE AEROSOL OPTICAL PROPERTIES.
!-------------------------------------------------------------------------------

DO JLEV=KTDIA,KLEV
  
    ZQICE(JLON,JLEV)=PQICE(JLON,JLEV)/MAX(ZEPSNEB,PNEB(JLON,JLEV))
    ZQLI (JLON,JLEV)=PQLI (JLON,JLEV)/MAX(ZEPSNEB,PNEB(JLON,JLEV))
  
ENDDO

! ALPHA1, ALPHA2 FOR AEROSOLS.
! daand: add loops
DO JLEV=KTDIA,KLEV
  
		ZEO2TA(JLON,JLEV)=0._JPRB
		ZEO2SA(JLON,JLEV)=0._JPRB
		ZEO1TA(JLON,JLEV)=0._JPRB
		ZEO1SA(JLON,JLEV)=0._JPRB
		ZEOSA(JLON,JLEV)=0._JPRB
		ZEOSADIR(JLON,JLEV)=0._JPRB
	
ENDDO
DO JAE=1,6
  ZUNSCALE=4._JPRB*YDML_PHY_MF%YRPHY3%USAA(JAE)/(3._JPRB+4._JPRB*YDML_PHY_MF%YRPHY3%USAA(JAE))  ! -g
  ZUNSCALE=1._JPRB/(1._JPRB-ZUNSCALE*ZUNSCALE)            ! 1/(1 - g^2)
  DO JLEV=KTDIA,KLEV
    
      ZZEO2TA=2._JPRB*YDML_PHY_MF%YRPHY3%BSFTA(JAE)*YDML_PHY_MF%YRPHY3%EODTA(JAE)*PDAER(JLON,JLEV,JAE)
      ZZEO2SA=2._JPRB*YDML_PHY_MF%YRPHY3%BSFSA(JAE)*YDML_PHY_MF%YRPHY3%EODSA(JAE)*PDAER(JLON,JLEV,JAE)
      ZEO2TA(JLON,JLEV)=ZEO2TA(JLON,JLEV)+ZZEO2TA
      ZEO2SA(JLON,JLEV)=ZEO2SA(JLON,JLEV)+ZZEO2SA
      ZEO1TA(JLON,JLEV)=ZEO1TA(JLON,JLEV)+ZZEO2TA+2._JPRB*YDML_PHY_MF%YRPHY3%EOATA(JAE)&
       & *PDAER(JLON,JLEV,JAE)  
      ZEO1SA(JLON,JLEV)=ZEO1SA(JLON,JLEV)+ZZEO2SA+2._JPRB*YDML_PHY_MF%YRPHY3%EOASA(JAE)&
       & *PDAER(JLON,JLEV,JAE)
      ZEOSA(JLON,JLEV)=ZEOSA(JLON,JLEV)+(YDML_PHY_MF%YRPHY3%EODSA(JAE)+YDML_PHY_MF%YRPHY3%EOASA(JAE))&
       & *PDAER(JLON,JLEV,JAE)
     ZEOSADIR(JLON,JLEV)=ZEOSADIR(JLON,JLEV)+ &
       & (YDML_PHY_MF%YRPHY3%EODSA(JAE)*ZUNSCALE+YDML_PHY_MF%YRPHY3%EOASA(JAE))*PDAER(JLON,JLEV,JAE)
    
  ENDDO
ENDDO

!     II.2 - CALCULS SOLAIRES ET LUNAIRES.
!     LORSQUE LE SOLEIL EST LEVE, ZMU0 ET ZII0 SONT CEUX DU SOLEIL.
!     SI SEULE LA LUNE EST LEVEE, ILS RECOIVENT CEUX DE LA LUNE.

!     II.2 - SOLAR AND LUNAR COMPUTATIONS.
!     IF SUN IS UP, ZMU0 AND ZII0 ARE RELATIVE TO THE SUN.
!     IF MOON IS UP AND SUN IS DOWN, ZMU0 AND ZII0 ARE RELATIVE TO THE MOON.
!-------------------------------------------------------------------------------

! detemine mu_0 and its min/max values within intermittency window
IF ( ICALS == 1 ) THEN
  
    PGMU0_MIN(JLON)=1._JPRB
    PGMU0_MAX(JLON)=0._JPRB
    ZCOLON=COS(PGELAM(JLON))
    ZSILON=SIN(PGELAM(JLON))
    ZCOLAT=SQRT(1.0_JPRB-PGEMU(JLON)**2)
  
  ZCOVSR=YDRIP%RCOVSR
  ZSIVSR=YDRIP%RSIVSR
  ZCODT =COS(2._JPRB*RPI*YDRIP%TSTEP/RDAY)
  ZSIDT =SIN(2._JPRB*RPI*YDRIP%TSTEP/RDAY)
  DO JSTEP=0,MIN(YDML_PHY_MF%YRPHY%NSORAYFR-1,YDRIP%NSTOP-KSTEP)
    
      ! store mu0 in global storage, so that it is not affected by change
      ! in solar declination in the following model steps
      PGMU0(JLON,JSTEP)=MAX( YDRIP%RSIDEC*PGEMU(JLON)   &
       & -YDRIP%RCODEC*ZCOVSR*ZCOLAT*ZCOLON &
       & +YDRIP%RCODEC*ZSIVSR*ZCOLAT*ZSILON &
       & ,0.0_JPRB)
      PGMU0_MIN(JLON)=MIN(PGMU0_MIN(JLON),PGMU0(JLON,JSTEP))
      PGMU0_MAX(JLON)=MAX(PGMU0_MAX(JLON),PGMU0(JLON,JSTEP))
    
    ZCOVSR_NEW=ZCOVSR*ZCODT-ZSIVSR*ZSIDT
    ZSIVSR    =ZSIVSR*ZCODT+ZCOVSR*ZSIDT
    ZCOVSR    =ZCOVSR_NEW
  ENDDO
ENDIF

IF (YDML_PHY_MF%YRPHY%LRAYLU) THEN

  ! get current solar/lunar mu0 and intensity (lunar when sun is down)
  
    ZSOLLEV=0.5_JPRB*(1._JPRB-SIGN(1._JPRB,0._JPRB-PMU0(JLON)))
    ZMU0=ZSOLLEV*PMU0(JLON)+(1._JPRB-ZSOLLEV)*PMU0LU(JLON)
    ZII0=ZSOLLEV*YDML_PHY_MF%YRPHY3%RII0+(1._JPRB-ZSOLLEV)*YDRIP%RIP0LU
  

ELSEIF ( ICALS == 0 ) THEN

  ! get current solar mu0 and intensity
  
    ZMU0=PMU0(JLON)
    ZII0=YDML_PHY_MF%YRPHY3%RII0
  

ELSE

  ! get precalculated mu0 from global storage and current intensity
  ISTEP=MOD(KSTEP,YDML_PHY_MF%YRPHY%NSORAYFR)
  
    ZMU0=PGMU0(JLON,ISTEP)
    ZII0=YDML_PHY_MF%YRPHY3%RII0
  

ENDIF

!     II.3 - CORRECTION TO ROTUNDITY OF EARTH
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! ZDM0I     : DEMI DE L'INVERSE DU COSINUS MODIFIE DE L'ANGLE ZENITHAL.
!           : HALF INVERSE OF THE MODIFIED COSINE OF THE ZENITH ANGLE.

! compute 1/(2.mu_0')
ZEARRT=YDML_PHY_MF%YRPHY3%EARRT*(YDML_PHY_MF%YRPHY3%EARRT+2._JPRB)
ZIEART=1._JPRB/YDML_PHY_MF%YRPHY3%EARRT

  ZDM0I(JLON)=0.5_JPRB*(SQRT(ZMU0*ZMU0+ &
   & ZEARRT)-ZMU0)*ZIEART


!     II.4 - CALCUL DES LIMITES JOUR/NUIT.

!     II.4 - COMPUTATION OF DAY/NIGHT LIMITS.
!-------------------------------------------------------------------------------

! compute day/night limits
IF (.NOT.YDML_PHY_MF%YRPHY%LRAYPL.OR.(YDML_PHY_MF%YRPHY%NPHYREP /= 0 .AND. YDML_PHY_MF%YRPHY%NPHYREP /= -4)) THEN
  ! daand: in my config, NPHYREP=1, so this case is always selected!
	
  ! PAS DE CALCUL DE "PLAGES" SOLAIRES.
  ! NO "DAYLIGHT" INTERVALS COMPUTATION.
  
	  LLMASKS(JLON)=.TRUE.
	

ELSE

  IF ( ICALS == 0 ) THEN
    
      ZMU00=ZMU0
    
  ELSE
    
      ZMU00=PGMU0_MAX(JLON)
    
  ENDIF
	
	
	  LLMASKS(JLON) = ZMU00 > 0._JPRB
	

ENDIF

!     III - CALCUL DES EPAISSEURS OPTIQUES GASEUSES. LES CALCULS
!     DESCENDANTS SONT SYMETRIQUES ENTRE SOLAIRE ET THERMIQUE (AU TERME
!     D'ALLONGEMENT PRES) MAIS LES CALCULS MONTANTS (DIFFUS TOUS LES
!     DEUX) SONT POUR UN FLUX SOLAIRE REFLECHI ET UN FLUX THERMIQUE
!     CORRESPONDANT A L'ECHANGE AVEC LA SURFACE.

!     III - COMPUTATION OF GASEOUS OPTICAL DEPTHS. THE DESCENDING
!     CALCULATIONS ARE SYMETRICAL BETWEEN SOLAR AND THERMAL (EXCEPT FOR
!     THE DIFFUSIVITY FACTOR) BUT THE ASCENDING CALCULATIONS (DIFFUSE IN
!     BOTH CASES) ARE FOR A REFLECTED SOLAR FLUX AND FOR A THERMAL FLUX
!     CORRESPONDING TO THE EXCHANGE WITH THE SURFACE.
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 2D (1:KLEV)

! ZDEOSI    : EPAISSEUR OPTIQUE "GAZ" DESCENDANTE SOLAIRE.
!           : GASEOUS OPTICAL DEPTH SOLAR DESCENDING.
! ZUEOSI    : EPAISSEUR OPTIQUE "GAZ" MONTANTE SOLAIRE.
!           : GASEOUS OPTICAL DEPTH SOLAR ASCENDING.

!     III.1 - SOLAR GASEOUS OPTICAL DEPTHS
!-------------------------------------------------------------------------------

! - TEMPORAIRE(S) 1D

! IIDIA     : TABLEAU DES INDICES DE DEBUTS DE "PLAGES" SOLAIRES.
!           : ARRAY OF INDICES OF BEGINNING OF "DAYLIGHT" INTERVALS.
! IFDIA     : TABLEAU DES INDICES DE FINS DE "PLAGES" SOLAIRES.
!           : ARRAY OF INDICES OF END OF "DAYLIGHT" INTERVALS.

! determine min/max 1/(2.mu_0')
IF ( ICALS > 0 ) THEN
	
		!IF ( LLMASKS(JLON) ) THEN
      ZDM0I_MIN(JLON)=0.5_JPRB*(SQRT(PGMU0_MAX(JLON)*PGMU0_MAX(JLON)+ &
       & ZEARRT)-PGMU0_MAX(JLON))*ZIEART-ZEPS1
      ZDM0I_MAX(JLON)=0.5_JPRB*(SQRT(PGMU0_MIN(JLON)*PGMU0_MIN(JLON)+ &
       & ZEARRT)-PGMU0_MIN(JLON))*ZIEART+ZEPS1
    !ENDIF
  
ENDIF

! compute optical depths
IF ( ICALS == 0 ) THEN

  ! compute current optical depths
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I,&
   & ZDEOSI,ZUEOSI, YLSTACK)

ELSEIF ( ICALS == 1 ) THEN

  ! compute log of optical depths for min 1/(2.mu_0') alias max sun elevation
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I_MIN,&
   & ZDEOSI,ZUEOSI, YLSTACK)
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        PGDEOSI(JLON,JLEV,1)=LOG(MAX(ZDEOSI(JLON,JLEV),ZTRLI))
        PGUEOSI(JLON,JLEV,1)=LOG(MAX(ZUEOSI(JLON,JLEV),ZTRLI))
      !ENDIF
    
  ENDDO

  ! compute log of optical depths for max 1/(2.mu_0') alias min sun elevation
  CALL ACRANEB_TRANSS(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,LLMASKS,&
   & PAPRS,PAPRSF,PDELP,PR,PT,PQ,PQCO2,PQO3,ZDM0I_MAX,&
   & ZDEOSI,ZUEOSI, YLSTACK)
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        PGDEOSI(JLON,JLEV,2)=LOG(MAX(ZDEOSI(JLON,JLEV),ZTRLI))
        PGUEOSI(JLON,JLEV,2)=LOG(MAX(ZUEOSI(JLON,JLEV),ZTRLI))
      !ENDIF
    
  ENDDO

ENDIF

! interpolate between min/max optical depths
IF ( ICALS > 0 ) THEN

  ! precompute interpolation weights
	
		!IF ( LLMASKS(JLON) ) THEN
      ZWEIGHT=LOG(ZDM0I    (JLON)/ZDM0I_MIN(JLON))/ &
       &            LOG(ZDM0I_MAX(JLON)/ZDM0I_MIN(JLON))
    !ENDIF
  

  ! interpolation with respect to 1/(2.mu_0') in log-log scale
  DO JLEV=KTDIA-1,KLEV
		
			!IF ( LLMASKS(JLON) ) THEN
        ZDEOSI(JLON,JLEV)=EXP(PGDEOSI(JLON,JLEV,1)+ZWEIGHT* &
         & (PGDEOSI(JLON,JLEV,2)-PGDEOSI(JLON,JLEV,1)))
        ZUEOSI(JLON,JLEV)=EXP(PGUEOSI(JLON,JLEV,1)+ZWEIGHT* &
         & (PGUEOSI(JLON,JLEV,2)-PGUEOSI(JLON,JLEV,1)))
      !ENDIF
    
  ENDDO

ENDIF

!     III.1 - THERMAL GASEOUS OPTICAL DEPTHS
!-------------------------------------------------------------------------------

IF ( ICALT > 0 ) THEN

  CALL ACRANEB_TRANST(YDML_PHY_MF%YRPHY,YDML_PHY_MF%YRPHY3, &
   & KIDIA,KFDIA,KLON,KTDIA,KLEV,&
   & LLAUTO,PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3,&
   & ZDEOTI,ZDEOTI2,ZUEOTI,ZUEOTI2,&
   & ZEOLT,ZEOXT,ZPNER0,ZPNER1,ZRPROX,ZRSURF, YLSTACK)

IF (JLON == 1) PRINT *, __FILE__, ':', __LINE__, ZEOXT(JLON,1)

RETURN
ELSE
ENDIF
END SUBROUTINE ACRANEB2
