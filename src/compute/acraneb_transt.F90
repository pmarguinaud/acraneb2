!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANST(YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDAUTO,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3,&
! - OUTPUT 2D
 & PDEOTI,PDEOTI2,PUEOTI,PUEOTI2,&
 & PEOLT,PEOXT,PPNER0,PPNER1,PRPROX,PRSURF)

! Purpose:
! --------
!   ACRANEB_TRANST - Computation of thermal gaseous optical depths.

! Interface:
! ----------
! INPUT:
!   KIDIA   - initial index for horizontal loops
!   KFDIA   - final index for horizontal loops
!   KLON    - horizontal dimension of arrays
!   KTDIA   - initial index for vertical loops (usually 1)
!   KLEV    - vertical dimension of full level arrays
!   LDAUTO  - key for computing level to level transmissions needed
!             for autoevaluation of bracketing weights
!   PAPRS   - pressure on half-levels
!   PAPRSF  - pressure on full-levels
!   PDELP   - layer thickness in pressure units
!   PR      - gas constant for air
!   PT      - temperature
!   PTS     - surface temperature
!   PQ      - specific humidity
!   PQCO2   - specific mass of CO2 with respect to dry air
!   PQO3    - specific mass of ozone with respect to dry air
! OUTPUT:
!   PDEOTI  - incremental gaseous optical depth (thermal descending),
!             using dB/dT(T0) based weights
!   PDEOTI2 - incremental gaseous optical depth (thermal descending, CTS),
!             including linear correction in (T_emit - T_local)
!   PUEOTI  - incremental gaseous optical depth (thermal ascending)
!             using dB/dT(T0) based weights
!   PUEOTI2 - incremental gaseous optical depth (thermal ascending),
!             including linear correction in (T_emit - T_local)
!   PEOLT   - local gaseous optical depth, dB/dT(T0) weights
!   PEOXT   - maximum gaseous optical depth for EBL, resp. EBL-EAL flux,
!             dB/dT(T0) weights
!   PPNER0  - transmission term for exact ebl computation, B(T0) weights
!   PPNER1  - transmission term for exact ebl computation, dB/dT(T0) weights
!   PRPROX  - correction term for adjacent exchanges
!   PRSURF  - corrective ratio for surface CTS contribution

! Externals:
! ----------

! Method:
! -------

! Reference:
! ----------

! Author:
! -------
!   1989-12, J.-F. Geleyn (original ACRANEB)

! Modifications:
! --------------
!   2009-10, T. Kral
!   Externalized from ACRANEB.

!   2011-06, R. Brozkova
!   Intermittent call.

!   2013-11, J. Masek
!   New ACRANEB2 gaseous transmissions, modularization. Backphasing to cy38t1.
! End Modifications
!-------------------------------------------------------------------------------

USE PARKIND1 ,ONLY : JPIM     ,JPRB     ,JPRD
USE YOMCST   ,ONLY : RPI      ,RD       ,RV
USE YOMPHY   ,ONLY : TPHY
USE YOMPHY3  ,ONLY : TPHY3

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(IN)   :: YDPHY
TYPE(TPHY3)       ,INTENT(IN)   :: YDPHY3
INTEGER(KIND=JPIM),INTENT(IN) :: KLON
INTEGER(KIND=JPIM),INTENT(IN) :: KLEV
INTEGER(KIND=JPIM),INTENT(IN) :: KIDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KFDIA
INTEGER(KIND=JPIM),INTENT(IN) :: KTDIA

LOGICAL,INTENT(IN) :: LDAUTO

REAL(KIND=JPRB),INTENT(IN)    :: PAPRS(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PAPRSF(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PDELP(KLON,KLEV) 
REAL(KIND=JPRB),INTENT(IN)    :: PR(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PTS(KLON)
REAL(KIND=JPRB),INTENT(IN)    :: PQ(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQCO2(KLON,KLEV)
REAL(KIND=JPRB),INTENT(IN)    :: PQO3(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PDEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PDEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PUEOTI(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PUEOTI2(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PEOLT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PEOXT(KLON,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PPNER0(KLON,KLEV,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PPNER1(KLON,KLEV,KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PRPROX(KLON,0:KLEV)
REAL(KIND=JPRB),INTENT(OUT)   :: PRSURF(KLON)

#include "abor1.intfb.h"
#include "acraneb_transt_delta_c.intfb.h"
#include "acraneb_transt_delta_t.intfb.h"

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSP,ZEPSU,ZTRLI,ZTCORR,ZTT

REAL(KIND=JPRB) :: ZDELP(KLON)
REAL(KIND=JPRB) :: ZEOTO(KLON)
REAL(KIND=JPRB) :: ZNSOR(KLON)
REAL(KIND=JPRB) :: ZP   (KLON)

REAL(KIND=JPRB) :: ZDU        (KLON,4)
REAL(KIND=JPRB) :: ZC_U       (KLON,3)
REAL(KIND=JPRB) :: ZC_PU      (KLON,3)
REAL(KIND=JPRB) :: ZC_TU      (KLON,3)
REAL(KIND=JPRB) :: ZC_UW      (KLON,3)
REAL(KIND=JPRB) :: ZC_US      (KLON,3)
REAL(KIND=JPRB) :: ZC_US_IRHOV(KLON,3)
REAL(KIND=JPRB) :: ZC_UC      (KLON)
REAL(KIND=JPRB) :: ZT_U       (KLON,3)
REAL(KIND=JPRB) :: ZT_PU      (KLON,3)
REAL(KIND=JPRB) :: ZT_TU      (KLON,3)
REAL(KIND=JPRB) :: ZT_UW      (KLON,3)
REAL(KIND=JPRB) :: ZT_US      (KLON,3)
REAL(KIND=JPRB) :: ZT_US_IRHOV(KLON,3)
REAL(KIND=JPRB) :: ZT_UC      (KLON)

REAL(KIND=JPRB) :: ZQ     (KLON,  KLEV)
REAL(KIND=JPRB) :: ZIRHOV (KLON,0:KLEV)
REAL(KIND=JPRB) :: ZDEOTA0(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZDEOTA1(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZDEOTA2(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZUEOTA0(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZUEOTA1(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZUEOTA2(KLON,0:KLEV+1)

REAL(KIND=JPRB) :: ZC_FW(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZC_FS(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZC_FC(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZT_FW(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZT_FS(KLON,0:KLEV,3)
REAL(KIND=JPRB) :: ZT_FC(KLON,0:KLEV)
REAL(KIND=JPRB) :: ZDEL0(KLON)
REAL(KIND=JPRB) :: ZDEL1(KLON)
REAL(KIND=JPRB) :: ZDEL1D(KLON,KLEV-1)
REAL(KIND=JPRB) :: ZTAU  (KLON)
REAL(KIND=JPRB) :: ZTAU0A(KLON)
REAL(KIND=JPRB) :: ZTAU1A(KLON)
REAL(KIND=JPRB) :: ZTAU0B(KLON)
REAL(KIND=JPRB) :: ZTAU1B(KLON)
REAL(KIND=JPRB) :: ZTAU0(KLON,0:KLEV,0:KLEV)
REAL(KIND=JPRB) :: ZTAU1(KLON,0:KLEV,0:KLEV)

! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLEV1,JLEV2,JLON,ILEV

ASSOCIATE(FGTT_OC=>YDPHY3%FGTT_OC, FGTT_OB=>YDPHY3%FGTT_OB, &
 & FGTC_D=>YDPHY3%FGTC_D, FGTT_OA=>YDPHY3%FGTT_OA, FGTC_A=>YDPHY3%FGTC_A, &
 & FGTC_C=>YDPHY3%FGTC_C, FGTC_B=>YDPHY3%FGTC_B, FGTC_P00=>YDPHY3%FGTC_P00, &
 & FGTC_Q=>YDPHY3%FGTC_Q, FGTC_P=>YDPHY3%FGTC_P, FGTT_D=>YDPHY3%FGTT_D, &
 & FGTT_B=>YDPHY3%FGTT_B, FGTT_C=>YDPHY3%FGTT_C, FGTT_A=>YDPHY3%FGTT_A, &
 & RTL=>YDPHY3%RTL, FGTC_DELTA0=>YDPHY3%FGTC_DELTA0, FGTT_P=>YDPHY3%FGTT_P, &
 & FGTT_Q=>YDPHY3%FGTT_Q, FGTC_OC=>YDPHY3%FGTC_OC, FGTC_OB=>YDPHY3%FGTC_OB, &
 & FGTC_OA=>YDPHY3%FGTC_OA, FGTT_DELTA0=>YDPHY3%FGTT_DELTA0, &
 & FGTC_OD=>YDPHY3%FGTC_OD, FGTT_OD=>YDPHY3%FGTT_OD, &
 & FGTT_ALPHA=>YDPHY3%FGTT_ALPHA, FGTT_P00=>YDPHY3%FGTT_P00, &
 & FGTC_ALPHA=>YDPHY3%FGTC_ALPHA, &
 & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)
! -----
! security constants, derived parameters for Voigt effect and
! Malkmus formula
! -----

! security constants
IF (JPRB == JPRD) THEN
  ZARGLI=-250._JPRB
ELSE
  ZARGLI=-80._JPRB
ENDIF
ZTRLI =EXP(ZARGLI)
ZEPSP =1.E-03_JPRB
ZEPSU =ZTRLI

! -----
! preparations
! -----

! safety - truncate specific humidity to interval [0, 1]
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZQ(JLON,JLEV)=MAX(0._JPRB,MIN(1._JPRB,PQ(JLON,JLEV)))
  ENDDO
ENDDO

! determine pressure for computations at model top
DO JLON=KIDIA,KFDIA
  ZDELP(JLON)=MAX(ZEPSP,PAPRS(JLON,KTDIA-1))
  ZP   (JLON)=0.5_JPRB*ZDELP(JLON)
ENDDO

! computation of doubled ozone quantity above starting layer KTDIA
DO JLON=KIDIA,KFDIA
  ZNSOR(JLON)=2._JPRB*MAX(ZEPSP,PAPRS(JLON,0))*PQO3(JLON,0)
ENDDO
DO JLEV=1,KTDIA-1
  DO JLON=KIDIA,KFDIA
    ZNSOR(JLON)=ZNSOR(JLON)+2._JPRB*PDELP(JLON,JLEV)*PQO3(JLON,JLEV)
  ENDDO
ENDDO

! compute inverse air density
DO JLON=KIDIA,KFDIA
  ZIRHOV(JLON,KTDIA-1)=(PR(JLON,KTDIA)*PT(JLON,KTDIA))/ZP(JLON)
ENDDO
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZIRHOV(JLON,JLEV)=(PR(JLON,JLEV)*PT(JLON,JLEV))/PAPRSF(JLON,JLEV)
  ENDDO
ENDDO

! loop through gases
DO JG=1,3

  ! computation of pressure/temperature factors for u_w, u_s
  DO JLON=KIDIA,KFDIA
    ZA=FGTC_A(JG,0)*(1._JPRB+FGTC_A(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+FGTC_A(JG,2)*PT(JLON,KTDIA))
    ZB=FGTC_B(JG,0)*(1._JPRB+FGTC_B(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+FGTC_B(JG,2)*PT(JLON,KTDIA))
    ZC_FW(JLON,KTDIA-1,JG)=ZA
    ZC_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP(JLON)
    ZA=FGTT_A(JG,0)*(1._JPRB+FGTT_A(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+FGTT_A(JG,2)*PT(JLON,KTDIA))
    ZB=FGTT_B(JG,0)*(1._JPRB+FGTT_B(JG,1)*PT(JLON,KTDIA))/&
     &              (1._JPRB+FGTT_B(JG,2)*PT(JLON,KTDIA))
    ZT_FW(JLON,KTDIA-1,JG)=ZA
    ZT_FS(JLON,KTDIA-1,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*ZP(JLON)
  ENDDO
  DO JLEV=KTDIA,KLEV
    DO JLON=KIDIA,KFDIA
      ZA=FGTC_A(JG,0)*(1._JPRB+FGTC_A(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+FGTC_A(JG,2)*PT(JLON,JLEV))
      ZB=FGTC_B(JG,0)*(1._JPRB+FGTC_B(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+FGTC_B(JG,2)*PT(JLON,JLEV))
      ZC_FW(JLON,JLEV,JG)=ZA
      ZC_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
      ZA=FGTT_A(JG,0)*(1._JPRB+FGTT_A(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+FGTT_A(JG,2)*PT(JLON,JLEV))
      ZB=FGTT_B(JG,0)*(1._JPRB+FGTT_B(JG,1)*PT(JLON,JLEV))/&
       &              (1._JPRB+FGTT_B(JG,2)*PT(JLON,JLEV))
      ZT_FW(JLON,JLEV,JG)=ZA
      ZT_FS(JLON,JLEV,JG)=(ZA*ZA/MAX(ZB,ZEPSU))*PAPRSF(JLON,JLEV)
    ENDDO
  ENDDO

ENDDO

! initialize pressure/temperature factors for H2O e-type continuum
DO JLON=KIDIA,KFDIA
  ZC_FC(JLON,KTDIA-1)=FGTC_C(1)*EXP(-FGTC_C(2)*PT(JLON,KTDIA))*&
   & ZP(JLON)
ENDDO
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZC_FC(JLON,JLEV)=FGTC_C(1)*EXP(-FGTC_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
  ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
  ZT_FC(JLON,KTDIA-1)=FGTT_C(1)*EXP(-FGTT_C(2)*PT(JLON,KTDIA))*&
   & ZP(JLON)
ENDDO
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZT_FC(JLON,JLEV)=FGTT_C(1)*EXP(-FGTT_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
  ENDDO
ENDDO

! -----
! computation of gaseous optical depths:
! - descending calculations are symmetrical between solar and thermal band
!   (except for diffusivity factor)
! - ascending calculations (diffuse in both cases) are for a reflected
!   solar flux and for thermal flux corresponding to the exchange with
!   surface
! -----

! -----
! model top
! -----

! compute unscaled absorber amounts 2.du (2 stands for diffusivity factor
! in weak line limit) and inverse air density; absorber amount for H2O
! e-type continuum is multiplied by ratio e/p (water vapor pressure
! to total pressure)
DO JLON=KIDIA,KFDIA
  ZDU(JLON,1)=2._JPRB*ZDELP(JLON)*ZQ   (JLON,KTDIA)
  ZDU(JLON,2)=2._JPRB*ZDELP(JLON)*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,3)=                    ZNSOR(JLON)      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))
ENDDO

! initialize auxiliary quantities u_w, u_s, u_s_rho and u_c
! daand: added explicit loops here
DO JG=1,3
	DO JLON=KIDIA,KFDIA
		ZC_UW      (JLON,JG)=ZEPSU
		ZC_US      (JLON,JG)=ZEPSU
		ZC_US_IRHOV(JLON,JG)=ZEPSU
		ZC_U       (JLON,JG)=ZEPSU
		ZC_PU      (JLON,JG)=ZEPSU
		ZC_TU      (JLON,JG)=ZEPSU
		ZT_UW      (JLON,JG)=ZEPSU
		ZT_US      (JLON,JG)=ZEPSU
		ZT_US_IRHOV(JLON,JG)=ZEPSU
		ZT_U       (JLON,JG)=ZEPSU
		ZT_PU      (JLON,JG)=ZEPSU
		ZT_TU      (JLON,JG)=ZEPSU
	ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
	ZC_UC      (JLON)  =ZEPSU
	ZT_UC      (JLON)  =ZEPSU
ENDDO

! compute total and incremental optical depths
! daand: a bit worried about PT and ZDEOTA0/ZDEOTA1 being passed as scalars here ...
CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLON,KLEV,KTDIA-1,ZP,PT(:,KTDIA),ZDU,&
 & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
 & ZDEOTA0(:,KTDIA-1),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,KTDIA-1,ZP,PT(:,KTDIA),ZDU,&
 & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
 & ZDEOTA1(:,KTDIA-1),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)
DO JLON=KIDIA,KFDIA
  PDEOTI(JLON,KTDIA-1)=ZDEOTA1(JLON,KTDIA-1)
ENDDO

! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOTO     : "old" thermal optical depth (for computing "new" one)
! -----

DO JLEV=KTDIA,KLEV

  ! compute unscaled absorber amounts 2.du
  DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  ENDDO

  ! compute total and incremental optical depths
  CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLON,KLEV,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
   & ZDEOTA0(:,JLEV),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
  CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
   & ZDEOTA1(:,JLEV),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)
  DO JLON=KIDIA,KFDIA
    PDEOTI(JLON,JLEV)=MAX(ZDEOTA1(JLON,JLEV)-ZDEOTA1(JLON,JLEV-1),0._JPRB)
  ENDDO

ENDDO

! -----
! temperature correction for CTS
! -----

DO JLON=KIDIA,KFDIA
  ZTCORR=4._JPRB*(PT(JLON,KTDIA)/RTL-1._JPRB)
  ZTAU0A(JLON)=EXP(MAX(-ZDEOTA0(JLON,KTDIA-1),ZARGLI))
  ZTAU1A(JLON)=EXP(MAX(-ZDEOTA1(JLON,KTDIA-1),ZARGLI))
  ZTAU(JLON)=ZTAU0A(JLON)+ZTCORR*(ZTAU1A(JLON)-ZTAU0A(JLON))
  ZDEOTA2(JLON,KTDIA-1)=-LOG(MAX(ZTAU(JLON),ZTRLI))
  PDEOTI2(JLON,KTDIA-1)=ZDEOTA2(JLON,KTDIA-1)
ENDDO
DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    ZTCORR=4._JPRB*(PT(JLON,JLEV)/RTL-1._JPRB)
    ZTAU0B(JLON)=EXP(MAX(-ZDEOTA0(JLON,JLEV),ZARGLI))
    ZTAU1B(JLON)=EXP(MAX(-ZDEOTA1(JLON,JLEV),ZARGLI))
    ZTAU(JLON)=ZTAU(JLON)+ZTAU0B(JLON)-ZTAU0A(JLON)+&
     & ZTCORR*(ZTAU1B(JLON)-ZTAU1A(JLON)-ZTAU0B(JLON)+ZTAU0A(JLON))
    ZDEOTA2(JLON,JLEV)=-LOG(MAX(ZTAU(JLON),ZTRLI))
    PDEOTI2(JLON,JLEV)=MAX(ZDEOTA2(JLON,JLEV)-ZDEOTA2(JLON,JLEV-1),0._JPRB)
    ZTAU0A(JLON)=ZTAU0B(JLON)
    ZTAU1A(JLON)=ZTAU1B(JLON)
  ENDDO
ENDDO

! -----
! surface condition
! -----

! thermal depths computed from surface up to given level
! daand: added explicit loops here
DO JG=1,3
	DO JLON=KIDIA,KFDIA
		ZC_UW      (JLON,JG)=ZEPSU
		ZC_US      (JLON,JG)=ZEPSU
		ZC_US_IRHOV(JLON,JG)=ZEPSU
		ZC_U       (JLON,JG)=ZEPSU
		ZC_PU      (JLON,JG)=ZEPSU
		ZC_TU      (JLON,JG)=ZEPSU
		ZT_UW      (JLON,JG)=ZEPSU
		ZT_US      (JLON,JG)=ZEPSU
		ZT_US_IRHOV(JLON,JG)=ZEPSU
		ZT_U       (JLON,JG)=ZEPSU
		ZT_PU      (JLON,JG)=ZEPSU
		ZT_TU      (JLON,JG)=ZEPSU
	ENDDO
ENDDO
DO JLON=KIDIA,KFDIA
  ZEOTO      (JLON)  =0._JPRB
	ZC_UC      (JLON)  =ZEPSU
	ZT_UC      (JLON)  =ZEPSU
ENDDO


! -----
! ascending vertical loop
! -----

DO JLEV=KLEV,KTDIA,-1

  ! compute unscaled absorber amounts 2.du
  DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
  ENDDO

  ! compute total and incremental optical depths
	! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...
  CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLEV,KLON,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
   & ZUEOTA0(:,JLEV),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
  CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
   & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
   & ZUEOTA1(:,JLEV),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)
  DO JLON=KIDIA,KFDIA
    PUEOTI(JLON,JLEV)=MAX(ZUEOTA1(JLON,JLEV)-ZEOTO(JLON),0._JPRB)
    ZEOTO  (JLON)    =ZUEOTA1(JLON,JLEV)
  ENDDO

ENDDO

! -----
! model top (arbitrarily small pressure value for thermal EBL computations)
! -----

! compute unscaled absorber amounts 2.du
DO JLON=KIDIA,KFDIA
  ZDU(JLON,1)=2._JPRB*ZDELP(JLON)*ZQ   (JLON,KTDIA)
  ZDU(JLON,2)=2._JPRB*ZDELP(JLON)*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,3)=                    ZNSOR(JLON)      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))
ENDDO

! compute total and incremental optical depths
! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...
CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLEV,KLON,KTDIA-1,ZP,PT(:,KTDIA),ZDU,&
 & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
 & ZUEOTA0(:,KTDIA-1),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLEV,KLON,KTDIA-1,ZP,PT(:,KTDIA),ZDU,&
 & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
 & ZUEOTA1(:,KTDIA-1),YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)
DO JLON=KIDIA,KFDIA
  PUEOTI(JLON,KTDIA-1)=&
   & MAX(ZUEOTA1(JLON,KTDIA-1)-ZUEOTA1(JLON,KTDIA),0._JPRB)
ENDDO

! -----
! temperature correction for EWS
! -----

DO JLON=KIDIA,KFDIA
  ZTAU0A (JLON)       =1._JPRB
  ZTAU1A (JLON)       =1._JPRB
  ZTAU   (JLON)       =1._JPRB
  ZUEOTA2(JLON,KLEV+1)=0._JPRB
ENDDO
DO JLEV=KLEV,KTDIA-1,-1
  ILEV=MAX(KTDIA,JLEV)
  DO JLON=KIDIA,KFDIA
    ZTT=PT(JLON,ILEV)/PTS(JLON)
    ZTCORR=4._JPRB*((PTS(JLON)/RTL)*&
     & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT))))/&
     & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT)))-1._JPRB)
    ZTAU0B(JLON)=EXP(MAX(-ZUEOTA0(JLON,JLEV),ZARGLI))
    ZTAU1B(JLON)=EXP(MAX(-ZUEOTA1(JLON,JLEV),ZARGLI))
    ZTAU(JLON)=ZTAU(JLON)+ZTAU0B(JLON)-ZTAU0A(JLON)+&
     & ZTCORR*(ZTAU1B(JLON)-ZTAU1A(JLON)-ZTAU0B(JLON)+ZTAU0A(JLON))
    ZUEOTA2 (JLON,JLEV)=-LOG(MAX(ZTAU(JLON),ZTRLI))
    PUEOTI2(JLON,JLEV)=MAX(ZUEOTA2(JLON,JLEV)-ZUEOTA2(JLON,JLEV+1),0._JPRB)
    ZTAU0A(JLON)=ZTAU0B(JLON)
    ZTAU1A(JLON)=ZTAU1B(JLON)
  ENDDO
ENDDO

IF ( .NOT.LDAUTO ) THEN

  ! -----
  ! local transmissions and optical depths for single and double layers
  ! -----

  DO JLEV1=KTDIA-1,KLEV-1  ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
			DO JLON=KIDIA,KFDIA
				ZC_UW      (JLON,JG)=ZEPSU
				ZC_US      (JLON,JG)=ZEPSU
				ZC_US_IRHOV(JLON,JG)=ZEPSU
				ZC_U       (JLON,JG)=ZEPSU
				ZC_PU      (JLON,JG)=ZEPSU
				ZC_TU      (JLON,JG)=ZEPSU
				ZT_UW      (JLON,JG)=ZEPSU
				ZT_US      (JLON,JG)=ZEPSU
				ZT_US_IRHOV(JLON,JG)=ZEPSU
				ZT_U       (JLON,JG)=ZEPSU
				ZT_PU      (JLON,JG)=ZEPSU
				ZT_TU      (JLON,JG)=ZEPSU
			ENDDO
		ENDDO
		DO JLON=KIDIA,KFDIA
			ZC_UC      (JLON)  =ZEPSU
			ZT_UC      (JLON)  =ZEPSU
		ENDDO


    IF ( LRPROX ) THEN
      ILEV=MIN(JLEV1+2,KLEV)
    ELSE
      ILEV=JLEV1+1
    ENDIF

    DO JLEV2=JLEV1+1,ILEV  ! final half level

      ! compute unscaled absorber amounts 2.du
      DO JLON=KIDIA,KFDIA
        ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
      ENDDO

      ! compute optical depths
      CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLON,KLEV,JLEV2,PAPRSF(:,JLEV2),PT(:,JLEV2),ZDU,&
       & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
       & ZDEL0,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
      CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,JLEV2,PAPRSF(:,JLEV2),PT(:,JLEV2),ZDU,&
       & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
       & ZDEL1,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)

      ! compute transmissions
      IF ( LRPROX ) THEN
        DO JLON=KIDIA,KFDIA
          ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
          ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
        ENDDO
      ENDIF

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
        DO JLON=KIDIA,KFDIA
          PEOLT(JLON,JLEV1+1)=ZDEL1(JLON)   ! single layer JLEV1+1
        ENDDO
      ELSEIF ( JLEV2 == JLEV1+2 ) THEN      ! only when LRPROX
        DO JLON=KIDIA,KFDIA
          ZDEL1D(JLON,JLEV1+1)=ZDEL1(JLON)  ! double layer JLEV1+1,JLEV1+2
        ENDDO
      ENDIF

    ENDDO
  ENDDO

  ! -----
  ! top to surface gaseous transmissions
  ! -----
 
	! daand: added explicit loops here
	DO JG=1,3
		DO JLON=KIDIA,KFDIA
			ZC_UW      (JLON,JG)=ZEPSU
			ZC_US      (JLON,JG)=ZEPSU
			ZC_US_IRHOV(JLON,JG)=ZEPSU
			ZC_U       (JLON,JG)=ZEPSU
			ZC_PU      (JLON,JG)=ZEPSU
			ZC_TU      (JLON,JG)=ZEPSU
			ZT_UW      (JLON,JG)=ZEPSU
			ZT_US      (JLON,JG)=ZEPSU
			ZT_US_IRHOV(JLON,JG)=ZEPSU
			ZT_U       (JLON,JG)=ZEPSU
			ZT_PU      (JLON,JG)=ZEPSU
			ZT_TU      (JLON,JG)=ZEPSU
		ENDDO
	ENDDO
	DO JLON=KIDIA,KFDIA
		ZC_UC      (JLON)  =ZEPSU
		ZT_UC      (JLON)  =ZEPSU
	ENDDO

  DO JLEV=KTDIA,KLEV

    ! compute unscaled absorber amounts 2.du
    DO JLON=KIDIA,KFDIA
      ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
      ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/&
       &          (RD+(RV-RD)*ZQ(JLON,JLEV))
    ENDDO

    ! compute optical depths
    CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLON,KLEV,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
     & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
     & ZDEL0,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
    CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,JLEV,PAPRSF(:,JLEV),PT(:,JLEV),ZDU,&
     & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
     & ZDEL1,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)

  ENDDO

  ! convert top to surface optical depths to transmissions
  DO JLON=KIDIA,KFDIA
    ZTAU0(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
    ZTAU1(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
  ENDDO

ELSE

  ! -----
  ! double vertical loop for the auto-evaluation of EBL flux with
  ! gaseous absorption only
  ! -----

  DO JLEV1=KTDIA-1,KLEV    ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
			DO JLON=KIDIA,KFDIA
				ZC_UW      (JLON,JG)=ZEPSU
				ZC_US      (JLON,JG)=ZEPSU
				ZC_US_IRHOV(JLON,JG)=ZEPSU
				ZC_U       (JLON,JG)=ZEPSU
				ZC_PU      (JLON,JG)=ZEPSU
				ZC_TU      (JLON,JG)=ZEPSU
				ZT_UW      (JLON,JG)=ZEPSU
				ZT_US      (JLON,JG)=ZEPSU
				ZT_US_IRHOV(JLON,JG)=ZEPSU
				ZT_U       (JLON,JG)=ZEPSU
				ZT_PU      (JLON,JG)=ZEPSU
				ZT_TU      (JLON,JG)=ZEPSU
			ENDDO
		ENDDO
		DO JLON=KIDIA,KFDIA
			ZC_UC      (JLON)  =ZEPSU
			ZT_UC      (JLON)  =ZEPSU
		ENDDO

    DO JLON=KIDIA,KFDIA
      ZTAU0(JLON,JLEV1,JLEV1)=1._JPRB
      ZTAU1(JLON,JLEV1,JLEV1)=1._JPRB
    ENDDO

    DO JLEV2=JLEV1+1,KLEV  ! final half level

      ! compute unscaled absorber amounts 2.du
      DO JLON=KIDIA,KFDIA
        ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
      ENDDO

      ! compute optical depths
      CALL ACRANEB_TRANST_DELTA_C(KIDIA,KFDIA,KLON,KLEV,JLEV2,PAPRSF(:,JLEV2),PT(:,JLEV2),ZDU,&
       & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
       & ZDEL0,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZC_FW,ZC_FS,ZC_FC)
      CALL ACRANEB_TRANST_DELTA_T(KIDIA,KFDIA,KLON,KLEV,JLEV2,PAPRSF(:,JLEV2),PT(:,JLEV2),ZDU,&
       & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
       & ZDEL1,YDPHY,YDPHY3,ZTRLI,ZIRHOV,ZT_FW,ZT_FS,ZT_FC)

      ! compute transmissions
      DO JLON=KIDIA,KFDIA
        ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
        ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
      ENDDO

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
        DO JLON=KIDIA,KFDIA
          PEOLT(JLON,JLEV1+1)=ZDEL1(JLON)   ! single layer JLEV1+1
        ENDDO
      ELSEIF ( LRPROX.AND.(JLEV2 == JLEV1+2) ) THEN
        DO JLON=KIDIA,KFDIA
          ZDEL1D(JLON,JLEV1+1)=ZDEL1(JLON)  ! double layer JLEV1+1,JLEV1+2
        ENDDO
      ENDIF

    ENDDO
  ENDDO

  ! compute quantities needed for T_e corrected EBL, resp. EBL-EAL flux
  DO JLEV1=KTDIA,KLEV      ! exchanging layer 1
    IF ( LRPROX ) THEN
      ILEV=JLEV1+2         ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1         ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV     ! exchanging layer 2
      DO JLON=KIDIA,KFDIA
        PPNER0(JLON,JLEV1,JLEV2)=&
         & ZTAU0(JLON,JLEV1,JLEV2  )-ZTAU0(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU0(JLON,JLEV1,JLEV2-1)+ZTAU0(JLON,JLEV1-1,JLEV2-1)
        PPNER1(JLON,JLEV1,JLEV2)=&
         & ZTAU1(JLON,JLEV1,JLEV2  )-ZTAU1(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU1(JLON,JLEV1,JLEV2-1)+ZTAU1(JLON,JLEV1-1,JLEV2-1)
      ENDDO
    ENDDO
  ENDDO

ENDIF

! -----
! compute maximum optical depths for EBL, resp. EBL-EAL flux and 
! correction factor for tau12 /= tau1.tau2
! -----

IF ( LRPROX ) THEN

  ! compute maximum optical depths for EBL-EAL
  DO JLON=KIDIA,KFDIA
    PEOXT(JLON,KTDIA  )=ZDEL1D(JLON,KTDIA  )-PEOLT(JLON,KTDIA  )
    PEOXT(JLON,KTDIA+1)=ZDEL1D(JLON,KTDIA+1)-PEOLT(JLON,KTDIA+1)
    PEOXT(JLON,KLEV -1)=ZDEL1D(JLON,KLEV -2)-PEOLT(JLON,KLEV -1)
    PEOXT(JLON,KLEV   )=ZDEL1D(JLON,KLEV -1)-PEOLT(JLON,KLEV   )
  ENDDO
  DO JLEV=KTDIA+2,KLEV-2
    DO JLON=KIDIA,KFDIA
      PEOXT(JLON,JLEV)=MAX(ZDEL1D(JLON,JLEV-1)-PEOLT(JLON,JLEV),&
       &                   ZDEL1D(JLON,JLEV  )-PEOLT(JLON,JLEV))
    ENDDO
  ENDDO

  ! compute correction factor for tau12 /= tau1.tau2
  ! daand: added explicit loops here
	DO JLEV=0,KLEV
	  DO JLON=KIDIA,KFDIA
	    PRPROX(JLON,JLEV)=0._JPRB
		ENDDO
	ENDDO
  DO JLEV=KTDIA,KLEV-1
    DO JLON=KIDIA,KFDIA
      ZTT=PT(JLON,JLEV)/PT(JLON,JLEV+1)
      ZTCORR=4._JPRB*((PT(JLON,JLEV+1)/RTL)*&
       & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT))))/&
       & (1._JPRB+ZTT*(1._JPRB+ZTT*(1._JPRB+ZTT)))-1._JPRB)
      PRPROX(JLON,JLEV)=((1._JPRB-ZTCORR)*(ZTAU0(JLON,JLEV-1,JLEV+1)-&
       & ZTAU0(JLON,JLEV-1,JLEV)-ZTAU0(JLON,JLEV,JLEV+1))+&
       & ZTCORR*(ZTAU1(JLON,JLEV-1,JLEV+1)-&
       & ZTAU1(JLON,JLEV-1,JLEV)-ZTAU1(JLON,JLEV,JLEV+1))+&
       & ZTAU1(JLON,JLEV-1,JLEV)+ZTAU1(JLON,JLEV,JLEV+1))/&
       & MAX(ZTAU1(JLON,JLEV-1,JLEV)*ZTAU1(JLON,JLEV,JLEV+1),ZTRLI)
    ENDDO
  ENDDO

ELSE

  ! fill maximum optical depths for EBL with local values
  DO JLEV=KTDIA,KLEV
    DO JLON=KIDIA,KFDIA
      PEOXT(JLON,JLEV)=PEOLT(JLON,JLEV)
    ENDDO
  ENDDO

ENDIF

! -----
! compute corrective ratio for surface CTS contribution
! -----

! add T_e corrected optical depths
! daand: added explicit loops here
DO JLON=KIDIA,KFDIA
	PRSURF(JLON)=0._JPRB
ENDDO

DO JLEV=KTDIA,KLEV
  DO JLON=KIDIA,KFDIA
    PRSURF(JLON)=PRSURF(JLON)+PDEOTI2(JLON,JLEV)
  ENDDO
ENDDO

! compute corrective ratio for KTDIA-1 to KLEV transmission
DO JLON=KIDIA,KFDIA
  ZTCORR=4._JPRB*(PTS(JLON)/RTL-1._JPRB)
  PRSURF(JLON)=(ZTAU0(JLON,KTDIA-1,KLEV)+&
   &    ZTCORR*(ZTAU1(JLON,KTDIA-1,KLEV)-ZTAU0(JLON,KTDIA-1,KLEV)))/&
   &    EXP(MAX(-PRSURF(JLON),ZARGLI))
ENDDO

END ASSOCIATE

END SUBROUTINE ACRANEB_TRANST
