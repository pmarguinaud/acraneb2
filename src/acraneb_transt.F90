!OPTIONS XOPT(NOEVAL)
!$acc routine(delta_c) seq
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

USE PARKIND1 ,ONLY : JPIM     ,JPRB
USE YOMHOOK  ,ONLY : LHOOK    ,DR_HOOK
USE YOMCST   ,ONLY : RPI      ,RD       ,RV
USE YOMPHY   ,ONLY : TPHY
USE YOMPHY3  ,ONLY : TPHY3

IMPLICIT NONE

TYPE(TPHY)        ,INTENT(INOUT):: YDPHY
TYPE(TPHY3)       ,INTENT(INOUT):: YDPHY3
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

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI,ZTCORR,ZTT

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

REAL(KIND=JPRB) :: ZT_RHOZ0V(3)

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

! local arrays for contained subroutines
REAL(KIND=JPRB) :: ZDELTA(KLON,3)
REAL(KIND=JPRB) :: ZZA    (KLON,3)
REAL(KIND=JPRB) :: ZAR   (KLON,3)
REAL(KIND=JPRB) :: ZCOEF (KLON,3)
REAL(KIND=JPRB) :: ZZTAU  (KLON,3)

! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLEV1,JLEV2,JLON,ILEV

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST',0,ZHOOK_HANDLE)
!ASSOCIATE(FGTT_OC=>YDPHY3%FGTT_OC, FGTT_OB=>YDPHY3%FGTT_OB, &
! & FGTC_D=>YDPHY3%FGTC_D, FGTT_OA=>YDPHY3%FGTT_OA, FGTC_A=>YDPHY3%FGTC_A, &
! & FGTC_C=>YDPHY3%FGTC_C, FGTC_B=>YDPHY3%FGTC_B, FGTC_P00=>YDPHY3%FGTC_P00, &
! & FGTC_Q=>YDPHY3%FGTC_Q, FGTC_P=>YDPHY3%FGTC_P, FGTT_D=>YDPHY3%FGTT_D, &
! & FGTT_B=>YDPHY3%FGTT_B, FGTT_C=>YDPHY3%FGTT_C, FGTT_A=>YDPHY3%FGTT_A, &
! & RTL=>YDPHY3%RTL, FGTC_DELTA0=>YDPHY3%FGTC_DELTA0, FGTT_P=>YDPHY3%FGTT_P, &
! & FGTT_Q=>YDPHY3%FGTT_Q, FGTC_OC=>YDPHY3%FGTC_OC, FGTC_OB=>YDPHY3%FGTC_OB, &
! & FGTC_OA=>YDPHY3%FGTC_OA, FGTT_DELTA0=>YDPHY3%FGTT_DELTA0, &
! & FGTC_OD=>YDPHY3%FGTC_OD, FGTT_OD=>YDPHY3%FGTT_OD, &
! & FGTT_ALPHA=>YDPHY3%FGTT_ALPHA, FGTT_P00=>YDPHY3%FGTT_P00, &
! & FGTC_ALPHA=>YDPHY3%FGTC_ALPHA, &
! & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)
#define FGTT_OC YDPHY3%FGTT_OC
#define FGTT_OB YDPHY3%FGTT_OB
#define FGTC_D YDPHY3%FGTC_D
#define FGTT_OA YDPHY3%FGTT_OA
#define FGTC_A YDPHY3%FGTC_A
#define FGTC_C YDPHY3%FGTC_C
#define FGTC_B YDPHY3%FGTC_B
#define FGTC_P00 YDPHY3%FGTC_P00
#define FGTC_Q YDPHY3%FGTC_Q
#define FGTC_P YDPHY3%FGTC_P
#define FGTT_D YDPHY3%FGTT_D
#define FGTT_B YDPHY3%FGTT_B
#define FGTT_C YDPHY3%FGTT_C
#define FGTT_A YDPHY3%FGTT_A
#define RTL YDPHY3%RTL
#define FGTC_DELTA0 YDPHY3%FGTC_DELTA0
#define FGTT_P YDPHY3%FGTT_P
#define FGTT_Q YDPHY3%FGTT_Q
#define FGTC_OC YDPHY3%FGTC_OC
#define FGTC_OB YDPHY3%FGTC_OB
#define FGTC_OA YDPHY3%FGTC_OA
#define FGTT_DELTA0 YDPHY3%FGTT_DELTA0
#define FGTC_OD YDPHY3%FGTC_OD
#define FGTT_OD YDPHY3%FGTT_OD
#define FGTT_ALPHA YDPHY3%FGTT_ALPHA
#define FGTT_P00 YDPHY3%FGTT_P00
#define FGTC_ALPHA YDPHY3%FGTC_ALPHA
#define LVOIGT YDPHY%LVOIGT
#define LVFULL YDPHY%LVFULL
#define LRPROX YDPHY%LRPROX

!$acc data &
!$acc& present(YDPHY,YDPHY3,PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3) &
!$acc& present(PDEOTI,PDEOTI2,PUEOTI,PUEOTI2,PEOLT,PEOXT,PPNER0,PPNER1,PRPROX,PRSURF) &
!$acc& create(ZT_RHOZ0V ,ZDELP ,ZEOTO ,ZNSOR ,ZP ,ZDU ,ZC_U ,ZC_PU ,ZC_TU ,ZC_UW ,&
!$acc&   ZC_US ,ZC_US_IRHOV ,ZC_UC ,ZT_U ,ZT_PU ,ZT_TU ,ZT_UW ,ZT_US ,ZT_US_IRHOV ,&
!$acc&   ZT_UC ,ZQ ,ZIRHOV ,ZDEOTA0 ,ZDEOTA1 ,ZDEOTA2 ,ZUEOTA0 ,ZUEOTA1 ,ZUEOTA2 ,&
!$acc&   ZC_FW ,ZC_FS ,ZC_FC ,ZT_FW ,ZT_FS ,ZT_FC ,ZDEL0 ,ZDEL1 ,ZDEL1D ,ZTAU ,ZTAU0A ,&
!$acc&   ZTAU1A ,ZTAU0B ,ZTAU1B ,ZTAU0 ,ZTAU1) &
!$acc& create(ZDELTA,ZZA,ZAR,ZCOEF,ZZTAU)

! -----
! security constants, derived parameters for Voigt effect and
! Malkmus formula
! -----

! security constants
ZARGLI=-250._JPRB
ZTRLI =EXP(ZARGLI)
ZEPSP =1.E-03_JPRB
ZEPSD =1.E-20_JPRB
ZEPSU =ZTRLI

! Voigt coefficients
ZT_RHOZ0V(1)=0.031_JPRB
ZT_RHOZ0V(2)=0.013_JPRB
ZT_RHOZ0V(3)=0.012_JPRB
ZGAMV       =0.36_JPRB
ZEPSV       =0.014_JPRB
ZIBV0       =1._JPRB/0.74_JPRB
ZIZV0       =1._JPRB/0.0027_JPRB
ZVOISIM     =1.21_JPRB

!$acc update device (ZT_RHOZ0V)

! ratio of diffusivity factors sqrt(e) and 2 (the latter is used in
! weak line limit)
ZMD=0.5_JPRB*EXP(0.5_JPRB)

! -----
! preparations
! -----

! safety - truncate specific humidity to interval [0, 1]
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZQ(JLON,JLEV)=MAX(0._JPRB,MIN(1._JPRB,PQ(JLON,JLEV)))
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

! determine pressure for computations at model top
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZDELP(JLON)=MAX(ZEPSP,PAPRS(JLON,KTDIA-1))
  ZP   (JLON)=0.5_JPRB*ZDELP(JLON)
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! computation of doubled ozone quantity above starting layer KTDIA
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZNSOR(JLON)=2._JPRB*MAX(ZEPSP,PAPRS(JLON,0))*PQO3(JLON,0)
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=1,KTDIA-1
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZNSOR(JLON)=ZNSOR(JLON)+2._JPRB*PDELP(JLON,JLEV)*PQO3(JLON,JLEV)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

! compute inverse air density
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZIRHOV(JLON,KTDIA-1)=(PR(JLON,KTDIA)*PT(JLON,KTDIA))/ZP(JLON)
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZIRHOV(JLON,JLEV)=(PR(JLON,JLEV)*PT(JLON,JLEV))/PAPRSF(JLON,JLEV)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

! loop through gases
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JG=1,3

  ! computation of pressure/temperature factors for u_w, u_s
! removed hloop :   DO JLON=KIDIA,KFDIA
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
! removed hloop :   ENDDO
  DO JLEV=KTDIA,KLEV
! removed hloop :     DO JLON=KIDIA,KFDIA
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
! removed hloop :     ENDDO
  ENDDO

ENDDO
enddo
!$acc end parallel loop

! initialize pressure/temperature factors for H2O e-type continuum
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZC_FC(JLON,KTDIA-1)=FGTC_C(1)*EXP(-FGTC_C(2)*PT(JLON,KTDIA))*&
   & ZP(JLON)
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZC_FC(JLON,JLEV)=FGTC_C(1)*EXP(-FGTC_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZT_FC(JLON,KTDIA-1)=FGTT_C(1)*EXP(-FGTT_C(2)*PT(JLON,KTDIA))*&
   & ZP(JLON)
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZT_FC(JLON,JLEV)=FGTT_C(1)*EXP(-FGTT_C(2)*PT(JLON,JLEV))*&
     & PAPRSF(JLON,JLEV)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

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
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZDU(JLON,1)=2._JPRB*ZDELP(JLON)*ZQ   (JLON,KTDIA)
  ZDU(JLON,2)=2._JPRB*ZDELP(JLON)*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,3)=                    ZNSOR(JLON)      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! initialize auxiliary quantities u_w, u_s, u_s_rho and u_c
! daand: added explicit loops here
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JG=1,3
! removed hloop : 	DO JLON=KIDIA,KFDIA
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
! removed hloop : 	ENDDO
ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
	ZC_UC      (JLON)  =ZEPSU
	ZT_UC      (JLON)  =ZEPSU
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! compute total and incremental optical depths
! daand: a bit worried about PT and ZDEOTA0/ZDEOTA1 being passed as scalars here ...
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
CALL DELTA_C(JLON,KTDIA-1,ZP,PT(1,KTDIA),ZDU,&
 & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
 & ZDEOTA0(1,KTDIA-1))
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
CALL DELTA_T(JLON,KTDIA-1,ZP,PT(1,KTDIA),ZDU,&
 & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
 & ZDEOTA1(1,KTDIA-1))
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  PDEOTI(JLON,KTDIA-1)=ZDEOTA1(JLON,KTDIA-1)
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! -----
! descending vertical loop
! -----
! - temporary 1D arrays:
! ZEOTO     : "old" thermal optical depth (for computing "new" one)
! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV

  ! compute unscaled absorber amounts 2.du
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
! removed hloop :   ENDDO

  ! compute total and incremental optical depths
! removed hloop :   DO JLON=KIDIA,KFDIA
  CALL DELTA_C(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
   & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
   & ZDEOTA0(1,JLEV))
! removed hloop :   ENDDO
! removed hloop :   DO JLON=KIDIA,KFDIA
  CALL DELTA_T(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
   & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
   & ZDEOTA1(1,JLEV))
! removed hloop :   ENDDO
! removed hloop : 	DO JLON=KIDIA,KFDIA
    PDEOTI(JLON,JLEV)=MAX(ZDEOTA1(JLON,JLEV)-ZDEOTA1(JLON,JLEV-1),0._JPRB)
! removed hloop :   ENDDO

ENDDO
enddo
!$acc end parallel loop

! -----
! temperature correction for CTS
! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZTCORR=4._JPRB*(PT(JLON,KTDIA)/RTL-1._JPRB)
  ZTAU0A(JLON)=EXP(MAX(-ZDEOTA0(JLON,KTDIA-1),ZARGLI))
  ZTAU1A(JLON)=EXP(MAX(-ZDEOTA1(JLON,KTDIA-1),ZARGLI))
  ZTAU(JLON)=ZTAU0A(JLON)+ZTCORR*(ZTAU1A(JLON)-ZTAU0A(JLON))
  ZDEOTA2(JLON,KTDIA-1)=-LOG(MAX(ZTAU(JLON),ZTRLI))
  PDEOTI2(JLON,KTDIA-1)=ZDEOTA2(JLON,KTDIA-1)
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZTCORR=4._JPRB*(PT(JLON,JLEV)/RTL-1._JPRB)
    ZTAU0B(JLON)=EXP(MAX(-ZDEOTA0(JLON,JLEV),ZARGLI))
    ZTAU1B(JLON)=EXP(MAX(-ZDEOTA1(JLON,JLEV),ZARGLI))
    ZTAU(JLON)=ZTAU(JLON)+ZTAU0B(JLON)-ZTAU0A(JLON)+&
     & ZTCORR*(ZTAU1B(JLON)-ZTAU1A(JLON)-ZTAU0B(JLON)+ZTAU0A(JLON))
    ZDEOTA2(JLON,JLEV)=-LOG(MAX(ZTAU(JLON),ZTRLI))
    PDEOTI2(JLON,JLEV)=MAX(ZDEOTA2(JLON,JLEV)-ZDEOTA2(JLON,JLEV-1),0._JPRB)
    ZTAU0A(JLON)=ZTAU0B(JLON)
    ZTAU1A(JLON)=ZTAU1B(JLON)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

! -----
! surface condition
! -----

! thermal depths computed from surface up to given level
! daand: added explicit loops here
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JG=1,3
! removed hloop : 	DO JLON=KIDIA,KFDIA
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
! removed hloop : 	ENDDO
ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZEOTO      (JLON)  =0._JPRB
	ZC_UC      (JLON)  =ZEPSU
	ZT_UC      (JLON)  =ZEPSU
! removed hloop : ENDDO
enddo
!$acc end parallel loop


! -----
! ascending vertical loop
! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KLEV,KTDIA,-1

  ! compute unscaled absorber amounts 2.du
! removed hloop :   DO JLON=KIDIA,KFDIA
    ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
    ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
     &          (1._JPRB-ZQ(JLON,JLEV))
    ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/(RD+(RV-RD)*ZQ(JLON,JLEV))
! removed hloop :   ENDDO

  ! compute total and incremental optical depths
	! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...
! removed hloop :   DO JLON=KIDIA,KFDIA
  CALL DELTA_C(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
   & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
   & ZUEOTA0(1,JLEV))
! removed hloop :   ENDDO
! removed hloop :   DO JLON=KIDIA,KFDIA
  CALL DELTA_T(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
   & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
   & ZUEOTA1(1,JLEV))
! removed hloop :   ENDDO
! removed hloop : 	DO JLON=KIDIA,KFDIA
    PUEOTI(JLON,JLEV)=MAX(ZUEOTA1(JLON,JLEV)-ZEOTO(JLON),0._JPRB)
    ZEOTO  (JLON)    =ZUEOTA1(JLON,JLEV)
! removed hloop :   ENDDO

ENDDO
enddo
!$acc end parallel loop

! -----
! model top (arbitrarily small pressure value for thermal EBL computations)
! -----

! compute unscaled absorber amounts 2.du
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZDU(JLON,1)=2._JPRB*ZDELP(JLON)*ZQ   (JLON,KTDIA)
  ZDU(JLON,2)=2._JPRB*ZDELP(JLON)*PQCO2(JLON,KTDIA)*(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,3)=                    ZNSOR(JLON)      *(1._JPRB-ZQ(JLON,KTDIA))
  ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,KTDIA)/(RD+(RV-RD)*ZQ(JLON,KTDIA))
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! compute total and incremental optical depths
! daand: a bit worried about PT and ZUEOTA0/ZUEOTA1 being passed as scalars here ...
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
CALL DELTA_C(JLON,KTDIA-1,ZP,PT(1,KTDIA),ZDU,&
 & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
 & ZUEOTA0(1,KTDIA-1))
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
CALL DELTA_T(JLON,KTDIA-1,ZP,PT(1,KTDIA),ZDU,&
 & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
 & ZUEOTA1(1,KTDIA-1))
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  PUEOTI(JLON,KTDIA-1)=&
   & MAX(ZUEOTA1(JLON,KTDIA-1)-ZUEOTA1(JLON,KTDIA),0._JPRB)
! removed hloop : ENDDO
enddo
!$acc end parallel loop

! -----
! temperature correction for EWS
! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZTAU0A (JLON)       =1._JPRB
  ZTAU1A (JLON)       =1._JPRB
  ZTAU   (JLON)       =1._JPRB
  ZUEOTA2(JLON,KLEV+1)=0._JPRB
! removed hloop : ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KLEV,KTDIA-1,-1
  ILEV=MAX(KTDIA,JLEV)
! removed hloop :   DO JLON=KIDIA,KFDIA
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
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

IF ( .NOT.LDAUTO ) THEN

  ! -----
  ! local transmissions and optical depths for single and double layers
  ! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV1=KTDIA-1,KLEV-1  ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
! removed hloop : 			DO JLON=KIDIA,KFDIA
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
! removed hloop : 			ENDDO
		ENDDO
! removed hloop : 		DO JLON=KIDIA,KFDIA
			ZC_UC      (JLON)  =ZEPSU
			ZT_UC      (JLON)  =ZEPSU
! removed hloop : 		ENDDO


    IF ( LRPROX ) THEN
      ILEV=MIN(JLEV1+2,KLEV)
    ELSE
      ILEV=JLEV1+1
    ENDIF

    DO JLEV2=JLEV1+1,ILEV  ! final half level

      ! compute unscaled absorber amounts 2.du
! removed hloop :       DO JLON=KIDIA,KFDIA
        ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
! removed hloop :       ENDDO

      ! compute optical depths
! removed hloop :       DO JLON=KIDIA,KFDIA
      CALL DELTA_C(JLON,JLEV2,PAPRSF(1,JLEV2),PT(1,JLEV2),ZDU,&
       & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
       & ZDEL0)
! removed hloop :       ENDDO
! removed hloop :       DO JLON=KIDIA,KFDIA
      CALL DELTA_T(JLON,JLEV2,PAPRSF(1,JLEV2),PT(1,JLEV2),ZDU,&
       & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
       & ZDEL1)
! removed hloop :       ENDDO
      ! compute transmissions
      IF ( LRPROX ) THEN
! removed hloop :         DO JLON=KIDIA,KFDIA
          ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
          ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
! removed hloop :         ENDDO
      ENDIF

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
! removed hloop :         DO JLON=KIDIA,KFDIA
          PEOLT(JLON,JLEV1+1)=ZDEL1(JLON)   ! single layer JLEV1+1
! removed hloop :         ENDDO
      ELSEIF ( JLEV2 == JLEV1+2 ) THEN      ! only when LRPROX
! removed hloop :         DO JLON=KIDIA,KFDIA
          ZDEL1D(JLON,JLEV1+1)=ZDEL1(JLON)  ! double layer JLEV1+1,JLEV1+2
! removed hloop :         ENDDO
      ENDIF

    ENDDO
  ENDDO
enddo
!$acc end parallel loop

  ! -----
  ! top to surface gaseous transmissions
  ! -----
 
	! daand: added explicit loops here
!$acc parallel loop gang vector
do jlon=kidia,kfdia

	DO JG=1,3
! removed hloop : 		DO JLON=KIDIA,KFDIA
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
! removed hloop : 		ENDDO
	ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : 	DO JLON=KIDIA,KFDIA
		ZC_UC      (JLON)  =ZEPSU
		ZT_UC      (JLON)  =ZEPSU
! removed hloop : 	ENDDO
enddo
!$acc end parallel loop

!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV=KTDIA,KLEV

    ! compute unscaled absorber amounts 2.du
! removed hloop :     DO JLON=KIDIA,KFDIA
      ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV)*ZQ   (JLON,JLEV)
      ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV)*PQCO2(JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV)*PQO3 (JLON,JLEV)*&
       &          (1._JPRB-ZQ(JLON,JLEV))
      ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV)/&
       &          (RD+(RV-RD)*ZQ(JLON,JLEV))
! removed hloop :     ENDDO

    ! compute optical depths
! removed hloop :     DO JLON=KIDIA,KFDIA
    CALL DELTA_C(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
     & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
     & ZDEL0)
! removed hloop :     ENDDO
! removed hloop :     DO JLON=KIDIA,KFDIA
    CALL DELTA_T(JLON,JLEV,PAPRSF(1,JLEV),PT(1,JLEV),ZDU,&
     & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
     & ZDEL1)
! removed hloop :     ENDDO
  ENDDO
enddo
!$acc end parallel loop

  ! convert top to surface optical depths to transmissions
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop :   DO JLON=KIDIA,KFDIA
    ZTAU0(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
    ZTAU1(JLON,KTDIA-1,KLEV)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
! removed hloop :   ENDDO
enddo
!$acc end parallel loop

ELSE

  ! -----
  ! double vertical loop for the auto-evaluation of EBL flux with
  ! gaseous absorption only
  ! -----

!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV1=KTDIA-1,KLEV    ! initial half level

		! daand: added explicit loops here
		DO JG=1,3
! removed hloop : 			DO JLON=KIDIA,KFDIA
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
! removed hloop : 			ENDDO
		ENDDO
! removed hloop : 		DO JLON=KIDIA,KFDIA
			ZC_UC      (JLON)  =ZEPSU
			ZT_UC      (JLON)  =ZEPSU
! removed hloop : 		ENDDO

! removed hloop :     DO JLON=KIDIA,KFDIA
      ZTAU0(JLON,JLEV1,JLEV1)=1._JPRB
      ZTAU1(JLON,JLEV1,JLEV1)=1._JPRB
! removed hloop :     ENDDO

    DO JLEV2=JLEV1+1,KLEV  ! final half level

      ! compute unscaled absorber amounts 2.du
! removed hloop :       DO JLON=KIDIA,KFDIA
        ZDU(JLON,1)=2._JPRB*PDELP(JLON,JLEV2)*ZQ   (JLON,JLEV2)
        ZDU(JLON,2)=2._JPRB*PDELP(JLON,JLEV2)*PQCO2(JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,3)=2._JPRB*PDELP(JLON,JLEV2)*PQO3 (JLON,JLEV2)*&
         &          (1._JPRB-ZQ(JLON,JLEV2))
        ZDU(JLON,4)=ZDU(JLON,1)*RV*ZQ(JLON,JLEV2)/&
         &          (RD+(RV-RD)*ZQ(JLON,JLEV2))
! removed hloop :       ENDDO

      ! compute optical depths
! removed hloop :       DO JLON=KIDIA,KFDIA
      CALL DELTA_C(JLON,JLEV2,PAPRSF(1,JLEV2),PT(1,JLEV2),ZDU,&
       & ZC_UW,ZC_US,ZC_US_IRHOV,ZC_UC,ZC_U,ZC_PU,ZC_TU,&
       & ZDEL0)
! removed hloop :       ENDDO
! removed hloop :       DO JLON=KIDIA,KFDIA
      CALL DELTA_T(JLON,JLEV2,PAPRSF(1,JLEV2),PT(1,JLEV2),ZDU,&
       & ZT_UW,ZT_US,ZT_US_IRHOV,ZT_UC,ZT_U,ZT_PU,ZT_TU,&
       & ZDEL1)
! removed hloop :       ENDDO
      ! compute transmissions
! removed hloop :       DO JLON=KIDIA,KFDIA
        ZTAU0(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL0(JLON),ZARGLI))
        ZTAU1(JLON,JLEV1,JLEV2)=EXP(MAX(-ZDEL1(JLON),ZARGLI))
! removed hloop :       ENDDO

      ! store local optical depths of single and double layers
      IF ( JLEV2 == JLEV1+1 ) THEN
! removed hloop :         DO JLON=KIDIA,KFDIA
          PEOLT(JLON,JLEV1+1)=ZDEL1(JLON)   ! single layer JLEV1+1
! removed hloop :         ENDDO
      ELSEIF ( LRPROX.AND.(JLEV2 == JLEV1+2) ) THEN
! removed hloop :         DO JLON=KIDIA,KFDIA
          ZDEL1D(JLON,JLEV1+1)=ZDEL1(JLON)  ! double layer JLEV1+1,JLEV1+2
! removed hloop :         ENDDO
      ENDIF

    ENDDO
  ENDDO
enddo
!$acc end parallel loop

  ! compute quantities needed for T_e corrected EBL, resp. EBL-EAL flux
!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV1=KTDIA,KLEV      ! exchanging layer 1
    IF ( LRPROX ) THEN
      ILEV=JLEV1+2         ! exclude exchange between adjacent layers
    ELSE
      ILEV=JLEV1+1         ! include exchange between adjacent layers
    ENDIF
    DO JLEV2=ILEV,KLEV     ! exchanging layer 2
! removed hloop :       DO JLON=KIDIA,KFDIA
        PPNER0(JLON,JLEV1,JLEV2)=&
         & ZTAU0(JLON,JLEV1,JLEV2  )-ZTAU0(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU0(JLON,JLEV1,JLEV2-1)+ZTAU0(JLON,JLEV1-1,JLEV2-1)
        PPNER1(JLON,JLEV1,JLEV2)=&
         & ZTAU1(JLON,JLEV1,JLEV2  )-ZTAU1(JLON,JLEV1-1,JLEV2  )-&
         & ZTAU1(JLON,JLEV1,JLEV2-1)+ZTAU1(JLON,JLEV1-1,JLEV2-1)
! removed hloop :       ENDDO
    ENDDO
  ENDDO
enddo
!$acc end parallel loop

ENDIF

! -----
! compute maximum optical depths for EBL, resp. EBL-EAL flux and 
! correction factor for tau12 /= tau1.tau2
! -----

IF ( LRPROX ) THEN

  ! compute maximum optical depths for EBL-EAL
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop :   DO JLON=KIDIA,KFDIA
    PEOXT(JLON,KTDIA  )=ZDEL1D(JLON,KTDIA  )-PEOLT(JLON,KTDIA  )
    PEOXT(JLON,KTDIA+1)=ZDEL1D(JLON,KTDIA+1)-PEOLT(JLON,KTDIA+1)
    PEOXT(JLON,KLEV -1)=ZDEL1D(JLON,KLEV -2)-PEOLT(JLON,KLEV -1)
    PEOXT(JLON,KLEV   )=ZDEL1D(JLON,KLEV -1)-PEOLT(JLON,KLEV   )
! removed hloop :   ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV=KTDIA+2,KLEV-2
! removed hloop :     DO JLON=KIDIA,KFDIA
      PEOXT(JLON,JLEV)=MAX(ZDEL1D(JLON,JLEV-1)-PEOLT(JLON,JLEV),&
       &                   ZDEL1D(JLON,JLEV  )-PEOLT(JLON,JLEV))
! removed hloop :     ENDDO
  ENDDO
enddo
!$acc end parallel loop

  ! compute correction factor for tau12 /= tau1.tau2
  ! daand: added explicit loops here
!$acc parallel loop gang vector
do jlon=kidia,kfdia

	DO JLEV=0,KLEV
! removed hloop : 	  DO JLON=KIDIA,KFDIA
	    PRPROX(JLON,JLEV)=0._JPRB
! removed hloop : 		ENDDO
	ENDDO
enddo
!$acc end parallel loop
!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV=KTDIA,KLEV-1
! removed hloop :     DO JLON=KIDIA,KFDIA
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
! removed hloop :     ENDDO
  ENDDO
enddo
!$acc end parallel loop

ELSE

  ! fill maximum optical depths for EBL with local values
!$acc parallel loop gang vector
do jlon=kidia,kfdia

  DO JLEV=KTDIA,KLEV
! removed hloop :     DO JLON=KIDIA,KFDIA
      PEOXT(JLON,JLEV)=PEOLT(JLON,JLEV)
! removed hloop :     ENDDO
  ENDDO
enddo
!$acc end parallel loop

ENDIF

! -----
! compute corrective ratio for surface CTS contribution
! -----

! add T_e corrected optical depths
! daand: added explicit loops here
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
	PRSURF(JLON)=0._JPRB
! removed hloop : ENDDO
enddo
!$acc end parallel loop

!$acc parallel loop gang vector
do jlon=kidia,kfdia

DO JLEV=KTDIA,KLEV
! removed hloop :   DO JLON=KIDIA,KFDIA
    PRSURF(JLON)=PRSURF(JLON)+PDEOTI2(JLON,JLEV)
! removed hloop :   ENDDO
ENDDO
enddo
!$acc end parallel loop

! compute corrective ratio for KTDIA-1 to KLEV transmission
!$acc parallel loop gang vector
do jlon=kidia,kfdia

! removed hloop : DO JLON=KIDIA,KFDIA
  ZTCORR=4._JPRB*(PTS(JLON)/RTL-1._JPRB)
  PRSURF(JLON)=(ZTAU0(JLON,KTDIA-1,KLEV)+&
   &    ZTCORR*(ZTAU1(JLON,KTDIA-1,KLEV)-ZTAU0(JLON,KTDIA-1,KLEV)))/&
   &    EXP(MAX(-PRSURF(JLON),ZARGLI))
! removed hloop : ENDDO
enddo
!$acc end parallel loop

!END ASSOCIATE

!$acc end data

IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST',1,ZHOOK_HANDLE)

! -----
! private procedures
! -----

!$thor start ignore

CONTAINS

! computation of thermal optical depths (B weights)
SUBROUTINE DELTA_C(JLON,KL,PRES,PT,PDU,PUW,PUS,PUS_IRHOV,PUC,&
 & P_U,P_PU,P_TU,PDELTA)

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN)    :: JLON
INTEGER(KIND=JPIM), INTENT(IN)    :: KL                 ! current level
REAL   (KIND=JPRB), INTENT(IN)    :: PRES     (KLON)    ! pressure
REAL   (KIND=JPRB), INTENT(IN)    :: PT       (KLON)    ! temperature
REAL   (KIND=JPRB), INTENT(IN)    :: PDU      (KLON,4)  ! 2.du
REAL   (KIND=JPRB), INTENT(INOUT) :: PUW      (KLON,3)  ! u_w
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS      (KLON,3)  ! u_s
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS_IRHOV(KLON,3)  ! u_s/rho
REAL   (KIND=JPRB), INTENT(INOUT) :: PUC      (KLON)    ! u_c
REAL   (KIND=JPRB), INTENT(INOUT) :: P_U      (KLON,3)  ! u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_PU     (KLON,3)  ! p_avg.u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_TU     (KLON,3)  ! T_avg.u
REAL   (KIND=JPRB), INTENT(OUT)   :: PDELTA   (KLON)    ! optical depth

INTEGER(KIND=JPIM) :: JG,JO

REAL(KIND=JPRB) :: Z4BU,ZA2B,ZAFVOI,ZVOIEMP,ZVOIGT
REAL(KIND=JPRB) :: ZAUX,ZLOG,ZP_AVG,ZT_AVG

REAL(KIND=JPRB) :: ZHOOK_HANDLE

!IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST:DELTA_C',0,ZHOOK_HANDLE)
!ASSOCIATE(FGTT_OC=>YDPHY3%FGTT_OC, FGTT_OB=>YDPHY3%FGTT_OB, &
! & FGTC_D=>YDPHY3%FGTC_D, FGTT_OA=>YDPHY3%FGTT_OA, FGTC_A=>YDPHY3%FGTC_A, &
! & FGTC_C=>YDPHY3%FGTC_C, FGTC_B=>YDPHY3%FGTC_B, FGTC_P00=>YDPHY3%FGTC_P00, &
! & FGTC_Q=>YDPHY3%FGTC_Q, FGTC_P=>YDPHY3%FGTC_P, FGTT_D=>YDPHY3%FGTT_D, &
! & FGTT_B=>YDPHY3%FGTT_B, FGTT_C=>YDPHY3%FGTT_C, FGTT_A=>YDPHY3%FGTT_A, &
! & RTL=>YDPHY3%RTL, FGTC_DELTA0=>YDPHY3%FGTC_DELTA0, FGTT_P=>YDPHY3%FGTT_P, &
! & FGTT_Q=>YDPHY3%FGTT_Q, FGTC_OC=>YDPHY3%FGTC_OC, FGTC_OB=>YDPHY3%FGTC_OB, &
! & FGTC_OA=>YDPHY3%FGTC_OA, FGTT_DELTA0=>YDPHY3%FGTT_DELTA0, &
! & FGTC_OD=>YDPHY3%FGTC_OD, FGTT_OD=>YDPHY3%FGTT_OD, &
! & FGTT_ALPHA=>YDPHY3%FGTT_ALPHA, FGTT_P00=>YDPHY3%FGTT_P00, &
! & FGTC_ALPHA=>YDPHY3%FGTC_ALPHA, &
! & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)

! compute optical depths including narrowband saturation
DO JG=1,3
  !DO JLON=KIDIA,KFDIA

    ! update quantities u_w, u_s and u_s_rho
    PUW(JLON,JG)=PUW(JLON,JG)+ZC_FW(JLON,KL,JG)*PDU(JLON,JG)
    PUS(JLON,JG)=PUS(JLON,JG)+ZC_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD
    PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
     &                        ZC_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
    Z4BU  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
    ZAFVOI=1._JPRB
    IF (LVOIGT) THEN
      ZVOIGT=ZT_RHOZ0V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
      IF (LVFULL) THEN
        ZVOIEMP=EXP(ZEPSV*LOG(ZIBV0*(Z4BU/ZVOIGT))*LOG(ZIZV0*ZVOIGT))
      ELSE
        ZVOIEMP=ZVOISIM
      ENDIF
      ZAFVOI=1._JPRB+ZVOIGT/(RPI*(Z4BU/ZVOIGT)/(1.5_JPRB*SQRT(Z4BU))+&
       & 4._JPRB/(Z4BU/ZVOIGT)+5._JPRB+ZGAMV*SQRT(Z4BU/ZVOIGT)*ZVOIEMP)
    ENDIF
    ZDELTA(JLON,JG)=ZA2B*(SQRT(1._JPRB+ZAFVOI*Z4BU)-1._JPRB)

  !ENDDO
ENDDO

! add H2O e-type continuum
!DO JLON=KIDIA,KFDIA
  PUC(JLON)=PUC(JLON)+ZC_FC(JLON,KL)*PDU(JLON,4)*ZMD
  ZDELTA(JLON,1)=ZDELTA(JLON,1)+PUC(JLON)/&
   & (1._JPRB+FGTC_C(3)*PUC(JLON))+FGTC_C(4)*PUC(JLON)**FGTC_C(5)
!ENDDO

! compute broadband saturation
DO JG=1,3
  !DO JLON=KIDIA,KFDIA

    ! rescaling in order to account for broadband saturation
    ZDELTA(JLON,JG)=(FGTC_DELTA0(JG)/FGTC_ALPHA(JG))*((1._JPRB+&
     & ZDELTA(JLON,JG)/FGTC_DELTA0(JG))**FGTC_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    P_U (JLON,JG)=P_U(JLON,JG)+            PDU(JLON,JG)
    P_PU(JLON,JG)=P_PU(JLON,JG)+PRES(JLON)*PDU(JLON,JG)
    P_TU(JLON,JG)=P_TU(JLON,JG)+PT  (JLON)*PDU(JLON,JG)
    ZP_AVG=P_PU(JLON,JG)/P_U(JLON,JG)
    ZT_AVG=P_TU(JLON,JG)/P_U(JLON,JG)

    ! corrective secondary fit
    ZAUX=ZDELTA(JLON,JG)/(ZDELTA(JLON,JG)+FGTC_D(JG))
    ZLOG=LOG(MAX(ZDELTA(JLON,JG),ZTRLI))
    ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,&
     & FGTC_P00(JG,0)+ZT_AVG*(FGTC_P00(JG,1)+ZT_AVG*FGTC_P00(JG,2))+&
     & ZAUX*(FGTC_P(JG,0,0)+ZT_AVG*(FGTC_P(JG,0,1)+ZT_AVG*FGTC_P(JG,0,2))+&
     & ZLOG*(FGTC_P(JG,1,0)+ZT_AVG*(FGTC_P(JG,1,1)+ZT_AVG*FGTC_P(JG,1,2))+&
     & ZLOG*(FGTC_P(JG,2,0)+ZT_AVG*(FGTC_P(JG,2,1)+ZT_AVG*FGTC_P(JG,2,2))+&
     & ZLOG*(FGTC_P(JG,3,0)+ZT_AVG*(FGTC_P(JG,3,1)+ZT_AVG*FGTC_P(JG,3,2))+&
     & ZLOG*(FGTC_P(JG,4,0)+ZT_AVG*(FGTC_P(JG,4,1)+ZT_AVG*FGTC_P(JG,4,2))+&
     & ZLOG*(FGTC_P(JG,5,0)+ZT_AVG*(FGTC_P(JG,5,1)+ZT_AVG*FGTC_P(JG,5,2))&
     & )))))))
    ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,1._JPRB+&
     & (FGTC_Q(JG,0)+ZP_AVG*(FGTC_Q(JG,1)+ZP_AVG*FGTC_Q(JG,2)))/&
     & (1._JPRB+ZDELTA(JLON,JG)))

  !ENDDO
ENDDO

! compute gaseous overlaps

! individual absorptivities
!DO JLON=KIDIA,KFDIA
  ZZA(JLON,1)=1._JPRB-EXP(-ZDELTA(JLON,1))
  ZZA(JLON,2)=1._JPRB-EXP(-ZDELTA(JLON,2))
  ZZA(JLON,3)=1._JPRB-EXP(-ZDELTA(JLON,3))
!ENDDO

! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits
!DO JLON=KIDIA,KFDIA
  ZAR(JLON,1)=ZZA(JLON,1)+ZZA(JLON,2)-ZZA(JLON,1)*ZZA(JLON,2)
  ZAR(JLON,2)=ZZA(JLON,1)+ZZA(JLON,3)-ZZA(JLON,1)*ZZA(JLON,3)
  ZAR(JLON,3)=ZZA(JLON,2)+ZZA(JLON,3)-ZZA(JLON,2)*ZZA(JLON,3)
  ZCOEF(JLON,1)=2._JPRB*ZZA(JLON,1)*ZZA(JLON,2)/&
   & (ZEPSD+ZZA(JLON,1)*ZZA(JLON,1)+ZZA(JLON,2)*ZZA(JLON,2))
  ZCOEF(JLON,2)=2._JPRB*ZZA(JLON,1)*ZZA(JLON,3)/&
   & (ZEPSD+ZZA(JLON,1)*ZZA(JLON,1)+ZZA(JLON,3)*ZZA(JLON,3))
  ZCOEF(JLON,3)=2._JPRB*ZZA(JLON,2)*ZZA(JLON,3)/&
   & (ZEPSD+ZZA(JLON,2)*ZZA(JLON,2)+ZZA(JLON,3)*ZZA(JLON,3))
!ENDDO

! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO=1,3
  !DO JLON=KIDIA,KFDIA
    ZZTAU(JLON,JO)=1._JPRB-ZAR(JLON,JO)
  !ENDDO
  IF ( FGTC_OA(JO) /= 0._JPRB ) THEN
    !DO JLON=KIDIA,KFDIA
      ZZTAU(JLON,JO)=ZZTAU(JLON,JO)-ZCOEF(JLON,JO)*FGTC_OA(JO)*&
       & (1._JPRB-ZAR(JLON,JO))**FGTC_OB(JO)*ZAR(JLON,JO)**FGTC_OC(JO)*&
       & (1._JPRB-FGTC_OD(JO)*ZAR(JLON,JO))
    !ENDDO
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)
!DO JLON=KIDIA,KFDIA
  PDELTA(JLON)=-LOG(MAX(ZTRLI,ZZTAU(JLON,1)*ZZTAU(JLON,2)*ZZTAU(JLON,3)))-&
   & (ZDELTA(JLON,1)+ZDELTA(JLON,2)+ZDELTA(JLON,3))
!ENDDO
!DO JLON=KIDIA,KFDIA
  PDELTA(JLON)=MAX(PDELTA(JLON),&
   & ZDELTA(JLON,1),ZDELTA(JLON,2),ZDELTA(JLON,3))
!ENDDO

!END ASSOCIATE
!IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST:DELTA_C',1,ZHOOK_HANDLE)

END SUBROUTINE DELTA_C

! -----

! computation of thermal optical depths (dB/dT weights)
SUBROUTINE DELTA_T(JLON,KL,PRES,PT,PDU,PUW,PUS,PUS_IRHOV,PUC,&
 & P_U,P_PU,P_TU,PDELTA)

IMPLICIT NONE

INTEGER(KIND=JPIM), INTENT(IN)    :: JLON
INTEGER(KIND=JPIM), INTENT(IN)    :: KL                 ! current level
REAL   (KIND=JPRB), INTENT(IN)    :: PRES     (KLON)    ! pressure
REAL   (KIND=JPRB), INTENT(IN)    :: PT       (KLON)    ! temperature
REAL   (KIND=JPRB), INTENT(IN)    :: PDU      (KLON,4)  ! 2.du
REAL   (KIND=JPRB), INTENT(INOUT) :: PUW      (KLON,3)  ! u_w
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS      (KLON,3)  ! u_s
REAL   (KIND=JPRB), INTENT(INOUT) :: PUS_IRHOV(KLON,3)  ! u_s/rho
REAL   (KIND=JPRB), INTENT(INOUT) :: PUC      (KLON)    ! u_c
REAL   (KIND=JPRB), INTENT(INOUT) :: P_U      (KLON,3)  ! u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_PU     (KLON,3)  ! p_avg.u
REAL   (KIND=JPRB), INTENT(INOUT) :: P_TU     (KLON,3)  ! T_avg.u
REAL   (KIND=JPRB), INTENT(OUT)   :: PDELTA   (KLON)    ! optical depth

INTEGER(KIND=JPIM) :: JG,JO

REAL(KIND=JPRB) :: Z4BU,ZA2B,ZAFVOI,ZVOIEMP,ZVOIGT
REAL(KIND=JPRB) :: ZAUX,ZLOG,ZP_AVG,ZT_AVG

REAL(KIND=JPRB) :: ZHOOK_HANDLE

!IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST:DELTA_T',0,ZHOOK_HANDLE)
!ASSOCIATE(FGTT_OC=>YDPHY3%FGTT_OC, FGTT_OB=>YDPHY3%FGTT_OB, &
! & FGTC_D=>YDPHY3%FGTC_D, FGTT_OA=>YDPHY3%FGTT_OA, FGTC_A=>YDPHY3%FGTC_A, &
! & FGTC_C=>YDPHY3%FGTC_C, FGTC_B=>YDPHY3%FGTC_B, FGTC_P00=>YDPHY3%FGTC_P00, &
! & FGTC_Q=>YDPHY3%FGTC_Q, FGTC_P=>YDPHY3%FGTC_P, FGTT_D=>YDPHY3%FGTT_D, &
! & FGTT_B=>YDPHY3%FGTT_B, FGTT_C=>YDPHY3%FGTT_C, FGTT_A=>YDPHY3%FGTT_A, &
! & RTL=>YDPHY3%RTL, FGTC_DELTA0=>YDPHY3%FGTC_DELTA0, FGTT_P=>YDPHY3%FGTT_P, &
! & FGTT_Q=>YDPHY3%FGTT_Q, FGTC_OC=>YDPHY3%FGTC_OC, FGTC_OB=>YDPHY3%FGTC_OB, &
! & FGTC_OA=>YDPHY3%FGTC_OA, FGTT_DELTA0=>YDPHY3%FGTT_DELTA0, &
! & FGTC_OD=>YDPHY3%FGTC_OD, FGTT_OD=>YDPHY3%FGTT_OD, &
! & FGTT_ALPHA=>YDPHY3%FGTT_ALPHA, FGTT_P00=>YDPHY3%FGTT_P00, &
! & FGTC_ALPHA=>YDPHY3%FGTC_ALPHA, &
! & LVOIGT=>YDPHY%LVOIGT, LVFULL=>YDPHY%LVFULL, LRPROX=>YDPHY%LRPROX)

! compute optical depths including narrowband saturation
DO JG=1,3
  !DO JLON=KIDIA,KFDIA

    ! update quantities u_w, u_s and u_s_rho
    PUW(JLON,JG)=PUW(JLON,JG)+ZT_FW(JLON,KL,JG)*PDU(JLON,JG)
    PUS(JLON,JG)=PUS(JLON,JG)+ZT_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD
    PUS_IRHOV(JLON,JG)=PUS_IRHOV(JLON,JG)+ZIRHOV(JLON,KL)*&
     &                        ZT_FS(JLON,KL,JG)*PDU(JLON,JG)*ZMD

    ! Malkmus-Voigt formula with Curtis-Godson approximation and
    ! rescaled absorber amount
    ZA2B  =PUS(JLON,JG)/(2._JPRB*PUW(JLON,JG))
    Z4BU  =4._JPRB*PUW(JLON,JG)*PUW(JLON,JG)/PUS(JLON,JG)
    ZAFVOI=1._JPRB
    IF (LVOIGT) THEN
      ZVOIGT=ZT_RHOZ0V(JG)*PUS_IRHOV(JLON,JG)/PUS(JLON,JG)
      IF (LVFULL) THEN
        ZVOIEMP=EXP(ZEPSV*LOG(ZIBV0*(Z4BU/ZVOIGT))*LOG(ZIZV0*ZVOIGT))
      ELSE
        ZVOIEMP=ZVOISIM
      ENDIF
      ZAFVOI=1._JPRB+ZVOIGT/(RPI*(Z4BU/ZVOIGT)/(1.5_JPRB*SQRT(Z4BU))+&
       & 4._JPRB/(Z4BU/ZVOIGT)+5._JPRB+ZGAMV*SQRT(Z4BU/ZVOIGT)*ZVOIEMP)
    ENDIF
    ZDELTA(JLON,JG)=ZA2B*(SQRT(1._JPRB+ZAFVOI*Z4BU)-1._JPRB)

  !ENDDO
ENDDO

! add H2O e-type continuum
!DO JLON=KIDIA,KFDIA
  PUC(JLON)=PUC(JLON)+ZT_FC(JLON,KL)*PDU(JLON,4)*ZMD
  ZDELTA(JLON,1)=ZDELTA(JLON,1)+PUC(JLON)/&
   & (1._JPRB+FGTT_C(3)*PUC(JLON))+FGTT_C(4)*PUC(JLON)**FGTT_C(5)
!ENDDO

! compute broadband saturation
DO JG=1,3
  !DO JLON=KIDIA,KFDIA

    ! rescaling in order to account for broadband saturation
    ZDELTA(JLON,JG)=(FGTT_DELTA0(JG)/FGTT_ALPHA(JG))*((1._JPRB+&
     & ZDELTA(JLON,JG)/FGTT_DELTA0(JG))**FGTT_ALPHA(JG)-1._JPRB)

    ! update u and T_avg.u, compute T_avg
    P_U (JLON,JG)=P_U (JLON,JG)+           PDU(JLON,JG)
    P_PU(JLON,JG)=P_PU(JLON,JG)+PRES(JLON)*PDU(JLON,JG)
    P_TU(JLON,JG)=P_TU(JLON,JG)+PT  (JLON)*PDU(JLON,JG)
    ZP_AVG=P_PU(JLON,JG)/P_U(JLON,JG)
    ZT_AVG=P_TU(JLON,JG)/P_U(JLON,JG)

    ! corrective secondary fit
    ZAUX=ZDELTA(JLON,JG)/(ZDELTA(JLON,JG)+FGTT_D(JG))
    ZLOG=LOG(MAX(ZDELTA(JLON,JG),ZTRLI))
    ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,&
     & FGTT_P00(JG,0)+ZT_AVG*(FGTT_P00(JG,1)+ZT_AVG*FGTT_P00(JG,2))+&
     & ZAUX*(FGTT_P(JG,0,0)+ZT_AVG*(FGTT_P(JG,0,1)+ZT_AVG*FGTT_P(JG,0,2))+&
     & ZLOG*(FGTT_P(JG,1,0)+ZT_AVG*(FGTT_P(JG,1,1)+ZT_AVG*FGTT_P(JG,1,2))+&
     & ZLOG*(FGTT_P(JG,2,0)+ZT_AVG*(FGTT_P(JG,2,1)+ZT_AVG*FGTT_P(JG,2,2))+&
     & ZLOG*(FGTT_P(JG,3,0)+ZT_AVG*(FGTT_P(JG,3,1)+ZT_AVG*FGTT_P(JG,3,2))+&
     & ZLOG*(FGTT_P(JG,4,0)+ZT_AVG*(FGTT_P(JG,4,1)+ZT_AVG*FGTT_P(JG,4,2))+&
     & ZLOG*(FGTT_P(JG,5,0)+ZT_AVG*(FGTT_P(JG,5,1)+ZT_AVG*FGTT_P(JG,5,2))&
     & )))))))
    ZDELTA(JLON,JG)=ZDELTA(JLON,JG)*MAX(0._JPRB,1._JPRB+&
     & (FGTT_Q(JG,0)+ZP_AVG*(FGTT_Q(JG,1)+ZP_AVG*FGTT_Q(JG,2)))/&
     & (1._JPRB+ZDELTA(JLON,JG)))
  !ENDDO
ENDDO

! compute gaseous overlaps

! individual absorptivities
!DO JLON=KIDIA,KFDIA
  ZZA(JLON,1)=1._JPRB-EXP(-ZDELTA(JLON,1))
  ZZA(JLON,2)=1._JPRB-EXP(-ZDELTA(JLON,2))
  ZZA(JLON,3)=1._JPRB-EXP(-ZDELTA(JLON,3))
!ENDDO

! absorptivities for pairs of gases assuming random overlaps and
! modulation factors for the fits
!DO JLON=KIDIA,KFDIA
  ZAR(JLON,1)=ZZA(JLON,1)+ZZA(JLON,2)-ZZA(JLON,1)*ZZA(JLON,2)
  ZAR(JLON,2)=ZZA(JLON,1)+ZZA(JLON,3)-ZZA(JLON,1)*ZZA(JLON,3)
  ZAR(JLON,3)=ZZA(JLON,2)+ZZA(JLON,3)-ZZA(JLON,2)*ZZA(JLON,3)
  ZCOEF(JLON,1)=2._JPRB*ZZA(JLON,1)*ZZA(JLON,2)/&
   & (ZEPSD+ZZA(JLON,1)*ZZA(JLON,1)+ZZA(JLON,2)*ZZA(JLON,2))
  ZCOEF(JLON,2)=2._JPRB*ZZA(JLON,1)*ZZA(JLON,3)/&
   & (ZEPSD+ZZA(JLON,1)*ZZA(JLON,1)+ZZA(JLON,3)*ZZA(JLON,3))
  ZCOEF(JLON,3)=2._JPRB*ZZA(JLON,2)*ZZA(JLON,3)/&
   & (ZEPSD+ZZA(JLON,2)*ZZA(JLON,2)+ZZA(JLON,3)*ZZA(JLON,3))
!ENDDO

! transmissions for pairs of gases containing fitted contribution
! of non-random overlaps
DO JO=1,3
  !DO JLON=KIDIA,KFDIA
    ZZTAU(JLON,JO)=1._JPRB-ZAR(JLON,JO)
  !ENDDO
  IF ( FGTT_OA(JO) /= 0._JPRB ) THEN
    !DO JLON=KIDIA,KFDIA
      ZZTAU(JLON,JO)=ZZTAU(JLON,JO)-ZCOEF(JLON,JO)*FGTT_OA(JO)*&
       & (1._JPRB-ZAR(JLON,JO))**FGTT_OB(JO)*ZAR(JLON,JO)**FGTT_OC(JO)*&
       & (1._JPRB-FGTT_OD(JO)*ZAR(JLON,JO))
    !ENDDO
  ENDIF
ENDDO

! final optical depth (cannot go below maximum of individual optical depths)
!DO JLON=KIDIA,KFDIA
  PDELTA(JLON)=-LOG(MAX(ZTRLI,ZZTAU(JLON,1)*ZZTAU(JLON,2)*ZZTAU(JLON,3)))-&
   & (ZDELTA(JLON,1)+ZDELTA(JLON,2)+ZDELTA(JLON,3))
!ENDDO
!DO JLON=KIDIA,KFDIA
  PDELTA(JLON)=MAX(PDELTA(JLON),&
   & ZDELTA(JLON,1),ZDELTA(JLON,2),ZDELTA(JLON,3))
!ENDDO

!END ASSOCIATE
!IF (LHOOK) CALL DR_HOOK('ACRANEB_TRANST:DELTA_T',1,ZHOOK_HANDLE)

END SUBROUTINE DELTA_T

! -----

!$thor end ignore

END SUBROUTINE ACRANEB_TRANST
