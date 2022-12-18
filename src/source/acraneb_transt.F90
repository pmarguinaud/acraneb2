!OPTIONS XOPT(NOEVAL)
SUBROUTINE ACRANEB_TRANST(YDPHY,YDPHY3,KIDIA,KFDIA,KLON,KTDIA,KLEV,LDAUTO,&
! - INPUT 2D
 & PAPRS,PAPRSF,PDELP,PR,PT,PTS,PQ,PQCO2,PQO3,&
! - OUTPUT 2D
 & PDEOTI,PDEOTI2,PUEOTI,PUEOTI2,&
 & PEOLT,PEOXT,PPNER0,PPNER1,PRPROX,PRSURF, YDSTACK)
!$acc routine (ACRANEB_TRANST) seq

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
USE STACK_MOD
#include "stack.h"

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
TYPE(STACK) :: YDSTACK, YLSTACK

#ifdef UNDEF

#include "abor1.intfb.h"

! -----

REAL(KIND=JPRB) :: ZA,ZB,ZARGLI,ZEPSD,ZEPSP,ZEPSU,ZTRLI,ZTCORR,ZTT

REAL(KIND=JPRB) :: ZEPSV,ZGAMV,ZIBV0,ZIZV0,ZMD,ZVOISIM

TYPE TT_RHOZ0V
REAL(KIND=JPRB) :: V(3)
END TYPE

TYPE (TT_RHOZ0V) :: YT_RHOZ0V

REAL(KIND=JPRB) :: ZDELP
REAL(KIND=JPRB) :: ZEOTO
REAL(KIND=JPRB) :: ZNSOR
REAL(KIND=JPRB) :: ZP   

REAL(KIND=JPRB) :: ZDU        (4)
REAL(KIND=JPRB) :: ZC_U       (3)
REAL(KIND=JPRB) :: ZC_PU      (3)
REAL(KIND=JPRB) :: ZC_TU      (3)
REAL(KIND=JPRB) :: ZC_UW      (3)
REAL(KIND=JPRB) :: ZC_US      (3)
REAL(KIND=JPRB) :: ZC_US_IRHOV(3)
REAL(KIND=JPRB) :: ZC_UC      
REAL(KIND=JPRB) :: ZT_U       (3)
REAL(KIND=JPRB) :: ZT_PU      (3)
REAL(KIND=JPRB) :: ZT_TU      (3)
REAL(KIND=JPRB) :: ZT_UW      (3)
REAL(KIND=JPRB) :: ZT_US      (3)
REAL(KIND=JPRB) :: ZT_US_IRHOV(3)
REAL(KIND=JPRB) :: ZT_UC      

temp (REAL(KIND=JPRB), ZQ, (KLON,  KLEV))
temp (REAL(KIND=JPRB), ZIRHOV, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA0, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA1, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZDEOTA2, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA0, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA1, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZUEOTA2, (KLON,0:KLEV+1))

temp (REAL(KIND=JPRB), ZC_FW, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZC_FS, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZC_FC, (KLON,0:KLEV))
temp (REAL(KIND=JPRB), ZT_FW, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZT_FS, (KLON,0:KLEV,3))
temp (REAL(KIND=JPRB), ZT_FC, (KLON,0:KLEV))
REAL(KIND=JPRB) :: ZDEL0
REAL(KIND=JPRB) :: ZDEL1
temp (REAL(KIND=JPRB), ZDEL1D, (KLON,KLEV-1))
REAL(KIND=JPRB) :: ZTAU  
REAL(KIND=JPRB) :: ZTAU0A
REAL(KIND=JPRB) :: ZTAU1A
REAL(KIND=JPRB) :: ZTAU0B
REAL(KIND=JPRB) :: ZTAU1B
temp (REAL(KIND=JPRB), ZTAU0, (KLON,0:KLEV,0:KLEV))
temp (REAL(KIND=JPRB), ZTAU1, (KLON,0:KLEV,0:KLEV))


INTEGER(KIND=JPIM) :: JO_

REAL(KIND=JPRB) :: Z4BU_,ZA2B_,ZAFVOI_,ZVOIEMP_,ZVOIGT_
REAL(KIND=JPRB) :: ZAUX_,ZLOG_,ZP_AVG_,ZT_AVG_

REAL(KIND=JPRB) :: ZDELTA_(3)
REAL(KIND=JPRB) :: ZA_    (3)
REAL(KIND=JPRB) :: ZAR_   (3)
REAL(KIND=JPRB) :: ZCOEF_ (3)
REAL(KIND=JPRB) :: ZTAU_  (3)
#endif


! local integer scalars
INTEGER(KIND=JPIM) :: JG,JLEV,JLEV1,JLEV2,JLON,ILEV

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


RETURN

END SUBROUTINE ACRANEB_TRANST
