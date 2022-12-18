INTERFACE

SUBROUTINE ACRANEB2( &
#ifdef UNDEF
 & YDERDI,YDRIP,YDML_PHY_MF,&
#endif
 & KIDIA,KFDIA,KLON,KTDIA,KLEV&
#ifdef UNDEF
 & ,KJN,KSTEP, &

 & PAPRS,PAPRSF,PCP,PR,PDELP,PNEB,PQ,PQCO2,PQICE,PQLI,PQO3,PT, &

 & PALB,PALBDIR,PEMIS,PGELAM,PGEMU,PMU0,PMU0LU,PTS,PDECRD,PCLCT, &

 & PGDEOSI,PGUEOSI,PGMU0,PGMU0_MIN,PGMU0_MAX, &
 & PGDEOTI,PGDEOTI2,PGUEOTI,PGUEOTI2,PGEOLT,PGEOXT, &
 & PGRPROX,PGMIXP,PGFLUXC,PGRSURF,PSDUR, &

 & PFRSO,PFRTH, &

 & PFRSODS,PFRSOPS,PFRSOLU,PFRTHDS, &

 & PDAER, YDSTACK&
#endif
 & )
!$acc routine (ACRANEB2) seq

USE MODEL_PHYSICS_MF_MOD , ONLY : MODEL_PHYSICS_MF_TYPE
USE PARKIND1 ,ONLY : JPIM     ,JPRB     ,JPRD
USE YOMCST   ,ONLY : RSIGMA   ,RG       ,RPI      ,RDAY

USE YOMRIP   ,ONLY : TRIP
USE YOERDI   ,ONLY : TERDI
USE STACK_MOD

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

END SUBROUTINE ACRANEB2

END INTERFACE
