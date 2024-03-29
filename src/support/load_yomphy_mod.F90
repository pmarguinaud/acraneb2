MODULE LOAD_YOMPHY_MOD

USE YOMPHY

CONTAINS

SUBROUTINE LOAD_YOMPHY (KLUN)
USE LOAD_TPHY_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
LOGICAL :: LYRPHY
READ (KLUN) LYRPHY
IF (LYRPHY) THEN
  ALLOCATE (YRPHY)
  CALL LOAD (KLUN, YRPHY)
ELSE
  NULLIFY (YRPHY)
ENDIF
END SUBROUTINE


END MODULE
