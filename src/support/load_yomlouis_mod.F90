MODULE LOAD_YOMLOUIS_MOD

USE YOMLOUIS

CONTAINS

SUBROUTINE LOAD_YOMLOUIS (KLUN)
USE LOAD_TLOUIS_MOD

IMPLICIT NONE
INTEGER, INTENT (IN) :: KLUN
LOGICAL :: LYRLOUIS
READ (KLUN) LYRLOUIS
IF (LYRLOUIS) THEN
  ALLOCATE (YRLOUIS)
  CALL LOAD (KLUN, YRLOUIS)
ELSE
  NULLIFY (YRLOUIS)
ENDIF
END SUBROUTINE


END MODULE
