! header file to implement Philippe Marguinauds ingenious stack mechanism

#ifdef USE_ACC
!$acc routine(ACRANEB_TRANST) seq
!$acc routine(ACRANEB_TRANSS) seq
!$acc routine(ACRANEB_COEFT) seq
!$acc routine(ACRANEB_COEFS) seq
!$acc routine(ACRANEB_SOLVT) seq
!$acc routine(ACRANEB_SOLVT3) seq
!$acc routine(ACRANEB_SOLVS) seq
!$acc routine(AC_CLOUD_MODEL2) seq
#endif

#ifdef USE_STACK

! special treatment for GFORTRAN, as its preprocessor doesn't handle stringification (#) and concatenation (##) so nicely
#ifdef __GFORTRAN__
#define PASTE(a) a
#define CAT(a,b) PASTE(a)b
#define temp(t, n, s) t, DIMENSION s :: n; POINTER (CAT(CAT(IP_,n),_), n)
#define init_stack() INTEGER(KIND=8) :: ISTACKPTR; ISTACKPTR = KSTACKPTR
#define alloc(n) CAT(CAT(IP_,n),_) = LOC (PSTACK (ISTACKPTR)); ISTACKPTR = ISTACKPTR + SIZE (n); IF (ISTACKPTR > KSTACKSIZE) STOP CAT("Stack depleted at ",n)
#else
#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)
#define init_stack() INTEGER(KIND=8) :: ISTACKPTR; ISTACKPTR = KSTACKPTR
#define alloc(n) IP_##n##_ = LOC (PSTACK (ISTACKPTR)); ISTACKPTR = ISTACKPTR + SIZE (n); IF (ISTACKPTR > KSTACKSIZE) STOP "Stack depleted at "//#n
#endif

#else

#define temp(t, n, s) t :: n s
#define alloc(n)
#define init_stack() INTEGER(KIND=8) :: ISTACKPTR

#endif

