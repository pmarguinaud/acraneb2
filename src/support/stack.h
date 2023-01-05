#ifndef _STACK_N
#define _STACK_N

#ifdef USE_STACK

#define temp(t, n, s) t, DIMENSION s :: n; POINTER (IP_##n##_, n)

#define MSIZE 8
#define WSIZE 32
#define alloc(n) IP_##n##_=YLSTACK%L+((KIDIA-1)/WSIZE)*WSIZE*(MSIZE-KIND(n));YLSTACK%L=YLSTACK%L+MSIZE*SIZE(n);IF(YLSTACK%L>YLSTACK%U)CALL ABOR1(__FILE__)

#else

#define temp(t, n, s) t, DIMENSION s :: n
#define alloc(n)

#endif

#endif
