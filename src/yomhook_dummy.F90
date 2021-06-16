module yomhook
use parkind1, only : jprb
implicit none
logical :: lhook=.false.

!$acc declare create(lhook)

contains
subroutine dr_hook(cdstr,ki,phook)
  character(len=*) :: cdstr
	integer :: ki
	real(kind=jprb) :: phook
end subroutine dr_hook
end module
