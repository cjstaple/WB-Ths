!=======================================================================
! Evaporation & Freezing Subroutine
!=======================================================================
      
      subroutine frac_calc(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none

!-----------------------------------------------------------------------
      type(map_type),dimension(d1,d2) :: m
      integer :: i, j
      real :: t_av              ! Cell Temperature
!-----------------------------------------------------------------------
      call prof_enter(3,1,'    CELL SINK CALC: ')
      do i=1,d1
       do j=1,d2
          t_av = 1.0d+00*(m(i,j)%temp)-2.900d+02
          m(i,j)%flow_frac = 5.43d-01-t_av*(7.3513d-04)
       enddo
      enddo
      call prof_exit(3,1)
      call prof_write

      end subroutine
!=======================================================================
