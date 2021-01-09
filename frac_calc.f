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
      real :: t_av, fl
!-----------------------------------------------------------------------
      call prof_enter(3,'    CELL SINK CALC: ')
      do i=1,d1
       do j=1,d2
          t_av = 1.0d+00*(m(i,j)%temp)-2.900d+02
          fl = 5.43d-01-t_av*(7.3513d-04)
          m(i,j)%rain=m(i,j)%rain*fl
       enddo
      enddo
      call prof_exit(3)
      call prof_write

      end subroutine
!=======================================================================
