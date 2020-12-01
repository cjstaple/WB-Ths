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

      real :: f_v,f_i !fractions lost to evaporation & freezing
      integer :: i, j
      integer :: tid
      integer :: OMP_GET_THREAD_NUM

      real :: t_av              ! Cell Temperature
      real :: t_vap = 3.75d+02  ! Vapor Temperature
      real :: t_ice = 2.70d+02  ! Freeze Temperature
!-----------------------------------------------------------------------
      call prof_enter(3,1,'    CELL SINK CALC: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(m,t_vap,t_ice)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(3,tid,'    CELL SINK CALC: ')
!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
          t_av = 1.0d+00*(m(i,j)%temp)
          m(i,j)%flow_frac = 0.543-0.00073513*(t_av-290.)
       enddo
      enddo
!$OMP END DO
      call prof_exit(3,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(3,1)
      call prof_write

      end subroutine
!=======================================================================
