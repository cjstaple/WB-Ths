!=======================================================================
!     Growing Radius Drain Pixel
!=======================================================================

      subroutine cell_drain(m)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
!-----------------------------------------------------------------------

      integer :: i,j              !Loop Indices
      integer,dimension(3) :: cpt
      integer :: cpti

      integer :: tid
      integer :: OMP_GET_THREAD_NUM
!-----------------------------------------------------------------------
      call prof_enter(3,1,'   DRAIN DIRECTION: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(d1,d2,m)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call itime(cpt)
      cpti=mod(cpt(3)*tid,10000)
      call srand(cpti)
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(3,tid,'   DRAIN DIRECTION: ')

!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
!$OMP CRITICAL
        call prof_write
!$OMP END CRITICAL
        call prof_enter(4,tid,'       LEVEL DRAIN: ')
        call drain_connect(m,i,j)
        call prof_exit(4,tid)
       enddo
      enddo
!$OMP END DO

      call prof_exit(3,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(3,1)

      end subroutine
!=======================================================================
