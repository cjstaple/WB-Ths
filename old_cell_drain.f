!=======================================================================
! Evaporation & Freezing Subroutine
!=======================================================================
      
      subroutine cell_flow(m)

      use map_module
      use prof_module
      use parameter_module

      implicit none

!-----------------------------------------------------------------------

      type(map_type),dimension(d1,d2) :: m

!-----------------------------------------------------------------------

      integer :: i,j,k,l !Loop Indices
      integer :: p,q,r ! Gradient Range
      integer :: nx,ny ! Cardinal Slopes
      integer :: htest ! Current Best
      real :: sx,sy

      integer :: tid
      integer :: OMP_GET_THREAD_NUM

!-----------------------------------------------------------------------
      call prof_enter(3,1,' OUTFLOW CELL CALC: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(d1,d2,m)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(3,tid,' OUTFLOW CELL CALC: ')
!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
          call prof_enter(4,tid,'       SINGLE CELL: ')
          sx = 0.
          sy = 0.
          nx = 0
          ny = 0
          htest = m(i,j)%height
          if(htest.eq.0) then
             call prof_exit(4,tid)
             cycle
          endif
          call prof_enter(5,tid,'        FALL CHECK: ')
          do k=0,2
           do l=0,2
              p=min(max(i+k-1,1),d1)
              q=min(max(j+l-1,1),d2)
              if(m(p,q)%height.lt.htest) then
                 htest = m(p,q)%height
                 nx=p
                 ny=q
              endif
           enddo
          enddo
          call prof_exit(5,tid)
          if((nx.eq.0).or.(ny.eq.0)) then
             call prof_enter(6,tid,'       LEVEL DRAIN: ')
             call level_drain(m,i,j)
             call prof_exit(6,tid)
          else
             m(i,j)%outflow_cell(1) = nx
             m(i,j)%outflow_cell(2) = ny
          endif
          call prof_exit(4,tid)
!$OMP CRITICAL
          call prof_write
!$OMP END CRITICAL
       enddo
      enddo
!$OMP END DO
      call prof_exit(3,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(3,1)

      end subroutine
!=======================================================================
