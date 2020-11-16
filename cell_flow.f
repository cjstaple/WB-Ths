!=======================================================================
!     Flow-Pathfinder: Flow Function
!=======================================================================

      subroutine cell_flow(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none

      type(map_type), dimension(d1,d2) :: m

!-----------------------------------------------------------------------
      integer :: i,j !indices
      integer :: x,y,x0,y0!coordinates
      integer :: h   !current height
      real :: source !m^3(1 year run)
      integer :: f_count

      integer :: tid
      integer :: OMP_GET_THREAD_NUM

!-----------------------------------------------------------------------
      call prof_enter(7,1,'    DOWN FLOW CALC: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(d1,d2,m)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(8,tid,'      ZEROING FLOW: ')
!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
        m(i,j)%outflow = 0.0
       enddo
      enddo
!$OMP END DO
      call prof_exit(8,tid)

      call prof_enter(9,tid,'  PROPOGATING FLOW: ')
!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
          f_count = 0
          source = m(i,j)%rain*(647.5)*m(i,j)%flow_frac
          m(i,j)%outflow = m(i,j)%outflow + source
          h = m(i,j)%height
          x0=i
          y0=j
          do while(h.gt.0)
            f_count = f_count + 1
            x=m(x0,y0)%outflow_cell(1)
            y=m(x0,y0)%outflow_cell(2)
            source = source*0.95
            m(x,y)%outflow = m(x,y)%outflow+source
            h=m(x,y)%height
            x0=x
            y0=y
            if((x0.eq.i).and.(y0.eq.j)) h=0
            if(source.lt.1.0) h=0
            if(mod(f_count,10).eq.0) then
!$OMP CRITICAL
               call prof_write
!$OMP END CRITICAL
            endif
            if(f_count.gt.500) h=0
          enddo
          m(i,j)%f_length = f_count
       enddo
      enddo
!$OMP END DO
      call prof_exit(9,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(7,1)

      return
      end subroutine
