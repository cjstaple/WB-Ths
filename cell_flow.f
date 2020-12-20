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
      integer :: i,j            !indices
      integer :: x,y,x0,y0      !coordinates
      integer :: h              !current height
      real :: source            !m^3(1 year run)
      integer :: f_count

!-----------------------------------------------------------------------
      call prof_enter(7,1,'    DOWN FLOW CALC: ')
      do i=1,d1
       do j=1,d2
        m(i,j)%outflow = 0.0
       enddo
      enddo

      do i=1,d1
       do j=1,d2
          f_count = 0
          source = m(i,j)%rain*m(i,j)%flow_frac
          m(i,j)%outflow = m(i,j)%outflow + source
          h = m(i,j)%height
          x0=i
          y0=j
          if(.not.m(i,j)%flow_solved) call drain_path(m,i,j)
          do while(h.gt.0)
            f_count = f_count + 1
            x=m(x0,y0)%outflow_cell(1)
            y=m(x0,y0)%outflow_cell(2)
            source = source*0.975
            m(x,y)%outflow = m(x,y)%outflow + source
            h=m(x,y)%height
            m(x,y)%d_px=m(x,y)%d_px+1
            x0=x
            y0=y
            if((x0.eq.i).and.(y0.eq.j)) h=0
            if(source.lt.0.1) h=0
          enddo
          m(i,j)%f_length = f_count
       enddo
      enddo

      do i=1,d1
       do j=1,d2
         m(i,j)%outflow=m(i,j)%outflow/(3.60d+02)/(8.64d+04) !m^3/s
       enddo
      enddo
      call prof_exit(7,1)
      call prof_write

      return
      end subroutine
