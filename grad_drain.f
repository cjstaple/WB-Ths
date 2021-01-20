!=======================================================================
!     Flow-Pathfinder: Flow Function
!=======================================================================

      subroutine grad_drain(m)

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
      logical :: rep            !Do While Logical Control

!-----------------------------------------------------------------------
      call prof_enter(7,'    DOWN FLOW CALC: ')
      m(:,:)%gradflow=0.
      m(:,:)%g_acc=0
      m(:,:)%g_dis=0
      do i=1,d1
       do j=1,d2
         if(m(i,j)%ocean) cycle
         f_count = 0
         source = m(i,j)%rain
         m(i,j)%gradflow = m(i,j)%gradflow + source
         h = m(i,j)%height
         x0=i
         y0=j
         rep = .true.
         do while(rep)
           f_count = f_count + 1
           source = source*0.9975
           x=m(x0,y0)%g_cell(1)
           y=m(x0,y0)%g_cell(2)
           if((x.eq.0).or.(y.eq.0)) then
             write(*,*) 'Discharge Out of Bounds'
             write(*,*) 'Starting from node:', i,j
             write(*,*) 'Error sourcing desintain of:',x0,y0
             write(*,*) 'Given Destination:',x,y
             stop
           endif
           m(x,y)%gradflow = m(x,y)%gradflow + source
           h=m(x,y)%height
           m(x,y)%g_acc=m(x,y)%g_acc+1
           x0=x
           y0=y
           if(h.eq.0) rep=.false.
           if((x0.eq.i).and.(y0.eq.j)) rep=.false.
           if(source.lt.0.1) rep=.false.
         enddo
         m(i,j)%g_dis = f_count
         m(i,j)%gradflow = m(i,j)%gradflow - m(i,j)%rain
       enddo
      enddo

      do i=1,d1
       do j=1,d2
         m(i,j)%gradflow=m(i,j)%gradflow/(3.60d+02)/(8.64d+04) !m^3/s
         if(m(i,j)%ocean) m(i,j)%gradflow = 0.0
       enddo
      enddo
      call prof_exit(7)
      call prof_write

      return
      end subroutine
!=======================================================================
