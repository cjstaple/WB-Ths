!=======================================================================
!     Flow-Pathfinder: River Properteries
!=======================================================================

      subroutine flow_rate(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none

      type(map_type), dimension(d1,d2) :: m

!-----------------------------------------------------------------------
      integer :: i,j
      real :: rout,grad
      real, parameter :: pi = 3.141592653589
      real, parameter :: n = 0.03
!-----------------------------------------------------------------------
      call prof_enter(9,1,' FLOW RATE SOLVER: ')
      do i=1,d1
       do j=1,d2
         if(m(i,j)%out_rate.eq.0.) then
            rout = 1.
            grad = 1.
         else
            rout = m(i,j)%out_rate
            grad = m(i,j)%grad
         endif
         m(i,j)%vol_0 = m(i,j)%outflow/(rout) ! m^3
         m(i,j)%xA = ((2*pi)/(sqrt(grad))*
     &                (n*m(i,j)%outflow)**3)**(1./4.)  ! m^2
         m(i,j)%dep = sqrt(2*m(i,j)%xA/pi) ! m
       enddo
      enddo

      call prof_exit(9,1)
      end subroutine
!=======================================================================
