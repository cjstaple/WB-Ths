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
      real :: xA,vel
      real, parameter :: pi = 3.141592653589
!-----------------------------------------------------------------------
      call prof_enter(9,' FLOW RATE SOLVER: ')
      do i=1,d1
       do j=1,d2
         if(m(i,j)%out_rate.eq.0.) then
            vel = 0.
            xA = 0.
         else
            vel = m(i,j)%out_rate*804.672
            xA = m(i,j)%outflow/vel
         endif
         m(i,j)%flow_v = vel
         m(i,j)%xA = xA
         m(i,j)%vol = xA*804.672
         m(i,j)%depth=sqrt(2*xA/pi)
       enddo
      enddo

      call prof_exit(9)
      end subroutine
!=======================================================================
