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
      real :: rout
!-----------------------------------------------------------------------
      call prof_enter(9,1,' FLOW RATE SOLVER: ')
      do i=1,d1
       do j=1,d2
         if(m(i,j)%out_rate.eq.0.) then
            rout = 1.
         else
            rout = m(i,j)%out_rate
         endif
         m(i,j)%vol_0 = m(i,j)%outflow/(960*rout) ! m^3
         m(i,j)%xA = m(i,j)%vol_0/(9*rout) ! m^2
         m(i,j)%dep= sqrt(2*m(i,j)%xA/3.141592653589) ! m
       enddo
      enddo

      call prof_exit(9,1)
      end subroutine
!=======================================================================
