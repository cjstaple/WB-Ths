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
      real :: xA_1,xA_2
      real :: vel_1,vel_2
      real, parameter :: pi = 3.141592653589
      real, parameter :: n = 0.03
      real, parameter :: ba = 6.4
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
         xA_1 = (ba*((n*m(i,j)%outflow)/(sqrt(grad)))**3.)**(1./4.)
         vel_1 = m(i,j)%outflow/xA_1
         vel_2 = 804.672*rout
         xA_2 = m(i,j)%outflow/vel_2

         m(i,j)%vel_out = vel_1            ! [m/s]
         m(i,j)%xA = xA_1                  ! [m^2]
         m(i,j)%v_o = vel_2                ! [m/s]
         m(i,j)%Ax = xA_2                  ! [m^2]
         m(i,j)%vol_s = m(i,j)%xA*804.672  ! [m^3]
         m(i,j)%dep = sqrt(2*m(i,j)%xA/pi) ! [m]
       enddo
      enddo

      call prof_exit(9,1)
      end subroutine
!=======================================================================
