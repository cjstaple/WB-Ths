!=======================================================================
!     Flow-Pathfinder: Parameter Module
!=======================================================================

      module parameter_module

      implicit none

!.....IMAGE PROPERTIES..................................................
      integer :: d1 = 1683     !first image dimension
      integer :: d2 = 2016     !second image diminesion
      real :: Lpx = 804.672    !Length of a px in m
      real :: hpx = 25.5       !height of a px in m
      real :: psol

      end module parameter_module
