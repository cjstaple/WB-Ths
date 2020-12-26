!=======================================================================
!     Flow-Pathfinder: % of Pixels that have been solved
!=======================================================================

      subroutine psolved(m)

      use parameter_module
      use map_module
      use prof_module

      implicit none

      type(map_type), dimension(d1,d2) :: m
      integer :: i,j,nsol

      nsol=0
      do i=1,d1
       do j=1,d2
         if(m(i,j)%flow_solved) nsol=nsol+1
       enddo
      enddo
      psol=(1.0d+00*nsol)/(d1*d2)

      return
      end subroutine
!=======================================================================
