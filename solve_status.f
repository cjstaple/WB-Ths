!=======================================================================
!     Flow-Pathfinder: % of Pixels that have been solved
!=======================================================================

      subroutine psolved(m)

      use prof_module
      use map_module

      implicit none

      type(map_type), dimension(d1,d2) :: m
      integer :: i,j,nsol
      real :: psl

      nsol=0
      do i=1,d1
       do j=1,d2
         if(m(i,j)%flow_solved) nsol=nsol+1
       enddo
      enddo
      psl = 1.0d+00*nsol/(d1*d2)
      psol = 100.*psl

      end subroutine
!=======================================================================
