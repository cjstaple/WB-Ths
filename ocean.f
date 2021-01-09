!=======================================================================
!      Flow-Pathfinder: Ocean Finder
!=======================================================================

      subroutine ocean_search(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none

      type(map_type), dimension(d1,d2) :: m

!-----------------------------------------------------------------------
      integer :: i,j
!-----------------------------------------------------------------------
      call prof_enter(2,'       OCEAN CHECK: ')
      m(:,:)%outflow = 0.
      m(:,:)%c_dis = 0
      m(:,:)%c_acc = 0
      do i=1,d1
       do j=1,d2
         if(m(i,j)%height.eq.0) then
            m(i,j)%ocean=.true.
            m(i,j)%flow_solved=.true.
         else
            m(i,j)%ocean=.false.
            m(i,j)%flow_solved=.false.
         endif
       enddo
      enddo
      call prof_exit(2)
      call prof_write

      end subroutine
!=======================================================================
