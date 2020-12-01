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
      call prof_enter(2,1,'       OCEAN CHECK: ')
      do i=1,d1
       do j=1,d2
         m(i,j)%outflow=0.
         m(i,j)%f_length=0
         if(m(i,j)%height.eq.0) then
            m(i,j)%ocean=.true.
            m(i,j)%flow_solved=.true.
            m(i,j)%flow_frac=0.
            m(i,j)%outflow_cell(1)=i
            m(i,j)%outflow_cell(2)=j
         else
            m(i,j)%ocean=.false.
            m(i,j)%flow_solved=.false.
            m(i,j)%flow_frac=0.5
            m(i,j)%outflow_cell(1)=d1
            m(i,j)%outflow_cell(2)=d2
         endif
       enddo
      enddo
      call prof_exit(2,1)
      call prof_write

      end subroutine
