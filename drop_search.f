!=======================================================================
!      Flow Pathfinder: Drop Search
!=======================================================================

      subroutine drop_search(m)

      use map_module
      use prof_module
      use parameter_module

      implicit none

!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
!-----------------------------------------------------------------------

      integer :: i,j,k,l
      integer :: p,q
      integer :: h00,h0,ht
      real :: g,g0

!-----------------------------------------------------------------------
      call prof_enter(4,1,'       DROP SEARCH: ')
      do i=1,d1
       do j=1,d2
         if(m(i,j)%flow_solved) cycle
         h00=m(i,j)%height
         h0=m(i,j)%height
         do k=-1,1,1
          do l=-1,1,1
            p=min(max(i+k,1),d1)
            q=min(max(j+l,1),d2)
            ht=m(p,q)%height
            if((ht.eq.h0).and.(h0.lt.h00)) then
              g=m(p,q)%out_rate
              if(g.gt.g0) then
                 g0=g
                 m(i,j)%d_cell(1)=p
                 m(i,j)%d_cell(2)=q
              endif
            endif
            if(ht.lt.h0) then
              h0=ht
              g0=m(p,q)%out_rate
              m(i,j)%d_cell(1)=p
              m(i,j)%d_cell(2)=q
              m(i,j)%flow_solved=.true.
            endif
          enddo
         enddo
       enddo
      enddo
      call prof_exit(4,1)

      end subroutine
!=======================================================================
