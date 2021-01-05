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
            if(ht.lt.h0) then !New Candidate is at a lower height
              h0=ht
              g0=m(p,q)%grad
              m(i,j)%d_cell(1) = p
              m(i,j)%d_cell(2) = q
              m(i,j)%flow_solved=.true.
            else if((ht.eq.h0).and.(h0.lt.h00)) then !2 Drop points
              if(m(p,q)%grad.gt.g0) then
                 g0=m(p,q)%grad
                 m(i,j)%d_cell(1)=p
                 m(i,j)%d_cell(2)=q
                 m(i,j)%flow_solved=.true.
              endif
            endif
          enddo
         enddo
         if(m(i,j)%flow_solved) then
            p = abs(m(i,j)%d_cell(1)-i)
            q = abs(m(i,j)%d_cell(2)-j)
            if((p.gt.1).or.(q.gt.1)) then
               write(*,*) 'Error in Drop Search at Cell',i,j
               stop
            endif
         endif
       enddo
      enddo
      call prof_exit(4,1)

      end subroutine
!=======================================================================
