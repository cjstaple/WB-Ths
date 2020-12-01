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

      integer i,j,k,l
      integer p,q
      integer h0,ht

!-----------------------------------------------------------------------
      call prof_enter(4,1,'       DROP SEARCH: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(m)
!$OMP DO SCHEDULE(STATIC)
      do i=1,d1
       do j=1,d2
         if(m(i,j)%flow_solved) cycle
         h0=m(i,j)%height
         do k=-1,1,1
          do l=-1,1,1
            p=min(max(i+k,1),d1)
            q=min(max(j+l,1),d2)
            ht=m(p,q)%height
            if(ht.lt.h0) then
              h0=ht
              m(i,j)%outflow_cell(1)=p
              m(i,j)%outflow_cell(2)=q
              m(i,j)%flow_solved=.true.
            endif
          enddo
         enddo
       enddo
      enddo
!$OMP END DO
!$OMP END PARALLEL
      call prof_exit(4,1)

      end subroutine
