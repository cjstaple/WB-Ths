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
      integer :: i,j,k,l
      integer :: p,q
      real :: rout
      logical :: rep
!-----------------------------------------------------------------------
      call prof_enter(9,1,' FLOW RATE SOLVER: ')
      do i=1,d1
       do j=1,d2
         m(i,j)%vol_0 = 0
       enddo
      enddo
      do while(rep)
       do i=1,d1
        do j=1,d2
         if(m(i,j)%rate_solved) cycle
         if(m(i,j)%out_rate.eq.0) then
            rout = 1.
         else
            rout = m(i,j)%out_rate
         endif
         m(i,j)%vol_0=m(i,j)%rain*m(i,j)%flow_frac*(1.873d-03)/rout
         do k=-1,1,1
          do l=-1,1,1
            p = min(max(i+k,1),d1)
            q = min(max(j+l,1),d2)
            if((m(p,q)%outflow_cell(1).eq.i).and.
     &         (m(p,q)%outflow_cell(2).eq.j)) then
              m(i,j)%vol_0 = m(i,j)%vol_0 + 
     &                       m(p,q)%vol_0*m(p,q)%out_rate/rout
            endif
          enddo
         enddo
         m(i,j)%rate_solved=.true.
         p=m(i,j)%outflow_cell(1)
         q=m(i,j)%outflow_cell(2)
         if(m(p,q)%rate_solved) m(p,q)%rate_solved=.false.
        enddo
       enddo
       rep =.false.
       do i=1,d1
        do j=1,d2
          if(m(i,j)%rate_solved) then
             continue
          else
             rep=.true.
          endif
        enddo
       enddo
      enddo

      do i=1,d1
       do j=1,d2
         if(m(i,j)%out_rate.gt.0) then
            rout = m(i,j)%out_rate
         else
            rout = 1.
         endif
         m(i,j)%xA = m(i,j)%vol_0/(9*rout)
         m(i,j)%dep = sqrt(2*m(i,j)%xA/3.141592653589)
       enddo
      enddo

      call prof_exit(9,1)
      end subroutine
!=======================================================================
