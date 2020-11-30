!=======================================================================
!     Flow Pathfinder: Level Drain Random Step
!=======================================================================

      subroutine rand_drain(m,x0,y0)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer :: x0,y0
!-----------------------------------------------------------------------

      integer :: i,j,k,l
      integer :: p,q,u,v
      integer :: h0,ht

      real :: rwalk,s,s0
      integer, dimension(3) :: cpt
      integer :: cpti
      logical :: set


!-----------------------------------------------------------------------
      call prof_enter(5,1,'       LEVEL DRAIN: ')
      call itime(cpt)
      cpti=mod(cpt(1)*cpt(2)*cpt(3),10000)
      call srand(cpti)
      set=.false.
      h0=m(x0,y0)%height

      do i=-1,1
       do j=-1,1
         p=min(max(x0+i,1),d1)
         q=min(max(y0+j,2),d2)
         ht=m(p,q)%height
         if(ht.lt.h0) then
            m(x0,y0)%outflow_cell(1)=p
            m(x0,y0)%outflow_cell(2)=q
            m(x0,y0)%flow_solved=.true.
            return
         elseif(ht.eq.h0) then
            if(set) then
               rwalk=rand()
               if(rwalk.gt.0.5) then
                 m(x0,y0)%outflow_cell(1)=p
                 m(x0,y0)%outflow_cell(2)=q
               endif
            else
               m(x0,y0)%outflow_cell(1)=p
               m(x0,y0)%outflow_cell(2)=q
               set=.true.
            endif
         endif
       enddo
      enddo
      m(x0,y0)%flow_solved=.true.

      end subroutine
