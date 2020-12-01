!=======================================================================
!     Flow Pathfinder: Level Drain Random Step
!=======================================================================

      subroutine rand_drain(m,x0,y0,xf,yf)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer,intent(in) :: x0,y0
      integer,intent(out) :: xf,yf
!-----------------------------------------------------------------------

      integer :: i,j,k,l
      integer :: p,q,u,v
      integer :: h0,ht

      real :: rwalk,s,s0
      integer, dimension(3) :: cpt
      integer :: cpti
      logical :: set

!-----------------------------------------------------------------------
      call prof_enter(5,1,'      RANDOM DRAIN: ')
      call itime(cpt)
      cpti=mod(cpt(1)*cpt(2)*cpt(3),10000)
      call srand(cpti)
      set=.false.
      h0=m(x0,y0)%height

      do i=-1,1
       do j=-1,1
         p=min(max(x0+i,1),d1)
         q=min(max(y0+j,1),d2)
         ht=m(p,q)%height
         if(ht.lt.h0) then
            xf=p
            yf=q
            h0=ht
            set=.true.
         elseif(ht.eq.h0) then
            if(set) then
              rwalk=rand()
              if(rwalk.gt.0.5) then
                xf=p
                yf=q
              endif
            else
              xf=p
              yf=q
              set=.true.
            endif
         endif
       enddo
      enddo

      return
      end subroutine
