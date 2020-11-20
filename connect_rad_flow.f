!=======================================================================
!     Growing Radius Search - Connected Drain Point
!=======================================================================

      subroutine drain_connect(m,x0,y0)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer :: x0,y0          !Draining Pixel
!-----------------------------------------------------------------------

      integer :: i,j,k,l        !Loop Indices
      integer :: p,q,u,v        !Coordinates
      integer :: r,xf,yf        !Search Range
      integer :: h0,ht,h        !Height Variables

      real :: rwalk,s,s0        !Direction Variables

      logical, dimension(d1,d2) :: connected
      logical :: finish

!-----------------------------------------------------------------------
!.....Initialization....................................................
      if(m(x0,y0)%ocean) then
         m(x0,y0)%outflow_cell(1)=x0
         m(x0,y0)%outflow_cell(2)=y0
         return
      endif
      finish=.false.
      h0=m(x0,y0)%height
      r=1
      do i=1,d1
       do j=1,d2
         connected(i,j)=.false.
       enddo
      enddo
!.....Initialize Connected Set & Check For Immediate Drop...............
      do i=-1,1,1
       do j=-1,1,1
         p=min(max(x0+i,1),d1)
         q=min(max(y0+j,1),d2)
         ht=m(p,q)%height
         if(ht.le.h0) connected(p,q)=.true.
         if(ht.lt.h0) then
            h0=ht
            m(x0,y0)%outflow_cell(1)=p
            m(x0,y0)%outflow_cell(2)=q
            finish=.true.
         endif
       enddo
      enddo
      if(finish) return
!.....Find Nearest Drain Point..........................................
      do while(h0.eq.m(x0,y0)%height)
       do i=-r,r,1
        do j=-r,r,1
          p=min(max(x0+i,1),d1)
          q=min(max(y0+j,1),d2)
          if(connected(p,q)) then
             do k=-1,1,1
              do l=-1,1,1
                u=min(max(p+k,1),d1)
                v=min(max(q+l,1),d2)
                ht=m(u,v)%height
                if(ht.le.h0) connected(u,v)=.true.
                if(ht.lt.h0) then
                   h0=ht
                   xf=u
                   yf=v
                endif
              enddo
             enddo
          endif
        enddo
       enddo
       r=r+1
      enddo
!.....Drain into connected cell nearest the drain point.................
      s0=2.0d+00*r
      do i=-1,1,1
       do j=-1,1,1
         p=min(max(x0+i,1),d1)
         q=min(max(y0+j,1),d2)
         if(connected(p,q)) then
           s=sqrt(1.0*(xf-p)**2. + 1.0*(yf-q)**2.)
           if(s.lt.s0) then
             s0=s
             m(x0,y0)%outflow_cell(1)=p
             m(x0,y0)%outflow_cell(2)=q
           elseif(s.eq.s0) then
             rwalk=rand()
             if(rwalk.gt.0.5) then
               m(x0,y0)%outflow_cell(1)=p
               m(x0,y0)%outflow_cell(2)=q
             endif
           endif
         endif
       enddo
      enddo

      return
      end subroutine
!=======================================================================
