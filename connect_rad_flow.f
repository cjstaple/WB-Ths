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

      integer, dimension(3) :: cpt
      integer :: cpti
      integer :: tid
      integer :: OMP_GET_THREAD_NUM

!-----------------------------------------------------------------------
!.....Initialization....................................................
      if(m(x0,y0)%ocean) then
         m(x0,y0)%outflow_cell(1)=x0
         m(x0,y0)%outflow_cell(2)=y0
         m(x0,y0)%flow_solved=.true.
         return
      endif
      call prof_enter(4,1,'        CELL DRAIN: ')
      finish=.false.
      h0=m(x0,y0)%height
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
         if(ht.le.m(x0,y0)%height) connected(p,q)=.true.
         if(ht.lt.h0) then
            h0=ht
            m(x0,y0)%outflow_cell(1)=p
            m(x0,y0)%outflow_cell(2)=q
            m(x0,y0)%flow_solved=.true.
            finish=.true.
         endif
       enddo
      enddo
      if(finish) then
        call prof_exit(4,1)
        return
      endif
      call prof_enter(5,1,'       LEVEL DRAIN: ')
!-----Set Up Parallelization--------------------------------------------
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(m,x0,y0,h0,xf,yf,r)
      tid=OMP_GET_THREAD_NUM()
      call itime(cpt)
      tid=tid+2
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      cpti=mod(cpt(3)*tid,10000)
      call prof_enter(5,tid,'      LEVEL DRAIN: ')
      call srand(cpti)
      r=1

!.....Find Nearest Drain Point..........................................
      do while(h0.eq.m(x0,y0)%height)
       call prof_enter(6,tid,'    RADIUS SEARCH: ')
!$OMP DO SCHEDULE(STATIC) COLLAPSE(2)
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
                if(ht.le.m(x0,y0)%height) connected(u,v)=.true.
                if(ht.lt.h0) then
!$OMP CRITICAL
                   h0=ht
                   xf=u
                   yf=v
!$OMP END CRITICAL
                endif
              enddo
             enddo
          endif
        enddo
       enddo
!$OMP END DO
       call prof_exit(6,tid)
       r=r+1
      enddo
!.....Drain into connected cell nearest the drain point.................
!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do k=1,d1
       do l=1,d2
         if(connected(k,l)) then
           s0=2.0d+03
           do i=-1,1,1
            do j=-1,1,1
              p=min(max(k+i,1),d1)
              q=min(max(l+j,1),d2)
              if(connected(p,q)) then
                s=sqrt(1.0*(xf-p)**2. + 1.0*(yf-q)**2.)
                if(s.lt.s0) then
                  s0=s
                  m(k,l)%outflow_cell(1)=p
                  m(k,l)%outflow_cell(2)=q
                  m(k,l)%flow_solved=.true.
                elseif(s.eq.s0) then
                  rwalk=rand()
                  if(rwalk.gt.0.5) then
                    m(k,l)%outflow_cell(1)=p
                    m(k,l)%outflow_cell(2)=q
                    m(k,l)%flow_solved=.true.
                  endif
                endif
              endif
            enddo
           enddo
           if(m(k,l)%flow_solved) then
             continue
           else
            open(10,file='error.log',status='unknown',position='append')
            write(10,11) 'Failed to Solve Connected Node:',k,l,s0,
     &         m(k,l)%height
            close(10)
           endif
         endif
       enddo
      enddo
!$OMP END DO
!-----End Parallelization-----------------------------------------------
      call prof_exit(5,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(5,1)
      call prof_exit(4,1)

11    format(1x,a33,i5,i5,' Dist: ',f12.5,' Height: ',i5)

      return
      end subroutine
!=======================================================================
