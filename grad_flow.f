!=======================================================================
!     Gradient Search Drain Subroutine
!=======================================================================

      subroutine grad_flow(m)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
!-----------------------------------------------------------------------

      integer :: i,j,k,l,r              !Loop Indices
      integer :: a,b                    !More Indices
      integer :: p,q,u,v                !Temp Coordinates
      integer :: xf,yf                  !Drain Coordinates
      integer :: h0,ht,h                !Height Values
      logical :: rep                    
      real :: d_max,d                   !Distance Variables

      integer, dimension(d1,d2) :: dist !Cell Distance from origin
      logical, dimension(d1,d2) :: lake !In Current Plain
      logical, dimension(d1,d2) :: visit !Cell Visited
      integer, dimension(2) :: drain    !Plain Drain Point

      integer :: tid
      integer :: OMP_GET_THREAD_NUM

!-----------------------------------------------------------------------

      call prof_enter(3,1,'   DRAIN DIRECTION: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(d1,d2,m)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(3,tid,'   DRAIN DIRECTION: ')
!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
         if(m(i,j)%ocean) cycle
         if(m(i,j)%flow_solved) cycle
         if(m(i,j)%active) cycle
         m(i,j)%active=.true.
         h0=m(i,j)%height
!........Check For a Nearby Drop........................................
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
         if(h0.lt.m(i,j)%height) cycle
!........Define Plain...................................................
         lake(i,j)=.true.
         visit(i,j)=.true.
         do k=-1,1,1
          do l=-1,1,1
            p=min(max(i+k,1),d1)
            q=min(max(j+l,1),d2)
            if(m(p,q)%height.eq.h0) then
               lake(p,q)=.true.
               m(p,q)%active=.true.
            endif
          enddo
         enddo
         rep=.true.
         r=0
         ht=h0
         do while(rep)
            r=r+1
            rep=.false.
            do k=-r,r,1
             do l=-r,r,1
               p=min(max(i+k,1),d1)
               q=min(max(j+l,1),d2)
               if(visit(p,q)) cycle
               if(lake(p,q)) then
                  do a=-1,1,1
                   do b=-1,1,1
                     u=min(max(p+a,1),d1)
                     v=min(max(q+b,1),d2)
                     if(visit(u,v)) cycle
                     if(m(u,v)%height.eq.h0) then
                        lake(u,v)=.true.
                        m(u,v)%active=.true.
                        rep=.true.
                     endif
                     if(m(u,v)%height.lt.ht) then
                        ht=m(u,v)%height
                        drain(1)=u
                        drain(2)=v
                     endif
                   enddo
                  enddo
               endif
               visit(p,q)=.true.
             enddo
            enddo
         enddo
!........Create Plain Flow..............................................
         d=0
         d_max=d1
         do k=1,d1
          do l=1,d2
            if(m(k,l)%flow_solved) cycle
            if(lake(k,l)) then
              do a=-1,1,1
               do b=-1,1,1
                 d=1.0*sqrt((k+a-drain(1))**2.+(l+b-drain(2))**2)
                 if(d.lt.d_max) then
                    d_max=d
                    m(k,l)%outflow_cell(1)=k+a
                    m(k,l)%outflow_cell(2)=l+b
                    m(k,l)%flow_solved=.true.
                 endif
               enddo
              enddo
              m(k,l)%active=.false.
            endif
          enddo
         enddo
       enddo
      enddo
!$OMP END DO
      call prof_exit(3,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(3,1)

      end subroutine
!=======================================================================
