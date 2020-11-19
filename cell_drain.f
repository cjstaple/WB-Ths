!=======================================================================
!     Growing Radius Drain Pixel
!=======================================================================

      subroutine cell_drain(m)

      use map_module
      use prof_module
      use parameter_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
!-----------------------------------------------------------------------

      integer :: i,j,k,l,a,b    !Loop Indices
      integer :: p,q,u,v,xf,yf  !Coordinates
      integer :: r              !Search Range
      real :: s,s0              !Cell Distance
      integer :: h0             !Initial Cell Height
      integer :: ht             !Test Cell Height
      real :: rwalk
      integer,dimension(3) :: cpt

      logical, dimension(d1,d2) :: connected

      integer :: tid
      integer :: OMP_GET_THREAD_NUM
!-----------------------------------------------------------------------
      call prof_enter(3,1,'   DRAIN DIRECTION: ')
!$OMP PARALLEL DEFAULT(PRIVATE) SHARED(d1,d2,m)
      tid=OMP_GET_THREAD_NUM()
      tid=tid+2
      call itime(cpt)
      call srand(mod(tid*cpt(3),100000))
      call prof_enter(n_max,tid,'    TOTAL RUN TIME: ')
      call prof_enter(3,tid,'   DRAIN DIRECTION: ')
!$OMP CRITICAL
      call prof_write
!$OMP END CRITICAL

!$OMP DO SCHEDULE(DYNAMIC) COLLAPSE(2)
      do i=1,d1
       do j=1,d2
        connected=.false.
!$OMP CRITICAL
        call prof_write
!$OMP END CRITICAL
        if(m(i,j)%ocean) then
           m(i,j)%outflow_cell(1)=i
           m(i,j)%outflow_cell(2)=j
        else
           r=1
           h0=m(i,j)%height
           !Set Connected Boundary
           do k=-1,1,1
            do l=-1,1,1
              p=min(max(i+k,1),d1)
              q=min(max(j+l,1),d2)
              if(m(p,q)%height.le.h0) connected(p,q)=.true.
            enddo
           enddo
           !Find Nearest Drain Point
           do while(h0.eq.m(i,j)%height)
            do k=-r,r,1
             do l=-r,r,1
              p=min(max(i+k,1),d1)
              q=min(max(j+l,1),d2)
              if(connected(p,q)) then
               ht=m(p,q)%height
               do a=-1,1,1
                do b=-1,1,1
                  u=min(max(p+a,1),d1)
                  v=min(max(q+b,1),d2)
                  if(m(u,v)%height.le.ht) connected(u,v)=.true.
                enddo
               enddo
               if(ht.lt.h0) then
                  h0=ht
                  xf=p
                  yf=q
               endif
              endif
             enddo
            enddo
            r=r+1
           enddo

           !Drain Into Cell Nearest the Drain Point
           s0=2.0d+00*r
           do k=-1,1,1
            do l=-1,1,1
               p=min(max(i+k,1),d1)
               q=min(max(j+l,1),d2)
               s=sqrt(1.0*(xf-p)**2. + 1.0*(yf-q)**2.)
               if(s.lt.s0) then
                  s0=s
                  m(i,j)%outflow_cell(1) = p
                  m(i,j)%outflow_cell(2) = q
               elseif(s.eq.s0) then
                  rwalk=rand()
                  if(rwalk.gt.0.5) then
                     m(i,j)%outflow_cell(1)=p
                     m(i,j)%outflow_cell(2)=q
                  endif
               endif
            enddo
           enddo
        endif
       enddo
      enddo
!$OMP END DO
      call prof_exit(3,tid)
      call prof_exit(n_max,tid)
!$OMP END PARALLEL
      call prof_exit(3,1)

      end subroutine
!=======================================================================
