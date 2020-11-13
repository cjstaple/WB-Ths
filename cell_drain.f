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

      integer :: i,j,k,l        !Loop Indices
      integer :: p,q,xf,yf      !Coordinates
      integer :: r              !Search Range
      real :: s,s0              !Cell Distance
      integer :: h0             !Initial Cell Height
      integer :: ht             !Test Cell Height

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
!$OMP CRITICAL
        call prof_write
!$OMP END CRITICAL
        r=1
        h0=m(i,j)%height
        if(h0.eq.0) then
           m(i,j)%outflow_cell(1)=i
           m(i,j)%outflow_cell(2)=j
        else
           call prof_enter(4,tid,'DRAIN POINT SEARCH: ')
           do while(h0.eq.m(i,j)%height)
            do k=-r,r,1
             do l=-r,r,1
              p=min(max(i+k,1),d1)
              q=min(max(j+l,1),d2)
              ht=m(p,q)%height
              if(ht.lt.h0) then
                 h0=ht
                 xf=p
                 yf=q
              endif
             enddo
            enddo
            r=r+1
           enddo
           call prof_exit(4,tid)

           call prof_enter(5,tid,'  DIRECTION SEARCH: ')
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
               endif
            enddo
           enddo
           call prof_exit(5,tid)
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
