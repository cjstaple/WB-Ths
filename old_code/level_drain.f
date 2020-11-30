!=======================================================================
!     LEVEL DRAIN DIRECTION
!=======================================================================

      subroutine level_drain(m,x0,y0)

      use parameter_module
      use map_module

      implicit none

      type(map_type), dimension(d1,d2) :: m  !Map
      integer :: x0, y0   ! Initial Coordinates

!-----Local Variables---------------------------------------------------

      integer, dimension(d1,d2) :: dist  !Path Distance from Start Point
      logical, dimension(d1,d2) :: active! True If <= height(x0,y0)
      logical, dimension(d1,d2) :: solved! True if nodes been visited
      integer, dimension(d1,d2,2) :: prev! Feeder Node
      integer, dimension(4,2) :: cand    ! Possible Candidates

      integer :: x,y                     ! Current Node Coordinates
      integer :: tx, ty                  ! Test Node Coordinates
      integer :: nx, ny                  ! Next Node's Coordinates
      integer :: xf,yf                   ! Final Node Coordinates
      integer :: h0,h                    ! Node Height Variables
      integer :: mindis                  ! Current Min Dist from Start

      integer :: i,j,k                   ! Cycle Indicies
      real :: d, d0                      ! Distance to nearest discharge
      
!-----Initialize Arrays-------------------------------------------------
      h0=m(x0,y0)%height
      do i=1,d1
       do j=1,d2
          dist(i,j) = 1000000
          prev(i,j,1) = 1
          prev(i,j,2) = 2
          if(m(i,j)%height.gt.h0) then
             active(i,j) = .false.
             solved(i,j) = .true.
          else
             active(i,j) = .true.
             solved(i,j) = .false.
          endif
       enddo
      enddo

!-----Initialize Starting Node------------------------------------------
      dist(x0,y0) = 0
      prev(x0,y0,1) = x0
      prev(x0,y0,2) = y0
      h = h0

      x=x0
      nx=x0
      y=y0
      ny=y0

      if(h0.eq.0) then
         m(x0,y0)%outflow_cell(1)=x0
         m(x0,y0)%outflow_cell(2)=y0
         return
      endif

!-----Pathfinding Algorithm---------------------------------------------
      do while(h.eq.h0)
!......Update The Current Node..........................................
       solved(x,y) = .true.
       cand(1,1)=min(x+1,d1)
       cand(1,2)=y
       cand(2,1)=x
       cand(2,2)=min(y+1,d2)
       cand(3,1)=max(x-1,1)
       cand(3,2)=y
       cand(4,1)=x
       cand(4,2)=max(y-1,1)
       do i=1,4
          tx=cand(i,1)
          ty=cand(i,2)
          if(solved(tx,ty)) cycle
          if(dist(tx,ty).gt.dist(x,y)+1) then
             dist(tx,ty)=dist(x,y)+1
             prev(tx,ty,1)=x
             prev(tx,ty,2)=y
          endif
       enddo

!......Select New Node..................................................
       mindis=dist(x,y)+1
       do i=1,d1
        do j=1,d2
           if(active(i,j).and..not.solved(i,j)) then
            if(dist(i,j).lt.mindis) then
               mindis=dist(i,j)
               nx=i
               ny=j
            elseif(dist(i,j).eq.mindis) then
               nx=i
               ny=j
            endif
           endif
        enddo
       enddo
       if((nx.eq.x).and.(ny.eq.y)) then
         h=m(1,1)%height
         nx=1
         ny=1
       else
         h=m(nx,ny)%height
         x=nx
         y=nx
       endif
      enddo
      xf=nx
      yf=ny

!.....Find Closest Discharge Cell.......................................
      cand(1,1)=min(x0+1,d1)
      cand(1,2)=y0
      cand(2,1)=x0
      cand(2,2)=min(y0+1,d2)
      cand(3,1)=max(x0-1,1)
      cand(3,2)=y0
      cand(4,1)=x0
      cand(4,2)=max(y0-1,1)
      d0=1.00d+06
      do i=1,4
         d = sqrt(1.0*(cand(i,1)-xf)**2. + 1.0*(cand(i,2)-yf)**2.)
         if(d.lt.d0) then
          k=i
          d0=d
         endif
      enddo
      m(x0,y0)%outflow_cell(1)=cand(k,1)
      m(x0,y0)%outflow_cell(2)=cand(k,2)

      return
      end subroutine
!=======================================================================

