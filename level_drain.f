!=======================================================================
!     Flow Pathfinder: Level Path
!=======================================================================

      subroutine drain_path(m,x0,y0)

      use parameter_module
      use map_module
      use prof_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type),dimension(d1,d2) :: m
      integer :: x0,y0
!-----------------------------------------------------------------------
      
      integer, dimension(d1,d2) :: dist         !Distance from origin pt
      integer, dimension(d1,d2,2) :: feeder     !Prior Position in chain
      integer, dimension(9,2) :: cand           !Neighbor Storage
      logical, dimension(d1,d2) :: solved       !Prevent Revisiting
      logical, dimension(d1,d2) :: activ        !Faster cycling state

      integer :: x,y            !Coordinates of Current Node
      integer :: tx,ty          !Coordinates of Test Node
      integer :: nx,ny          !Coordinates of Best Candidate Node
      integer :: xf,yf          !Coordinates of Drain Node
      integer :: mindis         !Distance to Closest Unsolved Node
      integer :: h0,h,ht        !Height Variables
      real :: g0,g              !Gradient Variables

      real :: rn                !Random Number for random walk
      integer :: cpti           !Seed Integer
      integer, dimension(3) :: cpt !system clock for seed

      integer :: i,j,k,l        !Indicies
      integer :: ct             !Counter
      integer, dimension(:), allocatable :: px,py !Paths

!-----------------------------------------------------------------------
      if(m(x0,y0)%flow_solved) return
!.....Initialize........................................................
      call prof_enter(5,1,' LEVEL PATH FINDER: ')
      do i=1,d1
       do j=1,d2
         solved(i,j)=.false.
         activ(i,j)=.false.
         dist(i,j)=1000000000
         feeder(i,j,1)=0
         feeder(i,j,2)=0
       enddo
      enddo
      call itime(cpt)
      cpti=mod(cpt(2)*cpt(3),10000)
      call srand(cpti)

!.....Set-Up Starting Node..............................................
      activ(x0,y0)=.true.
      dist(x0,y0)=0
      feeder(x0,y0,1)=x0
      feeder(x0,y0,2)=y0
      h0= m(x0,y0)%height
      g0= m(x0,y0)%grad
      g = g0
      ht= max(h0-1,0)
      x = x0
      nx= x0
      y = y0
      ny= y0

!.....Pathfinding Algorithm.............................................
      do while(h.gt.ht)
!........Update Current Node's Neighbors................................
         solved(x,y)=.true.
         h=m(x,y)%height
         ct=0
         do i=-1,1,1
          do j=-1,1,1
            ct=ct+1
            cand(ct,1)=min(max(x+i,1),d1)
            cand(ct,2)=min(max(y+j,1),d2)
          enddo
         enddo
         do i=1,9
           if(i.eq.5) cycle
           tx=cand(i,1)
           ty=cand(i,2)
           if(solved(tx,ty)) cycle
           if(m(tx,ty)%height.gt.h) then
              solved(tx,ty)=.true.
           endif
           if(dist(tx,ty).gt.dist(x,y)+1) then
              dist(tx,ty)=dist(x,y)+1
              feeder(tx,ty,1)=x
              feeder(tx,ty,2)=y
              activ(tx,ty)=.true.
           endif
         enddo
!........Find Nearest Unsolved Node Or Drain Point......................
         mindis=100000000
         do i=1,d1
          do j=1,d2
            if(solved(i,j)) cycle
            if(activ(i,j)) then
              if(m(i,j)%height.lt.h) then !Located a drain point
                h=m(i,j)%height
                mindis=dist(i,j)
                g0=m(i,j)%grad
                nx=i
                ny=j
              elseif(m(i,j)%height.eq.h) then!needed to stay @ drain pt
                if(dist(i,j).lt.mindis) then !closer that current cand?
                   mindis=dist(i,j)
                   g0=m(i,j)%grad
                   nx=i
                   ny=j
                elseif(dist(i,j).eq.mindis) then !Same height and dist
                   g=m(i,j)%grad
                   if(g.gt.g0) then !Go for the largest slope
                      g0=g
                      nx=i
                      ny=j
                   else if (g.eq.g0) then !All things equal
                     rn=rand()
                     if(rn.gt.0.5) then !Random Step
                       nx=i
                       ny=j
                     endif
                   endif
                endif
              endif
            endif
          enddo
         enddo !End Loop over all cells - current best node is new node
!........Error check to see if new node has previously been visited.....
         if(solved(nx,ny)) then
            !Print Error Message
            write(0,*) 'Level Drain Error for cell',x0,y0
            m(x0,y0)%d_cell(1)=x0
            m(x0,y0)%d_cell(2)=y0
            return
         endif
         ! Update Current Node to Best Candidate
         x = nx
         y = ny
         ! If new node has been solved, then reached a known drain path
         if(m(x,y)%flow_solved) then
            h=ht !If joining river on same plain, aborts loop
         else
            h = m(x,y)%height
         endif
      enddo !End Pathfinding Algorithm
      xf=nx
      yf=ny
!.....Define the Shortest Path from (x0,y0) to (xf,yf)..................
      k=dist(xf,yf)+1
      allocate(px(k))
      allocate(py(k))
      do i=k,1,-1
         x = nx
         y = ny
         px(i) = x
         py(i) = y
         nx=feeder(x,y,1)
         ny=feeder(x,y,2)
      enddo
!.....Drain the Entire path.............................................
      do i=1,k-1
        !if(m(px(i),py(i))%flow_solved) cycle
        m(px(i),py(i))%d_cell(1)=px(i+1)
        m(px(i),py(i))%d_cell(2)=py(i+1)
        m(px(i),py(i))%flow_solved=.true.
      enddo
      deallocate(px)
      deallocate(py)
      call prof_exit(5,1)

      end subroutine
!=======================================================================
