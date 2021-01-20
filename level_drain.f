!=======================================================================
!     Flow Pathfinder: Level Path
!=======================================================================

      subroutine drain_path(m,x0,y0,dbg)

      use parameter_module
      use map_module
      use prof_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type),dimension(d1,d2) :: m
      integer :: x0,y0
      logical :: dbg
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
      integer :: h0,h,ht,hq     !Height Variables
      real :: g0,g              !Gradient Variables
      logical :: search         !Pathfinding Repeater Variable

      real :: rn                !Random Number for random walk
      integer :: cpti           !Seed Integer
      integer, dimension(3) :: cpt !system clock for seed

      integer :: i,j,k,l        !Indicies
      integer :: ct             !Counter
      integer, dimension(:), allocatable :: px,py !Paths
      integer :: dpx,dpy        !Debug Path Variables

!-----------------------------------------------------------------------
      if(m(x0,y0)%flow_solved) return
!.....Initialize........................................................
      call prof_enter(5,' LEVEL PATH FINDER: ')
      solved(:,:)=.false.
      activ(:,:)=.false.
      dist(:,:)=1000000000
      feeder(:,:,:)=0
      search = .true.
      call itime(cpt)
      cpti=mod(cpt(2)*cpt(3),10000)
      call srand(cpti)

!.....Set-Up Starting Node..............................................
      activ(x0,y0)=.true.
      dist(x0,y0)=0
      feeder(x0,y0,1)=x0
      feeder(x0,y0,2)=y0
      h0= m(x0,y0)%height
      h = m(x0,y0)%height
      g0= m(x0,y0)%grad
      g = g0
      ht= max(h0-1,0)
      x = x0
      nx= 0
      y = y0
      ny= 0
      l = 0

!.....Pathfinding Algorithm.............................................
      do while(search)
         l = l+1
!........Update Current Node's Neighbors................................
         solved(x,y)=.true.
         do i=-1,1,1
          do j=-1,1,1
            tx=min(max(x+i,1),d1)
            ty=min(max(y+j,1),d2)
            if(solved(tx,ty)) cycle
            if(m(tx,ty)%height.gt.h0) then
               solved(tx,ty)=.true.
            elseif(dist(tx,ty).gt.dist(x,y)+1) then
               dist(tx,ty)=dist(x,y)+1
               feeder(tx,ty,1)=x
               feeder(tx,ty,2)=y
               activ(tx,ty)=.true.
            endif
          enddo
         enddo
!........Find Nearest Unsolved Node Or Drain Point......................
         mindis=100000000
         hq = h !Query Height of Current Cell Candidate
         do i=1,d1
          do j=1,d2
            if(activ(i,j)) then
              if(solved(i,j)) cycle
              if(m(i,j)%height.lt.hq) then !Located a drain point
                hq=m(i,j)%height
                mindis=dist(i,j)
                g0=m(i,j)%grad
                nx=i
                ny=j
              elseif(m(i,j)%height.eq.hq) then!needed to stay @ drain pt
                if(dist(i,j).lt.mindis) then !closer that current cand?
                   mindis=dist(i,j)
                   g0=m(i,j)%grad
                   nx=i
                   ny=j
                elseif(dist(i,j).eq.mindis) then !Same height and dist
                   nx = 2*i-feeder(i,j,1)
                   ny = 2*j-feeder(i,j,2)
                   ! Sets flow to continue along the same path
!The Largest Slope doesn't include direction so chase mountains instead
!of flowing into valleys
!                   g=m(i,j)%grad
!                   if(g.gt.g0) then !Go for the largest slope
!                      g0=g
!                      nx=i
!                      ny=j
!                   else if (g.eq.g0) then !All things equal
!                      !Uncomment Lines To Move Else Stay
!                      nx=i
!                      ny=j
!                      !Move means moving from (-1,-1)->(1,1) gives
!                      !pririty to unsolved nodes at (1,1)
!                   endif
                endif
              endif
            endif
          enddo
         enddo !End Loop over all cells - current best node is new node
         x = nx
         y = ny
         h = m(x,y)%height
!........Error check to see if new node has previously been visited.....
         if(solved(nx,ny)) then
            !Print Error Message
            write(0,*) 'Level Drain Error for cell',x0,y0
            m(x0,y0)%d_cell(1)=x0
            m(x0,y0)%d_cell(2)=y0
            m(x0,y0)%flow_solved=.true.
            return
         endif
         if(h.le.ht) search=.false.
         if(m(x,y)%flow_solved) search=.false.
      enddo !End Pathfinding Algorithm
      xf=nx
      yf=ny
      l = l+1
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
!.....Path Debug Checks.................................................
      if((px(1).ne.x0).or.(py(1).ne.y0)) then
         write(*,*) 'Error Solved Path Does Not Start On Origin Point'
         write(*,*) 'Error Encountered For Cell',x0,y0
         stop
      endif
      do i=1,k-1
        if(.not.activ(px(i),py(i))) then
          write(*,*) 'Path Drains Through Non-Active Cell'
          write(*,*) 'Error Encountered For Cell',x0,y0
        endif
        dpx = abs(px(i+1)-px(i))
        dpy = abs(py(i+1)-py(i))
        if((dpx.gt.1).or.(dpy.gt.1)) then
          write(*,*) 'Path Makes A Multi-Cell Jump of',max(dpx,dpy)
          write(*,*) 'Error Encountered for Cell',x0,y0
        endif
      enddo
!.....Drain the Entire path.............................................
      do i=1,k-1
        m(px(i),py(i))%d_cell(1)=px(i+1)
        m(px(i),py(i))%d_cell(2)=py(i+1)
        m(px(i),py(i))%flow_solved=.true.
      enddo
      deallocate(px)
      deallocate(py)
      call prof_exit(5)

      end subroutine
!=======================================================================
