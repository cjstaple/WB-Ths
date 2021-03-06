!=======================================================================
!     Flow-Pathfinder: Local Gradient Calculator
!=======================================================================

      subroutine grad(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none

!-----------------------------------------------------------------------
      type(map_type),dimension(d1,d2) :: m
      integer :: i,j,k,l,r,r2
      integer :: dzx,dzy,im,ip,jm,jp,imm,ipp,jmm,jpp
      logical :: rep
      real :: dzdx,dzdy,dzdr,rout,gcons
      integer :: nx,ny
!-----------------------------------------------------------------------
      call prof_enter(8,'    LOCAL GRADIENT: ')
      gcons = hpx/Lpx
      do i=1,d1
       do j=1,d2
       if(m(i,j)%ocean) cycle  ! Don't calculate gradient if ocean cell
!........Initialize the Gradient Loop...................................
         r=0
         dzx = 0
         dzy = 0
         rep = .true.
         dzdx = 0.
         dzdy = 0.
!........Determine the Local Cell Gradient..............................
         do while(rep)
           r=r+1       !Increment Gradient Radius
           r2=2*r      !Higher Order Gradient Radius
           im=min(max(i-r,1),d1)     ! i-1
           imm=min(max(i-r2,1),d1)   ! i-2
           ip=min(max(i+r,1),d1)     ! i+1
           ipp=min(max(i+r2,1),d1)   ! i+2
           jm=min(max(j-r,1),d2)     ! j-1
           jmm=min(max(j-r2,1),d2)   ! j-2
           jp=min(max(j+r,1),d2)     ! j+1
           jpp=min(max(j+r2,1),d2)   ! j+2
           !Calculate Gradient Numerator
           !f'(x) ~ ((8*(h(x+r)-h(x-r))+(h(x+2r)-h(x-2r)))/(12*r))
           dzx=8*m(ip,j)%height-m(im,j)%height+
     &          m(ipp,j)%height-m(imm,j)%height
           dzy=8*m(i,jp)%height-m(i,jm)%height+
     &          m(i,jpp)%height-m(i,jmm)%height
           !Sum new term into the local gradient
           dzdx = dzdx + (gcons*dzx/(12.*r))
           dzdy = dzdy + (gcons*dzy/(12.*r))
           !Check if slope is detected to end loop
           if((abs(dzdx).gt.0.).or.(abs(dzdy).gt.0.)) rep=.false.
         enddo
!........Set the output variables.......................................
         dzdr = sqrt(dzdx**2 + dzdy**2)
         ! v = sqrt(L*dzdr)
         rout = sqrt((9.81/804.672)*dzdr)
         m(i,j)%grad = dzdr
         m(i,j)%out_rate = rout
         m(i,j)%gvec(1) = (dzdx/dzdr)
         m(i,j)%gvec(2) = (dzdy/dzdr)
       enddo
      enddo
!.....Set up initial drain guess based on gradient vector...............
      do i=1,d1
       do j=1,d2
         nx = i+nint(m(i,j)%gvec(1))
         ny = j+nint(m(i,j)%gvec(2))
         if(m(nx,ny)%height.le.m(i,j)%height) then
           m(i,j)%g_cell(1)=nx
           m(i,j)%g_cell(2)=ny
         else
           m(i,j)%g_cell(1)=0
           m(i,j)%g_cell(2)=0
         endif
       enddo
      enddo
      call prof_exit(8)
      call prof_write

      return
      end subroutine
!=======================================================================
