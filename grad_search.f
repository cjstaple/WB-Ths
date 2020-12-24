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
      real :: dzdx,dzdy,dzdr
      real :: mu, qo, rout
!-----------------------------------------------------------------------
      call prof_enter(8,1,'    LOCAL GRADIENT: ')
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
           dzx=8*abs(m(ip,j)%height-m(im,j)%height)+
     &            abs(m(ipp,j)%height-m(imm,j)%height)
           dzy=8*abs(m(i,jp)%height-m(i,jm)%height)+
     &            abs(m(i,jpp)%height-m(i,jmm)%height)
           !Sum new term into the local gradient
           dzdx = dzdx + (3.1d-02*dzx/(12.*r))
           dzdy = dzdy + (3.1d-02*dzy/(12.*r))
           !Check if slope is detected to end loop
           if((dzdx.gt.0.).or.(dzdy.gt.0.)) rep=.false.
         enddo
!........Error Check - Make sure loop didn't exit with a 0 gradient.....
         if((dzdx.eq.0.).and.(dzdy.eq.0.)) then
           write(*,*) 'Gradient Search Error'
           write(*,*) ' i  = ', i
           write(*,*) ' j  = ', j
           write(*,*) ' r  = ', r
           write(*,*) 'dzx = ', dzx
           write(*,*) 'dzy = ', dzy
           write(*,*) 'im  = ', im
           write(*,*) 'ip  = ', ip
           write(*,*) 'jm  = ', jm
           write(*,*) 'jp  = ', jp
           stop
         endif
!........Set the output variables.......................................
         dzdr = sqrt(dzdx**2 + dzdy**2)
         ! v = sqrt(L*dzdr)
         rout = sqrt((9.81/804.672)*dzdr)
         m(i,j)%grad = dzdr
         m(i,j)%out_rate = rout
       enddo
      enddo
      call prof_exit(8,1)
      call prof_write

      return
      end subroutine
!=======================================================================
