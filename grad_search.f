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
      real :: norm, mu, rout
!-----------------------------------------------------------------------

      call prof_enter(8,1,'    LOCAL GRADIENT: ')
      do i=1,d1
       do j=1,d2
         if(m(i,j)%ocean) cycle
         r=0
         dzx = 0
         dzy = 0
         rep = .true.
         dzdx = 0.
         dzdy = 0.
         do while(rep)
           r=r+1
           r2=2*r
           im=min(max(i-r,1),d1)
           imm=min(max(i-r2,1),d1)
           ip=min(max(i+r,1),d1)
           ipp=min(max(i+r2,1),d1)
           jm=min(max(j-r,1),d2)
           jmm=min(max(j-r2,1),d2)
           jp=min(max(j+r,1),d2)
           jpp=min(max(j+r2,1),d2)
           dzx=8*abs(m(ip,j)%height-m(im,j)%height)+
     &            abs(m(ipp,j)%height-m(imm,j)%height)
           dzy=8*abs(m(i,jp)%height-m(i,jm)%height)
     &            abs(m(i,jpp)%height-m(i,jmm)%height)
           dzdx = dzdx + 1.0d+00*dzx/(12.*r)
           dzdy = dzdy + 1.0d+00*dzy/(12.*r)
           if((dzdx.gt.0.).or.(dzdy.gt.0.)) rep=.false.
         enddo
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
         if((dzdx.eq.0.).or.(dzdy.eq.0.)) then
            dzdr = max(dzdx,dzdy)
            rout = dzdr/(1+dzdr)
         else
            dzdr = dzdx*dzdy
            norm = sqrt(dzdx**2. + dzdy**2.)
            rout = dzdr/(norm+dzdr)
         endif
         m(i,j)%out_rate = rout
       enddo
      enddo
      call prof_exit(8,1)
      call prof_write

      return
      end subroutine
!=======================================================================
