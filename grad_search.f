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
      integer :: i,j,k,l,r
      integer :: dzx,dzy,im,ip,jm,jp
      logical :: rep,adj
      real :: dzdx,dzdy,dzdr
      real :: norm, mu, rout
!-----------------------------------------------------------------------

      call prof_enter(8,1,'    LOCAL GRADIENT: ')
      do i=1,d1
       do j=1,d2
         r=1
         dzx = 0
         dxy = 0
         rep = .true.
         adj = .true.
         dzdx = 0.
         dzdy = 0.
         do while(rep)
           im=min(max(i-r,1),d1)
           ip=min(max(i+r,1),d1)
           jm=min(max(j-r,1),d2)
           jp=min(max(j+r,1),d2)
           dzx=abs(m(ip,j)%height-m(im,j)%height)
           dzy=abs(m(i,jp)%height-m(i,jm)%height)
           if((dzx.gt.0).or.(dzy.gt.0)) then
             if(adj) then
               adj=.false.
             else
               rep=.false.
             endif
           endif
           r = r+1
           dzdx = dzdx + 1.0d+00*dzx/(ip-im)
           dzdy = dzdy + 1.0d+00*dzy/(jp-jm)
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

      return
      end subroutine
!=======================================================================
