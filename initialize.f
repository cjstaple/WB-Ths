!=======================================================================
!     RIVER-PATHFINDER: DATA INPUT
!=======================================================================

      subroutine readin(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer :: i,j,a,b
      real :: pxf
!-----------------------------------------------------------------------
      call prof_enter(1,1,'      DATA READ-IN: ')
      open(1,file="htab-a.dat")
      open(2,file="rtab-a.dat")
      open(3,file="ttab-a.dat")
      do i=1,d1
       do j=1,d2
         read(1,*) a,b, m(i,j)%height !pxls
         read(2,*) a,b, m(i,j)%rain   !mm/year
         read(3,*) a,b, m(i,j)%temp   !K
       enddo
      enddo
      close(1)
      close(2)
      close(3)

      pxf = (804.672**2)/1000.
      do i=1,d1
       do j=1,d2
         m(i,j)%elev = m(i,j)%height*25.1   !m
         m(i,j)%rain = m(i,j)%rain*pxf      !m^3/year
       enddo
      enddo

      call prof_exit(1,1)
      end subroutine
!=======================================================================