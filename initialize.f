!=======================================================================
!     RIVER-PATHFINDER: DATA INPUT
!=======================================================================

      subroutine readin(m,mode)

      use parameter_module
      use prof_module
      use map_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      logical :: mode !True = Single File Import; False = 3-File
      integer :: i,j,a,b
      real :: pxf,ele
!-----------------------------------------------------------------------
      call prof_enter(1,'      DATA READ-IN: ')
      pxf = (804.672**2)*1.0d-03
      if(mode) then
        open(4,file="data-a.dat")
        do i=1,d1
         do j=1,d2
           read(4,*) a,b,m(i,j)%height,m(i,j)%rain,m(i,j)%temp
         enddo
        enddo
        close(4)
      else
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
        open(4,file="data-a.dat")
        do i=1,d1
         do j=1,d2
           write(4,*) i,j,m(i,j)%height,m(i,j)%rain,m(i,j)%temp
         enddo
        enddo
        close(4)
      endif

      do i=1,d1
       do j=1,d2
         ele = m(i,j)%height 
         if((i.gt.1).and.(j.gt.1).and.(i.lt.d1).and.(j.lt.d2)) then
           ele = (ele + 0.5*(m(i+1,j)%height + m(i-1,j)%height
     &         + m(i,j+1)%height + m(i,j-1)%height))/3.
         endif
         m(i,j)%elev = ele*25.5             !m
         m(i,j)%rain = m(i,j)%rain*pxf      !m^3/year
       enddo
      enddo

      call prof_exit(1)
      end subroutine
!=======================================================================
