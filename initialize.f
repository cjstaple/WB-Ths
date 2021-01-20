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
      real :: pxf,ele,mele,tav,fl
!-----------------------------------------------------------------------
      call prof_enter(1,'      DATA READ-IN: ')
      pxf = (Lpx**2)*1.0d-03
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
!.....Get Rain (Source) into the Correct Units..........................
      do i=1,d1
       do j=1,d2
         tav = 1.0d+00*m(i,j)%temp-2.900d+02
         fl = 6.43d+01-(7.3513d-04)*tav
         m(i,j)%rain=m(i,j)%rain*fl*pxf   !m^3/year
       enddo
      enddo
!.....Define Cell Elevation based on pxl heights........................
      m(1,:)%elev = m(1,:)%height*hpx
      m(d1,:)%elev= m(d1,:)%height*hpx
      m(:,1)%elev = m(:,1)%height*hpx
      m(:,d2)%elev= m(:,d2)%height*hpx
      do i=2,d1-1
       do j=2,d2-1
         mele=min(m(i,j)%height,m(i+1,j)%height,m(i-1,j)%height,
     &    m(i,j+1)%height,m(i,j-1)%height)
         ele = (m(i,j)%height + 0.5*(m(i+1,j)%height + m(i-1,j)%height)
     &         + 0.5*(m(i,j+1)%height + m(i,j-1)%height))/3.
         m(i,j)%elev = hpx*max(mele,ele)   !m
       enddo
      enddo

      call prof_exit(1)
      end subroutine
!=======================================================================
