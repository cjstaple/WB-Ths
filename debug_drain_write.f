!=======================================================================
!     Flow-Pathfinder: Level Drain Debug Info Dump
!=======================================================================

      subroutine drain_write(m,x0,y0,dist,solved,activ,ln)

      use parameter_module
      use map_module
      use prof_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer :: x0,y0,ln
      integer, dimension(d1,d2) :: dist
      logical, dimension(d1,d2) :: solved, activ

      character(50) :: fname
      character(4) :: ext,num,xq,yq
!-----------------------------------------------------------------------
      integer :: i,j,k
!-----------------------------------------------------------------------
!.....Create File.......................................................
      ext = '.dat'
      num= char(48+mod(ln/10000,10))//char(48+mod(ln/1000,10))//
     &     char(48+mod(ln/100,10))//char(48+mod(ln/10,10))
      xq = char(48+mod(x0/1000,10))//char(48+mod(x0/100,10))//
     &     char(48+mod(x0/10,10))//char(48+mod(x0,10))
      yq = char(48+mod(y0/1000,10))//char(48+mod(y0/100,10))//
     &     char(48+mod(y0/10,10))//char(48+mod(y0,10))
      fname=trim('output/Drain_')//trim(xq)//'-'//trim(yq)//
     &      trim('_step_')//trim(num)//ext

!.....Output Data......................................................      
      open(1,file=fname,status='unknown')
      write(1,*) repeat("=",72)
      write(1,11) ln
      write(1,*) ' '
      write(1,*) repeat("=",72)
      write(1,*) ' '
      write(1,12) '  INDICES ','   OCEAN  ','  FLOW_S  ',
     &            '   DIST   ','  ACTIVE  ','  SOLVED  '
      write(1,13)
      do i=1,d1
       do j=1,d2
         write(1,15) i,j,m(i,j)%ocean,m(i,j)%flow_solved,
     &      dist(i,j), activ(i,j), solved(i,j)
       enddo
      enddo
      write(1,13)
      close(1)

!.....Format Statements.................................................
11    format(1x,'Current Step Numer:',t25,i5)
12    format(1x,3a10,1x,a10,1x,2a10)
13    format(1x,2('|----'),2('|---------'),'|-----------',
     &    2('|---------'))
15    format(1x,2i5,2L10,i12,2L10)

      end subroutine
