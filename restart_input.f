!=======================================================================
!     Flow-Pathfinder: Restart Input File
!=======================================================================

      subroutine check_restart(m,restart)

      use parameter_module
      use prof_module
      use map_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
      integer :: i,j,k,l,u,v,a,b,dumi
      real :: dumr
      character(20) :: dname,fname,vname
      logical :: dtrue,ftrue,vtrue,duml
      logical,intent(out) :: restart
      logical :: debug = .true.
!-----------------------------------------------------------------------
      call prof_enter(n_max-2,1,' RESTART READ DATA: ')
      fname = "output/flow-a.dat"
      vname = "output/vector-a.dat"
      inquire(file=fname,exist=ftrue)
      inquire(file=vname,exist=vtrue)
      restart = .false.

!.....Read in Flow Data.................................................     
      if(ftrue) then
         restart = .true.
         open(20,file=fname)
         read(20,*)
         read(20,*)
         do i=1,d1
          do j=1,d2
            read(20,23) u,v,duml,m(u,v)%flow_solved,dumr,dumi,dumi
          enddo
         enddo
         close(20)
      endif

!.....Read in Vector Data...............................................
      if(vtrue) then
         restart = .true.
         open(30,file=vname)
         read(30,*)
         read(30,*)
         do i=1,d1
          do j=1,d2
            read(30,33) k,l,u,v,a,b
            m(k,l)%d_cell(1)=u
            m(k,l)%d_cell(2)=v
          enddo
         enddo
      endif

      call prof_exit(n_max-2,1)
23    format(2(1x,i5),1x,L6,1x,L7,2(1x,g12.3),1x,i8)
33    format(6(1x,i5))
      
      end subroutine
!=======================================================================
