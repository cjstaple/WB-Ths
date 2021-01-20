!=======================================================================
!     Flow-Pathfinder: Output Function
!=======================================================================

      subroutine flow_out(m)

      use parameter_module
      use prof_module
      use map_module

      implicit none
!-----------------------------------------------------------------------
      type(map_type), dimension(d1,d2) :: m
!.......................................................................
      integer :: i,j,k,u,v
      character(20) :: dname,fname,vname,gname
      logical,save :: rep = .true.
!-----------------------------------------------------------------------
      call prof_enter(n_max-1,'      WRITE OUTPUT: ')
      if(rep) then
        rep = .false.
        open(50,file="output/elev-a.dat")
        write(50,51)
        write(50,52)
        do i=1,d1
         do j=1,d2
          write(50,53) i,j,m(i,j)%height,m(i,j)%elev
         enddo
        enddo
        write(50,52)
        close(50)
      endif
      dname="output/state-a.dat"
      fname="output/flow-a.dat"
      gname="output/grad-a.dat"
      vname="output/vec-a.dat"
      open(10,file=dname)
      open(20,file=fname)
      open(30,file=vname)
      open(40,file=gname)
      write(10,11)
      write(20,21)
      write(30,31)
      write(40,41)
      write(10,12)
      write(20,22)
      write(30,32)
      write(40,42)
      do i=1,d1
       do j=1,d2
        write(10,13) i,j,m(i,j)%depth,m(i,j)%xA,m(i,j)%vol,m(i,j)%flow_v
        write(20,23) i,j,m(i,j)%c_dis,m(i,j)%c_acc,m(i,j)%outflow
        write(30,33) i,j,m(i,j)%grad,m(i,j)%g_cell(1),m(i,j)%g_cell(2),
     &     m(i,j)%d_cell(1),m(i,j)%d_cell(2)
        write(40,43) i,j,m(i,j)%g_dis,m(i,j)%g_acc,m(i,j)%gradflow
       enddo
      enddo
      write(10,12)
      write(20,22)
      write(30,32)
      write(40,42)
      close(10)
      close(20)
      close(30)
      close(40)

      call prof_exit(n_max-1)
11    format('  INDICIES  ','   AVG DEPTH ','  CROSS-AREA ',
     &       '  STEADY VOL ','   FLOW VEL  ')
12    format(2('|-----'),4('|------------'))
13    format(2(1x,i5),4(1x,g12.5))


21    format('  INDICIES  ','   N_DN   ','   N_UP   ',
     &       '  DISCHARGE  ')
22    format(2('|-----'),'|------------',2('|---------'),
     &       '|------------')
23    format(2(1x,i5),2(1x,i9),1x,g12.3)

31    format('  INDICIES  ','  LOCAL GRAD ','  GRAD DEST ',
     &   ' FLOW DEST  ')
32    format(2('|-----'),'|------------',4('|-----'))
33    format(2(1x,i5),1x,g12.5,4(1x,i5))

41    format('  INDICIES  ','   N_DN   ','   N_UP   ',
     &       '  DISCHARGE  ')
42    format(2('|-----'),'|------------',2('|---------'),
     &       '|------------')
43    format(2(1x,i5),2(1x,i9),1x,g12.3)

51    format('  INDICIES  ','HEIGHT','ELEVATION')
52    format('---------------------------------')
53    format(3(1x,i5),(1x,g9.5))

      end subroutine
!=======================================================================
