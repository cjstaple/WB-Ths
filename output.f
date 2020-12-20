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
      character(20) :: dname,fname,vname
      logical :: debug = .true.
!-----------------------------------------------------------------------
      call prof_enter(n_max-1,1,'      WRITE OUTPUT: ')
      if(debug) then
        dname="output/state-a.dat"
        fname="output/flow-a.dat"
        vname="output/vector-a.dat"
        open(10,file=dname)
        write(10,11)
        write(10,12)
        do i=1,d1
         do j=1,d2
           write(10,13) i,j,m(i,j)%out_rate,m(i,j)%vol_0,m(i,j)%dep
         enddo
        enddo
        write(10,12)
        close(10)

        open(20,file=fname)
        write(20,21)
        write(20,22)
        do i=1,d1
         do j=1,d2
           write(20,23) i,j,m(i,j)%ocean,m(i,j)%flow_solved,
     &        m(i,j)%flow_frac,m(i,j)%outflow,m(i,j)%f_length
         enddo
        enddo
        write(20,22)
        close(20)

        open(30,file=vname)
        write(30,31)
        write(30,32)
        do i=1,d1
         do j=1,d2
           u=m(i,j)%outflow_cell(1)
           v=m(i,j)%outflow_cell(2)
           write(30,33) i,j,u,v,u-i,v-j
         enddo
        enddo
        write(30,32)
        close(30)
      else
        open(20,file=fname)
        write(20,21)
        write(20,22)
        do i=1,d1
         do j=1,d2
           write(20,23) i,j,m(i,j)%ocean,m(i,j)%flow_solved,
     &        m(i,j)%flow_frac,m(i,j)%outflow,m(i,j)%f_length
         enddo
        enddo
        write(20,22)
        close(20)
      endif

      call prof_exit(n_max-1,1)
11    format('  INDICIES  ','  RATE  ','  VOLUME  ','  RDEPTH  ')
12    format(2('|-----'),'|-------',2('|---------'))
13    format(2(1x,i5),1x,f7.5,2(1x,g9.2))
21    format('  INDICIES  ',' OCEAN ',' SOLVED ','    % FLOW   ',
     &   '   OUTFLOW   ',' FLOW LENGTH ')
22    format(2('|-----'),'|------','|-------','|------------',
     &   '|------------','|------------')
23    format(2(1x,i5),1x,L6,1x,L7,2(1x,g12.3),1x,i8)
31    format('  INDICIES  ','   OUTFLOW  ','   VECTOR   ')
32    format(6('|-----'))
33    format(6(1x,i5))

      end subroutine
