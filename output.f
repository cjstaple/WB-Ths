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
        open(10,file=dname)
        write(10,11)
        write(10,12)
        do i=1,d1
         do j=1,d2
           write(10,13) i,j,m(i,j)%dep,m(i,j)%xA,m(i,j)%vol_s,
     &       m(i,j)%vel_out,m(i,j)%grad,m(i,j)%Ax,m(i,j)%v_o
         enddo
        enddo
        write(10,12)
        close(10)

        open(20,file=fname)
        write(20,21)
        write(20,22)
        do i=1,d1
         do j=1,d2
           write(20,23) i,j,m(i,j)%flow_frac,m(i,j)%c_dis,
     &        m(i,j)%c_acc,m(i,j)%outflow
         enddo
        enddo
        write(20,22)
        close(20)
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
11    format('  INDICIES  ','   AVG DEPTH ','  CROSS-AREA ',
     &       '  STEADY VOL ','   FLOW VEL  ','   GRADIENT  ',
     &       '   2ND AREA  ','    2ND VOL  ')
12    format(2('|-----'),7('|------------'))
13    format(2(1x,i5),7(1x,g12.5))


21    format('  INDICIES  ','  FLOW FRAC  ',' DESCENDANTS ',
     &       ' THROUGH-PUT ','  DISCHARGE  ')
22    format(2('|-----'),'|------------',2('|-----'),'|------------')
23    format(2(1x,i5),1x,g12.6,2(1x,i5),1x,g12.3)

      end subroutine
!=======================================================================
