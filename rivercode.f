!=======================================================================
!     RIVER-PATHFINDER STRUCTURAL PROGRAM
!=======================================================================

      program rivercode

      use parameter_module
      use prof_module
      use map_module

      implicit none

!-----Local Variable Definitions----------------------------------------

      type(map_type), dimension(:,:),allocatable :: map
      integer :: i,j,k,a,b
      character(20) :: fname

!-----Import Map Data---------------------------------------------------
      call prof_initial
      allocate(map(d1,d2))

      call prof_enter(1,1,'      DATA READ-IN: ')
      open(1,file="htab-a.dat")
      open(2,file="rtab-a.dat")
      open(3,file="ttab-a.dat")
      do i=1,d1
       do j=1,d2
        read(1,*) a, b, map(i,j)%height !pxls
        read(2,*) a, b, map(i,j)%rain   !mm/year
        read(3,*) a, b, map(i,j)%temp   !K
       enddo
      enddo
      close(1)
      close(2)
      close(3)
      call prof_exit(1,1)
      write(*,*) 'Data Read Complete'

      do i=1,d1
       do j=1,d2
        if(map(i,j)%height.eq.0) then
           map(i,j)%ocean=.true.
           map(i,j)%flow_solved=.false.
        else
           map(i,j)%ocean=.false.
           map(i,j)%flow_solved=.true.
        endif
       enddo
      enddo
      write(*,*) 'Ocean Solved'

      call cell_sink(map)
      call prof_write
      write(*,*) 'Sink Calculated'

      call cell_drain(map)
      call prof_write
      write(*,*) 'Flow Cells Calculated'

      call cell_flow(map)
      call prof_write
      write(*,*) 'Cells Drained'

      call prof_enter(n_max-1,1,'      WRITE OUTPUT: ')
      fname="flow-a.dat"
      open(10,file=fname)
      do i=1,d1
       do j=1,d2
        write(10,11) i,j,map(i,j)%f_length, map(i,j)%outflow,
     &    map(i,j)%outflow_cell(1), map(i,j)%outflow_cell(2)
       enddo
      enddo
      close(10)
      call prof_exit(n_max-1,1)
11    format(2i5,i6,f18.6,2i5)

      deallocate(map)
      call prof_write
      end program
!=======================================================================
