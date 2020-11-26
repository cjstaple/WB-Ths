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
      call prof_write

      call prof_enter(2,1,'       OCEAN CHECK: ')
      do i=1,d1
       do j=1,d2
        if(map(i,j)%height.eq.0) then
           map(i,j)%ocean=.true.
           map(i,j)%flow_solved=.true.
           map(i,j)%flow_frac=0.
           map(i,j)%outflow=0.
           map(i,j)%f_length=0
           map(i,j)%outflow_cell(1)=i
           map(i,j)%outflow_cell(2)=j
        else
           map(i,j)%ocean=.false.
           map(i,j)%flow_solved=.false.
           map(i,j)%flow_frac=1.
           map(i,j)%outflow=0.
           map(i,j)%f_length=0
           map(i,j)%outflow_cell(1)=d1
           map(i,j)%outflow_cell(2)=d2
        endif
       enddo
      enddo
      call prof_exit(2,1)
      call prof_write

!      call cell_sink(map)

!      do i=1,d1
!       do j=1,d2
!         if(map(i,j)%flow_solved) cycle
!         call prof_write
!         call drain_connect(map,i,j)
!       enddo
!      enddo

!      call cell_flow(map)
!      call prof_write

      call flow_out(map)

      call prof_write
      deallocate(map)
      end program
!=======================================================================
