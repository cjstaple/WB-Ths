!=======================================================================
!     RIVER-PATHFINDER STRUCTURAL PROGRAM
!=======================================================================

      program rivercode

      use parameter_module
      use prof_module
      use map_module

      implicit none

!-----Local Variable Definitions----------------------------------------

      type(map_type), dimension(d1,d2) :: map
      integer :: i,j,k,a,b
      character(20) :: fname

!-----Import Map Data---------------------------------------------------
      call prof_initial
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

      do i=1,d1
       do j=1,d2
         if(map(i,j)%flow_solved) cycle
         call prof_write
         call drain_connect(map,i,j)
       enddo
      enddo
      write(*,*) 'Flow Cells Calculated'

      call cell_flow(map)
      call prof_write
      write(*,*) 'Cells Drained'

      call flow_out(map)

      call prof_write
      end program
!=======================================================================
