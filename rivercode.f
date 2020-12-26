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
      integer :: i,j,k,a,b,ncyc
      logical :: restart

!-----Import Map Data---------------------------------------------------
      call prof_initial
      allocate(map(d1,d2))
      call readin(map) !Read input data from files
      call prof_write !Code Profiling
      ncyc=0

      call ocean_search(map) !Search for the Ocean Cells
      call frac_calc(map) !Calculate fraction of water that flows out
      call grad(map) !Calculates Local Cell Gradients \& Flow Rates
!      call check_restart(map,restart) ! Read in Path data if it exists

      call drop_search(map) !Calculate Flows along slope edges
!.....Determine Fraction of Map That has been solved....................
      call psolved(map)
      call flow_out(map) !Outputs Data

!.....Calculate Flow Paths across level plains..........................
      do i=1,d1
       do j=1,d2
         ncyc=ncyc+1
         if(map(i,j)%ocean) cycle
         call prof_write
         call drain_path(map,i,j)
         if(ncyc.gt.10000) then
           ncyc=0
           call psolved(map)
         endif
       enddo
      enddo
      do i=1,d1
       do j=1,d2
         if((map(i,j)%d_cell(1).eq.0).or.(map(i,j)%d_cell(2).eq.0)) then
           map(i,j)%flow_solved=.false.
         endif
       enddo
      enddo
      do i=1,d1
       do j=1,d2
         ncyc=ncyc+1
         if(map(i,j)%ocean) cycle
         call prof_write
         call drain_path(map,i,j)
         if(ncyc.gt.10000) then
           ncyc=0
           call psolved(map)
         endif
       enddo
      enddo
      call psolved(map)
      call flow_out(map) !Outputs Data
      call prof_write

!.....Calculate Flow Rates and Steady State Volumes.....................
      call cell_flow(map) !Calculates Annual Flow Throughput
      call flow_rate(map) !Calculate Steady-State Volumes
      call flow_out(map) !Outputs Data

!.....Terminate Program.................................................
      call prof_write
      deallocate(map)

      end program
!=======================================================================
