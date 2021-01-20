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
      ncyc=0
      call prof_initial
      allocate(map(d1,d2))
      call readin(map,.false.)     !Read input data from files
      call prof_write              !Code Profiling
!.....Determine Base Map Properties.....................................
      call ocean_search(map)       !Identify Ocean Cells
      call grad(map)               !Calculate Local Cell Gradients

!.....Cell-To-Cell Flow.................................................
      call drop_search(map)        !Look for Cells with Lower Neighbors
!      call elev_drop_search(map)   !^^^ But uses Elevation instead of Ht
      call flow_out(map)           !Outputs Data
!.....Calculate Flow Paths across level plains..........................
      do i=1,d1
       do j=1,d2
         ncyc=ncyc+1
         if(map(i,j)%ocean) cycle
         call prof_write
         call drain_path(map,i,j,.false.)
!         call elev_drain_path(map,i,j,.false.)
         if(ncyc.gt.5000) then
           ncyc=0
           call psolved(map)
         endif
         a = map(i,j)%d_cell(1)
         b = map(i,j)%d_cell(2)
         if((a.eq.0).or.(b.eq.0)) then
           map(i,j)%flow_solved=.false.
         endif
       enddo
      enddo
      ncyc=0
      do i=1,d1
       do j=1,d2
         ncyc=ncyc+1
         if(map(i,j)%ocean) cycle
         call prof_write
         call drain_path(map,i,j,.true.)
!         call elev_drain_path(map,i,j,.true.)
         if(ncyc.gt.5000) then
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
      call grad_drain(map) !Drains Map based on Gradient Calculation
      call flow_rate(map) !Calculate Steady-State Volumes
      call flow_out(map) !Outputs Data
      call prof_write
      deallocate(map)

      end program
!=======================================================================
