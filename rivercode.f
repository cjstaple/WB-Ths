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
      integer :: nsol,ncyc

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
      call prof_write !Code Profiling
      nsol=0
      ncyc=0

      call ocean_search(map) !Search for the Ocean Cells
      call frac_calc(map) !Calculate fraction of water that flows out

      call drop_search(map) !Calculate Flows along slope edges
!.....Determine Fraction of Map That has been solved....................
      do i=1,d1
       do j=1,d2
         if(map(i,j)%flow_solved) nsol=nsol+1
       enddo
      enddo
      psol=1.0d+00*nsol/(d1*d2)
      call flow_out(map) !Outputs Data

!.....Calculate Flow Paths across level plains..........................
      do i=1,d1
       do j=1,d2
         ncyc=ncyc+1
         if(map(i,j)%flow_solved) cycle
         call prof_write
         call drain_path(map,i,j)
         if(ncyc.gt.50000) then
           ncyc=0
           nsol=0
           do a=1,d1
            do b=1,d2
              if(map(a,b)%flow_solved) nsol=nsol+1
            enddo
           enddo
           psol=1.0d+00*nsol/(d1*d2)
         endif
       enddo
      enddo
      call flow_out(map) !Outputs Data

      call cell_flow(map) !Calculates Annual Flow Throughput
      call flow_out(map) !Outputs Data

      call grad(map) !Calculates Local Gradient and computes output rate
      call flow_rate(map) !Calculate Steady-State
      call flow_out(map) !Outputs Data

      call prof_write
      deallocate(map)

      end program
!=======================================================================
