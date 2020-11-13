!=======================================================================
!     Flow-Pathfinder: Map Level Module
!=======================================================================

      module map_module

      use parameter_module

      implicit none

      type map_type
              integer :: height !pxl
              integer :: rain   !mm/year
              integer :: temp   !K
              real :: flow_frac
              real :: outflow
              integer :: f_length
              integer, dimension(2) :: outflow_cell
      end type

!-----Layer Functions---------------------------------------------------


      end module map_module
