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
         real :: flow_frac !Water - Freeze - Evaporate
         real :: infow
         real :: outflow
         integer :: f_length
         integer, dimension(2) :: outflow_cell
         ! Catagory Flags & Indices
         logical :: ocean
         logical :: flow_solved
         logical :: active
         integer :: plain_index
      end type

!-----Layer Functions---------------------------------------------------


      end module map_module
