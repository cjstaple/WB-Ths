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
         real :: invol
         real :: flow_frac !Water - Freeze - Evaporate
         real :: outflow
         integer :: f_length
         integer, dimension(2) :: outflow_cell
         ! Catagory Flags & Indices
         logical :: ocean
         logical :: flow_solved
         ! Flow Rate
         real :: out_rate
         real :: vol_0
         real :: xA
         real :: dep
         logical :: rate_solved
      end type

!-----Layer Functions---------------------------------------------------


      end module map_module
