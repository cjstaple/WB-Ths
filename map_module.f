!=======================================================================
!     Flow-Pathfinder: Map Level Module
!=======================================================================

      module map_module

      use parameter_module

      implicit none

      type map_type
         logical :: ocean              ! Flag for Ocean Cells
         logical :: flow_solved        ! Prevents resolving cells
         integer :: height             ! [px] Elevation
         integer :: temp               ! [K] Temperature
         integer :: c_dis              ! Cells in Discharge Path
         integer :: c_acc              ! Cells Draining into this one
         real :: rain                  ! [m^3/yr] Rain
         real :: elev                  ! [m] Elevation
         real :: outflow               ! Cell Discharge [m^3/s]
         real :: flow_frac             ! % of rain not lost to evap
         real :: grad                  ! Local Cell Gradient
         real :: out_rate              ! Gradient Calculated output
         real :: vel_out               ! Rate Flow Velocity
         real :: vol_s                 ! Steady State Volume
         real :: xA                    ! Cross-Sectional Area
         real :: Ax                    ! Secondary Area
         real :: v_o                   ! Secondary Velocity
         real :: dep                   ! River Depth Estimate
         integer, dimension(2) :: d_cell ! Flow Destination Cell
      end type

      end module map_module
