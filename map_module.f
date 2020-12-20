!=======================================================================
!     Flow-Pathfinder: Map Level Module
!=======================================================================

      module map_module

      use parameter_module

      implicit none

      type map_type
         ! Catagory Flags
         logical :: ocean
         logical :: flow_solved
         ! Cell Properties
         integer :: height   ! [px]
         real :: elev        ! Elevation [m]
         real :: rain        ! [m^3/yr]
         integer :: temp     ! [K]
         real :: flow_frac   ! Water - Freeze - Evaporate
         real :: outflow     ! Cell Discharge m^3/s
         integer :: f_length ! Number of px this flows into
         integer :: d_px     ! Number of px that flow through this one
         integer, dimension(2) :: outflow_cell
         ! Flow Rate
         real :: grad        ! Local Gradient
         real :: out_rate    ! Pixel drainage [px/s]
         real :: vol_0       ! Steady State Volume [m^3]
         real :: xA          ! X-Area of river [m^2]
         real :: dep         ! Approximate Depth
      end type

!-----Layer Functions---------------------------------------------------


      end module map_module
