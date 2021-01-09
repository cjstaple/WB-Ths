!=======================================================================
!     Code Performance & Profiling
!=======================================================================

      module prof_module

      use parameter_module

      implicit none

      integer, parameter :: n_max=20            !Max Number of Trackers
      logical, dimension(n_max) :: active       !On/Off Button
      character(len=20), dimension(n_max) :: leg      !Tracker Name
      integer, dimension(n_max) :: tag          !Number of Calls
      real, dimension(n_max) :: t0              !Interval Start
      real, dimension(n_max) :: dt              !Accumulated Time
      integer(8),dimension(n_max) :: sc         !System Clocks
      real, dimension(n_max) :: wct             !Wall Clock Time
      integer(8) :: ntmax, nps

      contains

!***********************************************************************
!     This subroutine gets called at the beginning of the program to
!     initialize the timing variables and set the run time standard
!***********************************************************************
      subroutine prof_initial
      
      real :: dummy

      call cpu_time(dummy) !Starts a dummy at the beginning of the code
      call system_clock(COUNT_RATE=nps,COUNT_MAX=ntmax)
      active = .false.
      leg = '  '
      tag = 0
      dt = 0.
      wct = 0.
      call prof_enter(n_max,'    TOTAL RUN TIME: ')

      end subroutine prof_initial

!***********************************************************************
!     This subroutine starts the tracker n on thread t 
!***********************************************************************

      subroutine prof_enter(n,nam)

      integer, intent(in) :: n
      character(len=*) :: nam

      active(n) = .true.   !turns on tracker n on thread t
      leg(n) = nam           !names the tracker
      tag(n) = tag(n)+1  !increments the calls
      call cpu_time(t0(n)) !Turn on cpu clock
      call system_clock(COUNT=sc(n)) !Turn on wall clock

      end subroutine prof_enter

!***********************************************************************
!     This subroutine ends the tracker n on thread t
!***********************************************************************

      subroutine prof_exit(n)

      integer, intent(in) :: n
      real :: t1
      integer(8) :: st
!.......................................................................

      if(active(n)) then
         call cpu_time(t1)
         call system_clock(COUNT=st)

         dt(n) = dt(n)+t1-t0(n)
         t0(n) = t1

         if(st.lt.sc(n)) st = st+ntmax
         wct(n) = wct(n)+real(st-sc(n))/nps
         sc(n) = MOD(st,ntmax)
      endif
      active(n) = .false.

      end subroutine prof_exit

!***********************************************************************
!     This subroutine is used to write the profiling information into a
!     constantly updating file periodically so that the performance can
!     be tracked at over different time intervals. 
!***********************************************************************

      subroutine prof_write

      integer :: n
      real :: total_cput = 0.00d+00
      real :: total_wct = 0.00d+00
!.......................................................................

      do n=1,n_max
        if(active(n)) then
           call prof_exit(n)
           active(n)=.true.
        endif
        total_cput = total_cput+dt(n)
        total_wct = total_wct+wct(n)
      enddo
!.......................................................................

      open(12,file='profile.dat',status='unknown')
      write(12,15)
      write(12,16) 'Profile for Master Thread'
      write(12,15)
      write(12,11)
      write(12,12) 'Interval Name','Calls','TOTAL CPU','Average CPU',
     &   '% CPU_t','Total Wall', 'Average Wall', '% Wall_t'
      write(12,11)
      do n=1,n_max
       if(tag(n).gt.0) then
         write(12,13) leg(n),tag(n),dt(n),dt(n)/float(tag(n)),
     &     100.*dt(n)/dt(n_max),wct(n),wct(n)/float(tag(n)),
     &     100.*wct(n)/wct(n_max)
       endif
      enddo
      write(12,11)
      write(12,10)
      write(12,15)
      write(12,10)

      write(12,17) psol
      close(12)
!.......................................................................
10    format('  ')
11    format('|',20('-'),'|',7('------------|'))
12    format(1x,a20,7(1x,a12))
13    format(1x,a20,1x,i12,2(1x,1pes12.4),1x,0pf12.3,2(1x,1pes12.4),1x,
     &   0pf12.3)
14    format(1x,a22,i4)
15    format(72('='))
16    format(10x,a30)
17    format(1x,f7.3,'% of cells have been solved')

      end subroutine prof_write

!***********************************************************************

      end module prof_module

!=======================================================================
