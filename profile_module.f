!=======================================================================
!     Code Performance & Profiling
!=======================================================================

      module prof_module

      use parameter_module

      implicit none

      integer, parameter :: n_max=20            !Max Number of Trackers
      integer, parameter :: t_max=5             !Max Number of Threads
      logical, dimension(n_max,t_max) :: active       !On/Off Button
      character(len=20), dimension(n_max) :: leg      !Tracker Name
      integer, dimension(n_max,t_max) :: tag          !Number of Calls
      real, dimension(n_max,t_max) :: t0              !Interval Start
      real, dimension(n_max,t_max) :: dt              !Accumulated Time
      integer(8),dimension(n_max,t_max) :: sc         !System Clocks
      real, dimension(n_max,t_max) :: wct             !Wall Clock Time
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
      call prof_enter(n_max,1,'    TOTAL RUN TIME: ')

      end subroutine prof_initial

!***********************************************************************
!     This subroutine starts the tracker n on thread t 
!***********************************************************************

      subroutine prof_enter(n,t,nam)

      integer, intent(in) :: n
      integer, intent(in) :: t
      character(len=*) :: nam

      active(n,t) = .true.   !turns on tracker n on thread t
      leg(n) = nam           !names the tracker
      tag(n,t) = tag(n,t)+1  !increments the calls
      call cpu_time(t0(n,t)) !Turn on cpu clock
      call system_clock(COUNT=sc(n,t)) !Turn on wall clock

      end subroutine prof_enter

!***********************************************************************
!     This subroutine ends the tracker n on thread t
!***********************************************************************

      subroutine prof_exit(n,t)

      integer, intent(in) :: n
      integer, intent(in) :: t
      real :: t1
      integer(8) :: st
!.......................................................................

      if(active(n,t)) then
         call cpu_time(t1)
         call system_clock(COUNT=st)

         dt(n,t) = dt(n,t)+t1-t0(n,t)
         t0(n,t) = t1

         if(st.lt.sc(n,t)) st = st+ntmax
         wct(n,t) = wct(n,t)+real(st-sc(n,t))/nps
         sc(n,t) = MOD(st,ntmax)
      endif
      active(n,t) = .false.

      end subroutine prof_exit

!***********************************************************************
!     This subroutine is used to write the profiling information into a
!     constantly updating file periodically so that the performance can
!     be tracked at over different time intervals. 
!***********************************************************************

      subroutine prof_write

      use parameter_module, only: psol

      integer :: n, t
      real,dimension(t_max) :: total_cput = 0.00d+00
      real,dimension(t_max) :: total_wct = 0.00d+00
!.......................................................................

      do n=1,n_max
       do t=1,t_max
        if(active(n,t)) then
           call prof_exit(n,t)
           active(n,t)=.true.
        endif
        total_cput(t) = total_cput(t)+dt(n,t)
        total_wct(t) = total_wct(t)+wct(n,t)
       enddo
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
       if(tag(n,1).gt.0) then
         write(12,13) leg(n),tag(n,1),dt(n,1),dt(n,1)/float(tag(n,1)),
     &     100.*dt(n,1)/dt(n_max,1),wct(n,1),wct(n,1)/float(tag(n,1)),
     &     100.*wct(n,1)/wct(n_max,1)
       endif
      enddo
      write(12,11)
      write(12,10)
      write(12,15)
      write(12,10)

      write(12,17) 100.*psol
!      do t=2,t_max
!       write(12,14) 'Profile for Thread: ',t
!       write(12,10)
!       write(12,11)
!       write(12,12) 'Interval Name','Calls','TOTAL CPU','Average CPU',
!     &   '% CPU_t','Total Wall', 'Average Wall', '% Wall_t'
!       write(12,11)
!       do n=1,n_max
!          if(tag(n,t).gt.0) then
!           write(12,13) leg(n),tag(n,t),dt(n,t),dt(n,t)/float(tag(n,t)),
!     &       100.*dt(n,t)/dt(n_max,t),wct(n,t),wct(n,t)/float(tag(n,t)),
!     &       100.*wct(n,t)/wct(n_max,t)
!          endif
!       enddo
!       write(12,11)
!       write(12,10)
!      enddo
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
