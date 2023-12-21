! =====================================================================
!     ****** LBE/bgk3D
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       bgk2d
!     DESCRIPTION
!       main program for LBM 3D
!     INPUTS
!       none
!     OUTPUT
!       none
!     TODO
!
!     NOTES
!
!       integer variables used: itfin, itstart, ivtim
!                               itime, itsave, icheck, itrestart
!                               isignal
!       real variables used: tempo1, tempo2     
!       open the following unit: 16 (bgk.log)
!                                60 (prof_k.dat)
!                                61 (prof_i.dat)
!                                62 (u_med.dat)
!                                63 (diagno.dat)
!                                68 (probe.dat)
!                                69 (bgk.perf)
!                              
!     *****
! =====================================================================
!
      program mpi_gpuaware
!
      use storage
      use timing
      use real_kinds
!
      implicit none
!
      INTEGER:: itfin, ierr
      INTEGER:: itime, icheck
!
! reading run input
      call input(itfin,icheck)
!      
! setup mpi stuff
      call setup_MPI
!
! some info
      call outdat(itfin,icheck)
!
! fields allocation
      call alloca
!      
! initialize the fields...
      call init
!
! start timing       
      call SYSTEM_CLOCK(countE0, count_rate, count_max)
      call time(tcountE0)
!
!$acc data copy(field1,field2,field3,temp1,temp2,temp3)
!
! main loop starts here.....
      do itime=1,itfin
         call boundaries         ! MPI call 
         call do_somethingGPU    ! do something on GP
!
! diagnostic         
         if(mod(itime,icheck)==0) then
            if(myrank==0) then
               write(6,*) "Iteration =", itime, "/", itfin
!               write(6,*) "VALIDATION (x): ", field1(l/2,m/2,n/2)
!               write(6,*) "VALIDATION (y): ", field2(l/2,m/2,n/2)
!               write(6,*) "VALIDATION (z): ", field3(l/2,m/2,n/2)
            endif
            call prof_i(itime,m/2,n/2)
            call prof_j(itime,l/2,n/2)
            call prof_k(itime,l/2,m/2)
         endif
!
      enddo
!$acc end data
!
! stop timing      
      call SYSTEM_CLOCK(countE1, count_rate, count_max)
      call time(tcountE1)
      time_loop = real(countE1-countE0)/(count_rate)
      time_loop1 = tcountE1-tcountE0
!
! finalize all
      call finalize(itfin)    
!
      end program mpi_gpuaware
