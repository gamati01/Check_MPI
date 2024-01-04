!=====================================================================
!     ****** mpi_gpuaware
!
!     COPYRIGHT
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       bgk2d
!     DESCRIPTION
!       driver for different mpi communication pattern
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
!       open the following unit: 
!                                6? (prof_k.dat)
!                                6? (prof_j.dat)
!                                6? (prof_i.dat)
!                                6? (task.<taskid>.log)
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
      INTEGER:: opt
!
! reading run input
      call input(itfin,icheck)
!      
! setup mpi stuff
      call setup_MPI
!      
#ifdef MPIP
! disable mpip (it is enabled by default)     
      call MPI_PCONTROL( 0 )
#endif
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
#ifdef MPIP
! enable mpip      
      call MPI_PCONTROL( 1 )
#endif
!
! start timing       
      call SYSTEM_CLOCK(countE0,count_rate,count_max)
      call time(tcountE0)
!
!$acc data copy(field1,field2,field3,temp1,temp2,temp3)
!
! main loop starts here.....
      do itime=1,itfin
         call boundaries         ! MPI call 

#ifdef STEP9         
! do something on GPU 
         call do_somethingGPU_overlap
#elif STEP8         
! do something on GPU 
         call do_somethingGPU_overlap
#else
! do something on GPU 
         call do_somethingGPU   
#endif         
!
! diagnostic         
         if(mod(itime,icheck)==0) then
!                 
! start timing       
            call SYSTEM_CLOCK(countD0, count_rate, count_max)
            call time(tcountD0)
!      
            if(myrank==0) then
               write(6,*) "Iteration =", itime, "/", itfin
!                  write(6,*) "VALIDATION (x): ", field1(l/2,m/2,n/2)
!                  write(6,*) "VALIDATION (y): ", field2(l/2,m/2,n/2)
!                  write(6,*) "VALIDATION (z): ", field3(l/2,m/2,n/2)
            endif
            call prof_i(itime,m/2,n/2)
            call prof_j(itime,l/2,n/2)
            call prof_k(itime,l/2,m/2)
!           
            call mpi_barrier(MPI_COMM_WORLD,ierr)
! 
! stop timing      
            call SYSTEM_CLOCK(countD1, count_rate, count_max)
            call time(tcountD1)
            time_dg  = time_dg  + real(countD1-countD0)/(count_rate)
            time_dg1 = time_dg1 + tcountD1-tcountD0
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
#ifdef MPIP
! disable mpip      
      call MPI_PCONTROL( 0 )
#endif
      
! finalize all
      call finalize(itfin)    
!
      end program mpi_gpuaware
