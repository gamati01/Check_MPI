!=====================================================================
!     ****** LBE/setup_mpi
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       setup_mpi
!     DESCRIPTION
!       Simple wrapper for mpi setup
!     INPUTS
!       none
!     OUTPUT
!       none
!     TODO
!       
!     NOTES
!
!     *****
!=====================================================================
!
      subroutine setup_mpi
!
      use storage
      use timing
!
      use mpi
!
!
#ifdef OPENACC
      use openacc
#endif

      implicit none
!
      integer:: i, uni
      integer:: ierr, len                    ! mpi variables
      character*15 hname
      character*17 file_name1
      character*17 file_name2
      character*17 file_name3
      character*15 file_name5
!
      real(mykind):: knorm
!
      knorm = 1.0/1024.0
      time_loop = 0.0
      time_coll = 0.0
      time_mp = 0.0
      time_dg = 0.0
      time_loop1 = 0.0
      time_coll1 = 0.0
      time_mp1 = 0.0
      time_dg1 = 0.0
!
      call mpi_init(ierr)
      call MPI_comm_size(MPI_COMM_WORLD, nprocs, ierr)
      call MPI_comm_rank(MPI_COMM_WORLD, myrank, ierr)
!
#ifdef OPENACC
      ndev= acc_get_num_devices(acc_device_nvidia)
      call acc_set_device_num(mydev,acc_device_nvidia)
      write(6,*) "INFO: using GPU",mydev, ndev
#endif
!
#ifdef OPENACC
      ndev= acc_get_num_devices(acc_device_nvidia)
      if(ndev == 0) then
         write(6,*) "WARNINIG: No GPUs found:", ndev
      else 
         write(6,*) "INFO: GPUs found:", ndev
      endif
#endif
!
! set the gpu to the task id
      call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, &
                                 MPI_INFO_NULL, localcomm, ierr)
      call MPI_Comm_rank(localcomm, mydev, ierr)
      call MPI_get_processor_name(hname,len,ierr)
#ifdef OPENACC
      call acc_set_device_num(mydev,acc_device_nvidia)
#endif
      write(6,*) "INFO: rank",myrank," GPU",mydev, "node ", hname
!
! check
      if ((proc_x*proc_y*proc_z).ne.nprocs) then
        if (myrank.eq.0) then
          write(*,*) 'ERROR: decomposed for', & 
                             proc_x*proc_y*proc_z, 'procs'
          write(*,*) 'ERROR: launched on', nprocs, 'processes'
        end if
        call MPI_finalize(ierr)
        stop
      end if
!
      rreorder=.false.
!
      periodic(1) = .true.
      periodic(2) = .true.
      periodic(3) = .true.
!
      prgrid(1) = proc_x
      prgrid(2) = proc_y
      prgrid(3) = proc_z
!
!
! building virtual topology
      call MPI_cart_create(mpi_comm_world, mpid, prgrid, &
                              periodic,rreorder,lbecomm,ierr)

      call MPI_comm_rank(lbecomm, myrank, ierr)
      call MPI_cart_coords(lbecomm, myrank, mpid, &
                            mpicoords, ierr)
!
      call mpi_barrier(MPI_COMM_WORLD,ierr)
!
! x dir
      call MPI_cart_shift(lbecomm, 0, 1, rear(2), front(2), ierr)
! y dir
      call MPI_cart_shift(lbecomm, 1, 1, left(2), right(2), ierr)
! z dir
      call MPI_cart_shift(lbecomm, 2, 1, down(2), up(2), ierr)
!
! yz plane is composed by single point (stride.ne.1)
      call MPI_type_vector((n+2)*(m+2), 1, l+2, MYMPIREAL, yzplane, ierr)
      call MPI_type_commit(yzplane,ierr)
      if(myrank.eq.0) then
         write(6,*) "INFO: yzplane (KB)-->", (n+2)*(m+2)*knorm
      endif
!
! xz plane is composed by single arrays (stride.ne.1)
      call MPI_type_vector(n+2, l+2, (m+2)*(l+2), MYMPIREAL, xzplane, ierr)
      call MPI_type_commit(xzplane,ierr)
      if(myrank.eq.0) then
         write(6,*) "INFO: xzplane (KB)-->", (n+2)*(l+2)*knorm
      endif
!
! xy plane is a contiguous arrays (stride.eq.1)
      call MPI_type_contiguous((l+2)*(m+2), MYMPIREAL, xyplane, ierr)
      call MPI_type_commit(xyplane,ierr)
      if(myrank.eq.0) then
         write(6,*) "INFO: xyplane (KB)-->", (m+2)*(l+2)*knorm
      endif
!
      file_offset = 0    !to check
!
#ifdef MEM_CHECK
      if(myrank == 0) then
         mem_stop = get_mem();
         write(6,*) "MEM_CHECK: after sub. setup_MPI mem =", mem_stop
      endif
#endif
!
! prof_i
      file_name1 = 'prof_i.xxxxxx.dat'
      write(file_name1(8:13),3100) myrank
      open(61,file=file_name1, status='unknown')
!
! prof_j
      file_name2 = 'prof_j.xxxxxx.dat'
      write(file_name2(8:13),3100) myrank
      open(62,file=file_name2, status='unknown')
!
! prof_k
      file_name3 = 'prof_k.xxxxxx.dat'
      write(file_name3(8:13),3100) myrank
      open(63,file=file_name3, status='unknown')
!
! task.log
      file_name5 = 'task.xxxxxx.log'
      write(file_name5(6:11),3100) myrank
      open(38,file=file_name5, status='unknown')        ! task.XXXXXX.log
!
! formats...
3100      format(i6.6)

#ifdef MDEBUG_1
      if(myrank == 0) then
         write(6,*) "DEBUG1: Exiting from sub. setup_mpi"
      endif
#endif

      end subroutine setup_mpi
