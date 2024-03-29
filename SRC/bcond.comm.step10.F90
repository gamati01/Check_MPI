!=====================================================================
!     ****** LBE/bcond_comm_step10
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       bcond_comm
!     DESCRIPTION
!
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
        subroutine bcond_comm_step10
!
        use timing
        use storage
        use mpi
!
        implicit none
!
        integer      :: i,j,k 
        integer      :: tag, ierr
        integer      :: msgsizeX
        integer      :: msgsizeY
        integer      :: msgsizeZ
        integer      :: opt
        integer      :: status(MPI_STATUS_SIZE)
!
        real(mystor), dimension(:,:,:), allocatable, save :: bufferXINP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferXINM
        real(mystor), dimension(:,:,:), allocatable, save :: bufferXOUTP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferXOUTM
!        
        real(mystor), dimension(:,:,:), allocatable, save :: bufferYINP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferYINM
        real(mystor), dimension(:,:,:), allocatable, save :: bufferYOUTP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferYOUTM
!        
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZINP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZINM
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZOUTP
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZOUTM
!
        integer      :: status_front(MPI_STATUS_SIZE)
        integer      :: status_rear(MPI_STATUS_SIZE)
        integer      :: reqs_front(2)
        integer      :: reqs_rear(2)
!        
        integer      :: status_right(MPI_STATUS_SIZE)
        integer      :: status_left(MPI_STATUS_SIZE)
        integer      :: reqs_right(2)
        integer      :: reqs_left(2)
!        
        integer      :: status_up(MPI_STATUS_SIZE)
        integer      :: status_down(MPI_STATUS_SIZE)
        integer      :: reqs_up(2)
        integer      :: reqs_down(2)
!        
! start timing...
        call SYSTEM_CLOCK(countA0, count_rate, count_max)
        call time(tcountA0)
!
        if (.not. allocated(bufferXINP)) then
           allocate(bufferXINP (0:m+1,0:n+1,1:3))
           allocate(bufferXINM (0:m+1,0:n+1,1:3))
           allocate(bufferXOUTP(0:m+1,0:n+1,1:3))
           allocate(bufferXOUTM(0:m+1,0:n+1,1:3))
!           
           allocate(bufferYINP (0:l+1,0:n+1,1:3))
           allocate(bufferYINM (0:l+1,0:n+1,1:3))
           allocate(bufferYOUTP(0:l+1,0:n+1,1:3))
           allocate(bufferYOUTM(0:l+1,0:n+1,1:3))
!           
           allocate(bufferZINP (0:l+1,0:m+1,1:3))
           allocate(bufferZINM (0:l+1,0:m+1,1:3))
           allocate(bufferZOUTP(0:l+1,0:m+1,1:3))
           allocate(bufferZOUTM(0:l+1,0:m+1,1:3))
        endif
!        
        msgsizeX = (n+2)*(m+2)*3
        msgsizeY = (l+2)*(n+2)*3
        msgsizeZ = (l+2)*(m+2)*3
!
! Adding directly handling of self-copies. This is done as an
! optimization but also because CUDA-aware MPI_Sendrecv uses inefficient
! H2D + D2H copies
!        
!$acc enter data create(bufferXINP,bufferXINM,bufferXOUTM,bufferXOUTP & 
!$acc&                 ,bufferYINP,bufferYINM,bufferYOUTP,bufferYOUTM &
!$acc&                 ,bufferZINP,bufferZINM,bufferZOUTP,bufferZOUTM)
!
!------------------------------------------------------------------------
!
        if(proc_x == 1) then 
                write(6,*) "ERROR: not enough tasks along x", proc_z
                stop
        endif
!
        if(proc_y == 1) then 
                write(6,*) "ERROR: not enough tasks along y", proc_z
                stop
        endif
!
        if(proc_z == 1) then 
                write(6,*) "ERROR: not enough tasks along z", proc_z
                stop
        endif
!
!           
!----------------------------------------------------------------
! First pack data.....                
        call time(tcountZ0)
!$acc kernels async
        do j = 0,m+1
           do i = 0,l+1
! z+ direction              
              bufferZINP(i,j,1)=field1(i,j,n)
              bufferZINP(i,j,2)=field2(i,j,n)
              bufferZINP(i,j,3)=field3(i,j,n)
!
! z- direction              
              bufferZINM(i,j,1)=field1(i,j,1)
              bufferZINM(i,j,2)=field2(i,j,1)
              bufferZINM(i,j,3)=field3(i,j,1)
           enddo
        enddo
!$acc end kernels 
        call time(tcountZ1)
        timeZ = timeZ + (tcountZ1 -tcountZ0)
!
        call time(tcountX0)
!$acc kernels async
        do k = 0,n+1
           do j = 0,m+1
! x+ direction              
              bufferXINP(j,k,1)=field1(l,j,k)
              bufferXINP(j,k,2)=field2(l,j,k)
              bufferXINP(j,k,3)=field3(l,j,k)
!
! x- direction              
              bufferXINM(j,k,1)=field1(1,j,k)
              bufferXINM(j,k,2)=field2(1,j,k)
              bufferXINM(j,k,3)=field3(1,j,k)
           enddo
        enddo
!$acc end kernels
        call time(tcountX1)
        timeX = timeX + (tcountX1 -tcountX0)
!
        call time(tcountY0)
!$acc kernels async
        do k = 0,n+1
           do i = 0,l+1
! y+ direction              
              bufferYINP(i,k,1)=field1(i,m,k)
              bufferYINP(i,k,2)=field2(i,m,k)
              bufferYINP(i,k,3)=field3(i,m,k)
!
! y- direction              
              bufferYINM(i,k,1)=field1(i,1,k)
              bufferYINM(i,k,2)=field2(i,1,k)
              bufferYINM(i,k,3)=field3(i,1,k)
           enddo
        enddo
!$acc end kernels
        call time(tcountY1)
        timeY = timeY + (tcountY1 -tcountY0)
!           
!$acc wait 
!----------------------------------------------------------------
! Second receive data
        tag = 34
!$acc host_data use_device(bufferZOUTP)
        call mpi_irecv(bufferZOUTP(0,0,1), msgsizez, MYMPIREAL, down(2), tag, &
                          lbecomm, reqs_up(1), ierr)
!$acc end host_data
!
        tag = 32
!$acc host_data use_device(bufferZOUTM)
        call mpi_irecv(bufferZOUTM(0,0,1), msgsizez, MYMPIREAL, up(2), tag, &
                          lbecomm, reqs_down(1), ierr)
!$acc end host_data
!
        tag = 11
!$acc host_data use_device(bufferXOUTP)
        call mpi_irecv(bufferXOUTP(0,0,1),msgsizeX,MYMPIREAL,rear(2), tag, &
                          lbecomm, reqs_front(1), ierr)
!$acc end host_data
!                     
        tag = 10
!$acc host_data use_device(bufferXOUTM)
        call mpi_irecv(bufferXOUTM(0,0,1),msgsizeX,MYMPIREAL,front(2),tag, &
                             lbecomm, reqs_rear(1), ierr)
!$acc end host_data
!
        tag = 23
!$acc host_data use_device(bufferYOUTP)
        call mpi_irecv(bufferYOUTP(0,0,1), msgsizey, MYMPIREAL,left(2), tag, &
                          lbecomm, reqs_right(1), ierr)
!$acc end host_data
!                  
        tag = 21
!$acc host_data use_device(bufferYOUTM)
        call mpi_irecv(bufferYOUTM(0,0,1), msgsizey, MYMPIREAL,right(2), tag, &
                          lbecomm, reqs_left(1), ierr)
!$acc end host_data
!
!----------------------------------------------------------------
! Third send data.....                
        tag = 34
!$acc host_data use_device(bufferZINP)
        call mpi_isend(bufferZINP(0,0,1), msgsizez, MYMPIREAL, up(2), tag, &
                          lbecomm, reqs_up(2), ierr)
!$acc end host_data
!
        tag = 32
!$acc host_data use_device(bufferZINM)
        call mpi_isend(bufferZINM(0,0,1), msgsizez, MYMPIREAL, down(2), tag, &
                          lbecomm, reqs_down(2), ierr)
!$acc end host_data
!
        tag = 11
!$acc host_data use_device(bufferXINP)
        call mpi_isend(bufferXINP(0,0,1),msgsizeX,MYMPIREAL,front(2),tag, &
                          lbecomm, reqs_front(2), ierr)
!$acc end host_data
!
        tag = 10
!$acc host_data use_device(bufferXINM)
        call mpi_isend(bufferXINM(0,0,1),msgsizex,MYMPIREAL,rear(2),tag, &
                          lbecomm, reqs_rear(2), ierr)
!$acc end host_data
!
        tag = 23
!$acc host_data use_device(bufferYINP)
        call mpi_isend(bufferYINP(0,0,1), msgsizeY, MYMPIREAL,right(2), tag, &
                          lbecomm, reqs_right(2), ierr)
!$acc end host_data
!
        tag = 21
!$acc host_data use_device(bufferYINM)
        call mpi_isend(bufferYINM(0,0,1), msgsizey, MYMPIREAL, left(2), tag, &
                          lbecomm, reqs_left(2), ierr)
!$acc end host_data
!
! ------------------------------------------------------------------------------
!                  
! overlap region
        call do_somethingGPU_masked(zero)
!        
!----------------------------------------------------------------
! forth  wait...           
        call MPI_Waitall(2,reqs_up   ,MPI_STATUSES_IGNORE, ierr)
        call MPI_Waitall(2,reqs_down ,MPI_STATUSES_IGNORE, ierr)
        call MPI_Waitall(2,reqs_rear ,MPI_STATUSES_IGNORE, ierr)
        call MPI_Waitall(2,reqs_front,MPI_STATUSES_IGNORE, ierr)
        call MPI_Waitall(2,reqs_left ,MPI_STATUSES_IGNORE, ierr)
        call MPI_Waitall(2,reqs_right,MPI_STATUSES_IGNORE, ierr)
!
!        call mpi_barrier(lbecomm,ierr)
!
!----------------------------------------------------------------
!fifth unpack data
        call time(tcountZ0)
!$acc kernels async 
        do j = 0,m+1
           do i = 0,l+1
! z+ direction
              field1(i,j,0)=bufferZOUTP(i,j,1)
              field2(i,j,0)=bufferZOUTP(i,j,2)
              field3(i,j,0)=bufferZOUTP(i,j,3)
!
! z- direction
              field1(i,j,n+1) = bufferZOUTM(i,j,1)
              field2(i,j,n+1) = bufferZOUTM(i,j,2)
              field3(i,j,n+1) = bufferZOUTM(i,j,3)
           enddo
        enddo
!$acc end kernels
        call time(tcountZ1)
        timeZ = timeZ + (tcountZ1 -tcountZ0)
!
        call time(tcountX0)
!$acc kernels async 
        do k = 0,n+1
           do j = 0,m+1
! x+ direction
              field1(0,j,k) = bufferXOUTP(j,k,1)
              field2(0,j,k) = bufferXOUTP(j,k,2)
              field3(0,j,k) = bufferXOUTP(j,k,3)
!
! x- direction
              field1(l+1,j,k) = bufferXOUTM(j,k,1)
              field2(l+1,j,k) = bufferXOUTM(j,k,2)
              field3(l+1,j,k) = bufferXOUTM(j,k,3)
           enddo
        enddo
!$acc end kernels
        call time(tcountX1)
        timeX = timeX + (tcountX1 -tcountX0)
!           
        call time(tcountY0)
!$acc kernels async 
        do k = 0,n+1
           do i = 0,l+1
! y+ direction
              field1(i,0,k)=bufferYOUTP(i,k,1)
              field2(i,0,k)=bufferYOUTP(i,k,2)
              field3(i,0,k)=bufferYOUTP(i,k,3)
!                 
! y- direction
              field1(i,m+1,k)=bufferYOUTM(i,k,1)
              field2(i,m+1,k)=bufferYOUTM(i,k,2)
              field3(i,m+1,k)=bufferYOUTM(i,k,3)
           enddo
        enddo
!$acc end kernels

!$acc wait
        call time(tcountY1)
        timeY = timeY + (tcountY1 -tcountY0)
!
        call time(tcountA1)
        call SYSTEM_CLOCK(countA1, count_rate, count_max)
        time_mp = time_mp + real(countA1-countA0)/(count_rate)
        time_mp1 = time_mp1 + (tcountA1-tcountA0)
!
#ifdef DEBUG_1
        if(myrank == 0) then
           write(6,*) "DEBUG1: Exiting from sub. bcond_comm_step10"
        endif
#endif
!        
        end subroutine bcond_comm_step10
