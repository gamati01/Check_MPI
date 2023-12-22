!=====================================================================
!     ****** LBE/bcond_comm_step6
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
        subroutine bcond_comm_step6
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
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZIN
        real(mystor), dimension(:,:,:), allocatable, save :: bufferZOUT
!
        integer      :: status_front(MPI_STATUS_SIZE)
        integer      :: status_rear(MPI_STATUS_SIZE)
        integer      :: reqs_front(1)
        integer      :: reqs_rear(1)
!        
        integer      :: status_right(MPI_STATUS_SIZE)
        integer      :: status_left(MPI_STATUS_SIZE)
        integer      :: reqs_right(1)
        integer      :: reqs_left(1)
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
           allocate(bufferZIN (0:l+1,0:m+1,1:3))
           allocate(bufferZOUT(0:l+1,0:m+1,1:3))
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
!$acc enter data create(bufferXINP,bufferXINM,bufferXOUTM,bufferXOUTP,bufferYINP,bufferYINM,bufferYOUTP,bufferYOUTM,bufferZIN,bufferZOUT)
!
!------------------------------------------------------------------------
! comms along z + 
!

        if(proc_z == 1) then 
!
!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 field1(i,j,0)=field1(i,j,n)
                 field2(i,j,0)=field2(i,j,n)
                 field3(i,j,0)=field3(i,j,n)
              enddo
           enddo
!$acc end kernels
!           
!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 field1(i,j,n+1)=field1(i,j,1)
                 field2(i,j,n+1)=field2(i,j,1)
                 field3(i,j,n+1)=field3(i,j,1)
              enddo
           enddo
!$acc end kernels
!           
        else
!                
           call time(tcountZ0)
!           
           tag = 34

!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 bufferZIN(i,j,1)=field1(i,j,n)
                 bufferZIN(i,j,2)=field2(i,j,n)
                 bufferZIN(i,j,3)=field3(i,j,n)
              enddo
           enddo
!$acc end kernels
!
!$acc host_data use_device(bufferZIN,bufferZOUT)
           call mpi_sendrecv(bufferZIN(0,0,1),msgsizeZ,MYMPIREAL,up(2),tag,&
                             bufferZOUT(0,0,1),msgsizeZ,MYMPIREAL,down(2),tag,&
                             lbecomm,status,ierr)
!$acc end host_data
!
!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 field1(i,j,0)=bufferZOUT(i,j,1)
                 field2(i,j,0)=bufferZOUT(i,j,2)
                 field3(i,j,0)=bufferZOUT(i,j,3)
              enddo
           enddo
!$acc end kernels
!        
!           call mpi_barrier(lbecomm,ierr)
!
! comms along z - 
!
           tag = 32
!
!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 bufferZIN(i,j,1)=field1(i,j,1)
                 bufferZIN(i,j,2)=field2(i,j,1)
                 bufferZIN(i,j,3)=field3(i,j,1)
              enddo
           enddo
!$acc end kernels
!        
!$acc host_data use_device(bufferZIN,bufferZOUT)
           call mpi_sendrecv(bufferZIN(0,0,1),msgsizeZ,MYMPIREAL,down(2),tag,&
                             bufferZOUT(0,0,1),msgsizeZ,MYMPIREAL,up(2),tag,&
                             lbecomm,status,ierr)
!$acc end host_data
!
!$acc kernels
           do j = 0,m+1
              do i = 0,l+1
                 field1(i,j,n+1) = bufferZOUT(i,j,1)
                 field2(i,j,n+1) = bufferZOUT(i,j,2)
                 field3(i,j,n+1) = bufferZOUT(i,j,3)
              enddo
           enddo
!$acc end kernels
!
           call time(tcountZ1)
           timeZ = timeZ + (tcountZ1 -tcountZ0)
!
        endif
!        
        call mpi_barrier(lbecomm,ierr)
!
!------------------------------------------------------------------------
! comms along x + 
!        
        if(proc_x == 1) then 
!                
!$acc kernels
           do k = 0,n+1
              do j = 0,m+1
                 field1(0,j,k)=field1(l,j,k)
                 field2(0,j,k)=field2(l,j,k)
                 field3(0,j,k)=field3(l,j,k)
              enddo
           enddo
!$acc end kernels
!           
!$acc kernels
           do k = 0,n+1
              do j = 0,m+1
                 field1(l+1,j,k)=field1(1,j,k)
                 field2(l+1,j,k)=field2(1,j,k)
                 field3(l+1,j,k)=field3(1,j,k)
              enddo
           enddo
!$acc end kernels
!           
        else 
!
           call time(tcountX0)
!
! First pack data.....                
!$acc kernels
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
!
! Second send data.....                
           tag = 11
!$acc host_data use_device(bufferXINP)
           call mpi_isend(bufferXINP(0,0,1),msgsizeX,MYMPIREAL,front(2),tag, &
                             lbecomm, reqs_front(1), ierr)
!$acc end host_data
!
           tag = 10
!$acc host_data use_device(bufferXINM)
           call mpi_isend(bufferXINM(0,0,1),msgsizex,MYMPIREAL,rear(2),tag, &
                             lbecomm, reqs_rear(1), ierr)
!$acc end host_data
!
           tag = 11
!Third receive data
!$acc host_data use_device(bufferXOUTP)
           call mpi_recv(bufferXOUTP(0,0,1),msgsizeX,MYMPIREAL,rear(2), tag, &
                             lbecomm, status_front, ierr)
!$acc end host_data
!                     
           tag = 10
!$acc host_data use_device(bufferXOUTM)
           call mpi_recv(bufferXOUTM(0,0,1),msgsizeX,MYMPIREAL,front(2),tag, &
                             lbecomm, status_rear, ierr)
!$acc end host_data
!
! Fourth  unpack data
!$acc kernels
           do k = 0,n+1
              do j = 0,m+1
! x- direction                 
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
!
! Fifth  wait...           
           call mpi_wait(reqs_rear(1), status_rear, ierr)
           call mpi_wait(reqs_front(1), status_front, ierr)
!        
           call time(tcountX1)
           timeX = timeX + (tcountX1 -tcountX0)
!
        endif
!        
!        call mpi_barrier(lbecomm,ierr)
!
!------------------------------------------------------------------------
! comms along y + 
!        
        if(proc_y == 1) then 
!                
!$acc kernels
           do k = 0,n+1
              do i = 0,l+1
                 field1(i,0,k)=field1(i,m,k)
                 field2(i,0,k)=field2(i,m,k)
                 field3(i,0,k)=field3(i,m,k)
              enddo
           enddo
!$acc end kernels
!
!$acc kernels
           do k = 0,n+1
              do i = 0,l+1
                 field1(i,m+1,k)=field1(i,1,k)
                 field2(i,m+1,k)=field2(i,1,k)
                 field3(i,m+1,k)=field3(i,1,k)
              enddo
           enddo
!$acc end kernels
!
        else
!        
           call time(tcountY0)
!        
! First pack data.....                
!$acc kernels
           do k = 0,n+1
              do i = 0,l+1
                 bufferYINP(i,k,1)=field1(i,m,k)
                 bufferYINP(i,k,2)=field2(i,m,k)
                 bufferYINP(i,k,3)=field3(i,m,k)
!
                 bufferYINM(i,k,1)=field1(i,1,k)
                 bufferYINM(i,k,2)=field2(i,1,k)
                 bufferYINM(i,k,3)=field3(i,1,k)
              enddo
           enddo
!$acc end kernels
!           
! Second send data.....
           tag = 23
!$acc host_data use_device(bufferYINP)
           call mpi_isend(bufferYINP(0,0,1), msgsizeY, MYMPIREAL,right(2), tag, &
                          lbecomm, reqs_right(1), ierr)
!$acc end host_data
!
           tag = 21
!$acc host_data use_device(bufferYINM)
           call mpi_isend(bufferYINM(0,0,1), msgsizey, MYMPIREAL, left(2), tag, &
                          lbecomm, reqs_left(1), ierr)
!$acc end host_data
!
!Third receive data
           tag = 23
!$acc host_data use_device(bufferYOUTP)
           call mpi_recv(bufferYOUTP(0,0,1), msgsizey, MYMPIREAL,left(2), tag, &
                          lbecomm, status_right, ierr)
!$acc end host_data
!                  
           tag = 21
!$acc host_data use_device(bufferYOUTM)
           call mpi_recv(bufferYOUTM(0,0,1), msgsizey, MYMPIREAL,right(2), tag, &
                          lbecomm, status_left, ierr)
!$acc end host_data


! Fourth  unpack data
!$acc kernels
           do k = 0,n+1
              do i = 0,l+1
                 field1(i,0,k)=bufferYOUTP(i,k,1)
                 field2(i,0,k)=bufferYOUTP(i,k,2)
                 field3(i,0,k)=bufferYOUTP(i,k,3)
!                 
                 field1(i,m+1,k)=bufferYOUTM(i,k,1)
                 field2(i,m+1,k)=bufferYOUTM(i,k,2)
                 field3(i,m+1,k)=bufferYOUTM(i,k,3)
              enddo
           enddo
!$acc end kernels

! Fifth  wait...
           call mpi_wait(reqs_left(1), status_left, ierr)
           call mpi_wait(reqs_right(1), status_right, ierr)

           call time(tcountY1)
           timeY = timeY + (tcountY1 -tcountY0)
!
        endif
!        
        call mpi_barrier(lbecomm,ierr)
!
        call time(tcountA1)
        call SYSTEM_CLOCK(countA1, count_rate, count_max)
        time_mp = time_mp + real(countA1-countA0)/(count_rate)
        time_mp1 = time_mp1 + (tcountA1-tcountA0)
!
#ifdef DEBUG_1
        if(myrank == 0) then
           write(6,*) "DEBUG1: Exiting from sub. bcond_comm_step6"
        endif
#endif
!        
        end subroutine bcond_comm_step6
