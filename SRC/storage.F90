! =====================================================================
!     ****** LBE/storage.f90
!
!     COPYRIGHT
!       (c) 2000-2008 by CASPUR/G.Amati
!     NAME
!       storage
!     DESCRIPTION
!       module for storage
!     INPUTS
!       none
!     OUTPUT
!       none
!     TODO
!
!     NOTES
!
!       integer variables defined:  l,n,l1,n1
!       real variables defined: lx, ly, dt, p0, p1, p2, rf, qf
!                               svisc, u0, omega,fgrad
!                               a02, a04, a05, a06, a11, a13, a14, a15, a19
!
!     *****
! =====================================================================
!
        module storage
!
        use real_kinds
        use mpi
!
        integer:: lx, ly, lz              ! global size        (MPI stuff)
        integer:: proc_x, proc_y, proc_z  ! task decomposition (MPI stuff)
!
        integer:: l, m, n                    ! local (task) size
        integer:: up(2),down(2),left(2)
        integer:: front(2),rear(2),right(2)
!
        integer, parameter::  mpid=3      ! mpi dimension
!
        integer:: nprocs, myrank, lbecomm, localcomm
        integer:: rear_task, front_task
        integer:: left_task, right_task
        integer:: down_task, up_task
        integer:: xyplane, xzplane, yzplane, myxrank, yzcomm
        integer:: prgrid(mpid)
        integer:: mpicoords(mpid)
        integer:: mydev, ndev              ! openacc variables
        integer, parameter::  zero=0
        integer, parameter::  uno=1
!
        integer(kind=MPI_OFFSET_KIND):: file_offset
!
        logical remdims(mpid)
        logical periodic(mpid)
        logical rreorder
!
#ifdef STEP10
        real(mystor), dimension(:,:,:), contiguous, pointer :: field1
        real(mystor), dimension(:,:,:), contiguous, pointer :: field1post 
        real(mystor), dimension(:,:,:), contiguous, pointer :: temp1 
        real(mystor), dimension(:,:,:), contiguous, pointer :: field2
        real(mystor), dimension(:,:,:), contiguous, pointer :: field2post 
        real(mystor), dimension(:,:,:), contiguous, pointer :: temp2 
        real(mystor), dimension(:,:,:), contiguous, pointer :: field3
        real(mystor), dimension(:,:,:), contiguous, pointer :: field3post 
        real(mystor), dimension(:,:,:), contiguous, pointer :: temp3 
!
        integer, dimension(:,:,:), allocatable :: mask
!
#else
        real(mystor), dimension(:,:,:), allocatable :: field1, temp1
        real(mystor), dimension(:,:,:), allocatable :: field2, temp2
        real(mystor), dimension(:,:,:), allocatable :: field3, temp3
#endif

!
        end module  storage
