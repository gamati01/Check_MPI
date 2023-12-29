!=======================================================================
!     ****** LBE/init
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       init
!     DESCRIPTION
!
!     INPUTS
!
!     OUTPUT
!
!     TODO
!
!     NOTES
!       integer variables used: i,j,k
!                           
!     *****
!=======================================================================
!
        subroutine init(border)
!
        use storage
        implicit none
!
        integer i,j,k,ierr
        integer border
!
        real(mykind) ::  x,y,z,xj,yj,zj,pi
!
        integer      :: nn, mm, ll
!
        parameter(pi=3.141592653589793238462643383279)
!
        do k = 0, n+1
           z = (2.0*pi*(float(k-1)/float(n)))
           do j = 0, m+1
              y = (2.0*pi*(float(j-1)/float(m)))
              do i = 0, l+1
                 x = (2.0*pi*(float(i-1)/float(l)))
                 field1(i,j,k) = sin(x)
                 field2(i,j,k) = sin(y)
                 field3(i,j,k) = sin(z)
              end do
           end do
        end do
!        
! check        
        if(myrank==0) then
           do i = 0, l+1
              write(66,*) i, field1(i,m/2,n/2)
           end do
        endif
!                 
        call mpi_barrier(lbecomm,ierr)
! 
#ifdef DEBUG_1
        if(myrank == 0) then
           write(6,*) "DEBUG1: Exiting from sub. init"
        endif
#endif
!
        end subroutine init
