!=====================================================================
!     ****** LBE/boundaries
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       bcond
!     DESCRIPTION
!       Simple wrapper for boundaries routine...
!       - call bcond  else
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
        subroutine boundaries
!
        use timing
        use storage
!
        implicit none
        integer:: ierr, i
        real(mykind):: temp1, temp2
!
#ifdef DEBUG_2
        if(myrank == 0) then
           write(6,*) "DEBUG2: Entering in sub. boundaries"
        endif
#endif
!
! step 0
! simple sendrecv with datatype.
! 1 call for each field        
#ifdef STEP2
        call bcond_comm_step2
#elif STEP1
! not using MPI datatype
        call bcond_comm_step1
#else
! default (naive)
        call bcond_comm_step0
#endif

      end subroutine boundaries
