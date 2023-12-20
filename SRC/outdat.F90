! =====================================================================
!     ****** LBE/outdat
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       outdat
!     DESCRIPTION
!       write back simulation parameters
!     INPUTS
!       itfin    --> end of the run
!     OUTPUT
!       none
!     TODO
!       
!     NOTES
!       integer variables used: itfin
!
!     *****
! =====================================================================
!
      subroutine outdat(itfin)
!
      use storage
      implicit none
!
      INTEGER     :: itfin
      character*15 :: comms
!
      if(myrank==0) then 
!
#ifdef STEP3
         comms="STEP3-OpenACC"
#elif STEP2
         comms="STEP2"
#elif STEP1
         comms="STEP1"
#else
         comms="STEP0"
#endif
!
         write(6,*) ' '
         write(6,*) '*********** size of the lattice **************'
         write(6,*) 'lx (width x) =',lx
         write(6,*) 'ly (width y) =',ly
         write(6,*) 'lz (height)  =',lz
         write(6,*) '*********** decomposition *******************'
         write(6,*) 'proc_x       =',proc_x
         write(6,*) 'proc_y       =',proc_y
         write(6,*) 'proc_z       =',proc_z
         write(6,*) '*********** size of the task  ***************'
         write(6,*) 'l (width x)  =',l
         write(6,*) 'm (width y)  =',m
         write(6,*) 'n (height)   =',n
         write(6,*) '*********** run data ************************'
         write(6,*) 'itfin        =',itfin
         write(6,*) 'precision    =',MYMPIREAL
         write(6,*) '*********** implementation ******************'
         write(6,*) 'COMMS        =', comms
         write(6,*) '*********************************************'
      endif

#ifdef DEBUG_1
      if(myrank == 0) then
         write(6,*) "DEBUG1: Exiting from sub. outdat"
      endif
#endif

      end subroutine outdat
