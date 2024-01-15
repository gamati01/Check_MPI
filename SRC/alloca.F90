!=====================================================================
!     ****** LBE/alloca
!
!     COPYRIGHT
!       (c) 2000-2011 by CASPUR/G.Amati
!       (c) 2013-20?? by CINECA/G.Amati
!     NAME
!       alloca
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
subroutine alloca()
      use storage; 
      use timing

      implicit none
!
      integer i,j,k
!
#ifdef STEP10
      allocate(field1(0:l+1,0:m+1,0:n+1))
      allocate(field2(0:l+1,0:m+1,0:n+1))
      allocate(field3(0:l+1,0:m+1,0:n+1))
!      
      allocate(field1post(0:l+1,0:m+1,0:n+1))
      allocate(field2post(0:l+1,0:m+1,0:n+1))
      allocate(field3post(0:l+1,0:m+1,0:n+1))
!      
      allocate(mask(1:l,1:m,1:n))
!     
      temp1 => null()
      temp2 => null()
      temp3 => null()
#else
      allocate(field1(0:l+1,0:m+1,0:n+1))
      allocate(field2(0:l+1,0:m+1,0:n+1))
      allocate(field3(0:l+1,0:m+1,0:n+1))
!      
      allocate(temp1(0:l+1,0:m+1,0:n+1))
      allocate(temp2(0:l+1,0:m+1,0:n+1))
      allocate(temp3(0:l+1,0:m+1,0:n+1))
#endif
!
#ifdef DEBUG_1
        if(myrank == 0) then
           write(6,*) "DEBUG1: Exiting from sub. alloca"
        endif
#endif
!
# ifdef MEM_CHECK
        if(myrank == 0) then
           mem_stop = get_mem();
           write(6,*) "MEM_CHECK: after sub. alloca mem =", mem_stop
        endif
# endif

end subroutine alloca

