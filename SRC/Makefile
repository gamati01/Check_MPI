#----------------------------------------------
# makefile per BGK
# Giorgio Amati, CASPUR 2000-2012
# Giorgio Amati, CINECA 2013-20??
#----------------------------------------------
# notes:
#
# 	1) works correctly using gmake
#	2) check on
#		Intel/AMD
#
#----------------------------------------------
# targets:
#
#		serial ---> serial code
#		openmp ---> serial + openmp
#               mpi    ---> mpi
#		all    ---> mpi + openmp
# 
# flags
#               DBL = -DDOUBLE_P (double precision, default)
#               DBL =            (single precision)
#               * NON BLOCK (or SENDRECV)
#               * DOMAIN DECOMPOSITION (or MULTIBLOCK)
#               * CHANNEL/DUCT/DRIVEN/COUETTE/NOSLIP
#               * FUSED (or ORIGINAL)
#               * PARALLEL (or SERIAL)
#               * FORCING_X (or FORCING_Y/FORCING_Z)
#               * RAW (or HDF5/MPIIO)
#
#               FERMI: FIX = -WF,-DDEBUG_1,-DDEBUG_2,-DFERMI_FIX
#               KNL: make hybrid "FIX = -qopt-report-phase=vec -xmic-avx512 -qopt-prefetch=5 -qopt-report=4 -DKNOWN_TRIP_COUNT"
#               KNL: make openmp KNL=1  " FIX=-assume contiguous_pointer -qopt-zmm-usage=high " 
#                Pointers --> FIX=-assume contiguous_pointer 
# #
# make serial FUGAKU=1 "FIX= -DFUGAKU -Knoalias=s -Kfast -mcpu=a64fx -Kassume=notime_saving_compilation
# 
# make serial FUGAKU=1 "FIX= -DFUGAKU -Knoalias=s -Kfast -mcpu=a64fx -Kassume=notime_saving_compilation -Kopenmp -Kopenmp_assume_norecurrence -Nfjomplib -Koptmsg=2 -Kswp_strong"
# 
# make FUGAKU=1 COMM=1 "FIX= -DFUGAKU -Knoalias=s -Kfast -mcpu=a64fx -Kassume=notime_saving_compilation  -Kopenmp_assume_norecurrence -Koptmsg=2 -Kswp_strong -DORIGINAL"
# 
#
#
#
#----------------------------------------------
#
DATE = $(shell date +%d%m%y)
#
# default
#----------------------------------------------
FINE = "********** Fine Compilazione *********"
#----------------------------------------------
#
#----------------------------------------------
# compiler section
#----------------------------------------------
# default (NVIDIAcompiler)
 CC = mpicc
 FC = mpifort
 FIX = -DPGI
 FOPT = -fast -Mcontiguous -Mnodepchk
 COPT = 
 OPT = -fast -Mcontiguous -Mnodepchk
#
ifdef GNU
 CC = mpicc
 FC = mpif90
 FOPT = -ftree-vectorize -Ofast
 COPT = -ftree-vectorize -Ofast
 OPT  = -ftree-vectorize -Ofast
endif
#
#----------------------------------------------
# version section
#----------------------------------------------
#
VER = step0
ifdef STEP1
 FIX=-DSTEP1
 VER=step1
endif
#
ifdef STEP2
 FIX=-DSTEP2
 VER=step2
endif
#
ifdef STEP3
 FIX=-DSTEP3 -acc -Minfo=acc -DOPENACC -DFAST
 VER=step3
endif
#
ifdef STEP4
 FIX=-DSTEP4 -acc -Minfo=acc -DOPENACC -DFAST
 VER=step4
endif
#
ifdef STEP5
 FIX=-DSTEP5 -acc -Minfo=acc -DOPENACC -DFAST
 VER=step5
endif
#
ifdef STEP6
 FIX=-DSTEP6 -acc -Minfo=acc -DOPENACC -DFAST
 VER=step6
endif
#
#----------------------------------------------
# Makefile section
#----------------------------------------------
MOD  = get_mem_mod.o \
       precision.o \

OBJ  = storage.o \
       mod_tools.o \
       input.o \
       alloca.o \
       init.o \
       do_somethingGPU.o \
       finalize.o \
       mpi_gpuaware.o \
       boundaries.o \
       bcond.comm.step0.o \
       bcond.comm.step1.o \
       bcond.comm.step2.o \
       bcond.comm.step3.o \
       bcond.comm.step4.o \
       bcond.comm.step5.o \
       bcond.comm.step6.o \
       setup_MPI.o \
       prof_i.o \
       prof_j.o \
       prof_k.o \
       outdat.o \

#----------------------------------------------
#
all: $(MOD) $(OBJ) 
	$(FC) $(OPT) $(OBJ) $(FIX) $(TRY) $(LIB) -o ../RUN/mpi_gpuaware.$(VER).x 
	@echo $(FINE)

#----------------------------------------------

%.o %.mod: $(INC) %.f90
	$(FC) $(FOPT) $(DBL) $(PREPROC) $(FIX) $(TRY) -c $<

%.o: $(INC) %.F90
	$(FC) $(FOPT) $(DBL) $(PREPROC) $(FIX) $(TRY) -c $<

%.o: $(INC) %.f
	$(FC) $(FOPT) $(DBL) $(PREPROC) $(FIX) $(TRY) c $<

%.o: $(INC) %.c
	$(CC) $(COPT)                          -c $<

%.p: %.f90
	$(FC) $(FOPT) $(DBL) $(PREPROC) $(FIX) $(TRY) $<

#----------------------------------------------
clean:
	rm -f *.o;
	rm -f *.mod;
	rm -f core;
#----------------------------------------------
info:  $(INC)
	@echo "Modules          =  "$(MOD);
	@echo "Objects          =  "$(OBJ);
	@echo "Compiler         =  "$(FC);
	@echo "Compiler flags   =  "$(OPT);
	@echo "Preproc  flags   =  "$(PREPROC);
	@echo "Fix      flags   =  "$(FIX);
	@echo "Try      flags   =  "$(TRY);
	@echo "Double   flags   =  "$(DBL);
	@echo "Architecture     =  "$(HOSTTYPE);
	@echo "Version          =  "$(VER);
	@echo "Libraries        =  "$(LIB);

#----------------------------------------------
tar:
	make clean
	rm -rf ../RUN/*.vtk
	rm -rf ../RUN/*.bin
	rm -rf ../RUN/*.h5
	tar -cvf ../../archive.mpi_gpuaware.$(DATE).tar ../../MPI_GPUAWARE/*
#----------------------------------------------
help:
	@echo "mpi (default)"
#----------------------------------------------

