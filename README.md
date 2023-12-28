# Check_MPI

Simple program to check performance of different approch

STEP0 (default)
	1 sendrecv per direction, field and +/+ (3*3*2)
	using MPI Type for x,y,z directions

STEP1
	CPU
	1 sendrecv per direction, field and +/+ (3*3*2)
	using MPI Type for y and z directions
	hand-made packing/unpacking for x direction

STEP2
	CPU
	1 sendrecv per direction, and +/- (3*1*2) per task
	hand-made packing/unpacking for x,y,z directions


STEP3
	GPU with openacc kernels
	1 sendrecv per direction, and +/+ (3*1*2) per task
	hand-made packing/unpacking for x,y,z directions

STEP4
	GPU with openacc kernels with CUDA-Aware 
	1 sendrecv per direction, and +/- (3*1*2) per task
	hand-made packing/unpacking for x,y,z directions
	
STEP5
STEP6
STEP7
STEP8
