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
	1 sendrecv per direction, and +/+ (3*1*2)
	hand-made packing/unpacking for x,y,z directions
