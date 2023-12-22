#!/bin/tcsh
#
setenv DIR RUN_STEP3
setenv EXE mpi_gpuaware.step3.x
#
echo "---------------------------"
echo "starting test STEP 3       "
echo " ---> " $EXE
echo " ---> " $DIR
echo "---------------------------"
#
rm -rf $DIR
mkdir $DIR
cd $DIR

# step 1: compiling
echo "step 1: compiling"
cd ../../SRC
make clean
make STEP3=1 
if ($?) then
   echo "compiling fails..."
   exit 1
else
   cd -
   cp ../../RUN/$EXE .
   cp ../file.input .
   echo "compiling  ended succesfully..."
endif


# step 2: running test
echo "step 2: running test"
mpirun -np 4 ./$EXE >& out.log


echo "That's all folks!!!!" 
