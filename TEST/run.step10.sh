#!/bin/tcsh
#
setenv DIR RUN_STEP10
setenv EXE mpi_gpuaware.step10.x
#
echo "---------------------------"
echo "starting test STEP 10      "
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
make STEP10=1  GPUENABLE=1
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
mpirun -np 8 ./$EXE >& out.log


echo "That's all folks!!!!" 
