#!/bin/bash
# 
#SBATCH --nodes=2
#SBATCH --ntasks=8
#SBATCH --ntasks-per-node=4
#SBATCH --cpus-per-task=8
#SBATCH --gres=gpu:4
#SBATCH -p boost_usr_prod
#SBATCH -A cin_staff
##SBATCH --qos=boost_qos_bprod
#SBATCH -t 00:30:00 
#SBATCH --exclusive



mkdir $SLURM_JOBID
cd $SLURM_JOBID




