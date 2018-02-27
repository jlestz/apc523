#!/bin/bash 
#SBATCH --nodes=1 
#SBATCH --ntasks-per-node=10
#SBATCH --cpus-per-task=1
#SBATCH --time=00:30:00 
#SBATCH --verbose
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jlestz@princeton.edu 

srcdir=~/apc523/hw1/p1/src
rundir=~/apc523/hw1/p1/run

cd $rundir 

module load openmpi

srun -n 10 $srcdir/pi_mpi.x > $rundir/out_pi_mpi
