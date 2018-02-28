#!/bin/bash 
#SBATCH --nodes=2 
#SBATCH --ntasks-per-node=20
#SBATCH --cpus-per-task=1
#SBATCH --time=00:50:00 
#SBATCH --verbose
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jlestz@princeton.edu 

srcdir=~/apc523/hw1/p1/src
rundir=~/apc523/hw1/p1/run

cd $rundir 

module load openmpi

srun -n 40 $srcdir/pi_mpi_scale.x > $rundir/out_pi_mpi_scale
