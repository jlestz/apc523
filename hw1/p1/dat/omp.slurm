#!/bin/bash 
#SBATCH --nodes=1 
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=20
#SBATCH --time=00:30:00 
#SBATCH --verbose
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jlestz@princeton.edu 

srcdir=~/apc523/hw1/p1/src
rundir=~/apc523/hw1/p1/run

cd $rundir 

export OMP_NUM_THREADS=20
$srcdir/pi_omp.x > $rundir/out_pi_omp
