FTN = ifort
OMP = $(FTN) -qopenmp
MPI = mpifort
OPT = -O2
DB= -g  

all: pi_omp pi_omp_scale pi_mpi pi_mpi_scale

# main executables 
pi_omp : ran1.o pi_omp.o main_omp.o
	$(OMP) $^ -o pi_omp.x

pi_omp_scale : ran1.o pi_omp.o main_omp_scale.o
	$(OMP) $^ -o pi_omp_scale.x

pi_mpi : ran1.o pi_mpi.o main_mpi.o 
	$(MPI) $^ -o pi_mpi.x

pi_mpi_scale : ran1.o pi_mpi.o main_mpi_scale.o 
	$(MPI) $^ -o pi_mpi_scale.x

# OMP objects 
main_omp_scale.o : main_omp_scale.f90 
	$(OMP) -c $^ -o $@

main_omp.o : main_omp.f90 
	$(OMP) -c $^ -o $@

pi_omp.o : pi_omp.f90 
	$(OMP) -c $^ -o $@

# MPI objects 
pi_mpi.o : pi_mpi.f90
	$(MPI) -c $^ -o $@

main_mpi.o : main_mpi.f90 
	$(MPI) -c $^ -o $@

main_mpi_scale.o : main_mpi_scale.f90 
	$(MPI) -c $^ -o $@

# generic object 
%.o: %.f90
	$(FTN) -c $< -o $@

clean: 
	rm *.o *.x
