FTN = ifort
OMP = $(FTN) -qopenmp
MPI = mpifort
OPT = -O2
DB= -g  

all: pi_omp pi_mpi

pi_omp : ran1.o pi_omp.o main_omp.o
	$(OMP) $^ -o pi_omp.x

pi_mpi : ran1.o pi_mpi.o main_mpi.o 
	$(MPI) $^ -o pi_mpi.x

omp_db : ran1.o pi_omp.o main_db.o $(OBJ)
	$(OMP) $(DB) $^ -o omp_db.x 

mueller_mpi : mueller_mpi.o
	$(MPI) $^ -o mueller_mpi.x

pi_omp.o : pi_omp.f90 
	$(OMP) -c $^ -o $@

pi_mpi.o : pi_mpi.f90
	$(MPI) -c $^ -o $@

mueller_mpi.o : mueller_mpi.f90 
	$(MPI) -c $^ -o $@

%.o: %.f90
	$(FTN) -c $< -o $@

clean: 
	rm *.o *.x
