FTN = ifort
OMP = $(FTN) -qopenmp
MPI = mpifort
OPT = -O2
DB= -g  

all: pi_omp pi_omp_scale pi_mpi

test : test.f90 ran1.f90
	$(MPI) $^ -o test.x

# main executables 
pi_omp : ran1.o pi_omp.o main_omp.o
	$(OMP) $^ -o pi_omp.x

pi_omp_scale : ran1.o pi_omp.o main_omp_scale.o
	$(OMP) $^ -o pi_omp_scale.x

pi_mpi : ran1.o pi_mpi.o main_mpi.o 
	$(MPI) $^ -o pi_mpi.x

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

# generic object 
%.o: %.f90
	$(FTN) -c $< -o $@

clean: 
	rm *.o *.x

# misc other
omp_db : ran1.o pi_omp.o main_db.o $(OBJ)
	$(OMP) $(DB) $^ -o omp_db.x 

mueller_mpi : mueller_mpi.o
	$(MPI) $^ -o mueller_mpi.x

mueller_mpi.o : mueller_mpi.f90 
	$(MPI) -c $^ -o $@
