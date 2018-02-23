FTN = ifort
OPT = -O2
DB= -g  
OMP= -qopenmp

OBJ = pi_mc.o ran1.o 

all: pi pi_omp

pi: $(OBJ)
	$(FTN) $(OBJ) main.o -o pi.x

pi_omp : $(OBJ)
	$(FTN) $(OMP) $(OBJ) main.o -o pi_omp.x

omp_db : main_db.o pi_mc.o ran1.o 
	$(FTN) $(DB) $(OMP) $(OBJ) main_db.o -o omp_db.x 

%.o: %.f90
	$(FTN) -c -g $< -o $@

clean: 
	rm *.o *.x