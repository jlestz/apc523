FTN = ifort
DB= -g  -Wall 

OBJ_REAL = main_real.o fun_real.o 
OBJ_STAR = main_star.o fun_star.o 
OBJ_KIND = main_kind.o fun_kind.o

all: test_real test_star test_d0

test_real: $(OBJ_REAL) 
	$(FTN) $(OBJ_REAL) -o test_real.x 

test_star: $(OBJ_STAR) 
	$(FTN) $(OBJ_STAR) -o test_star.x 

test_kind: $(OBJ_KIND) 
	$(FTN) $(OBJ_KIND) -o test_kind.x 

%.o: %.f90
	$(FTN) -c $< -o $@

clean: 
	rm *.o *.x
