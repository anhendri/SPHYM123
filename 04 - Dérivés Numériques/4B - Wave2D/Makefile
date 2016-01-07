OPTS   = -fdollar-ok
LAPACK = -llapack -lblas
PGPLOT = -I%GWMODDIR% -Wl,--subsystem,console -lpgplot -lGrWin -mwindows
OPENMP = -fopenmp
MPI    = -I C:\MPICH2\include -L C:\MPICH2\lib -lmpi -lfmpich2g -lmpicxx
DEBUG  = -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace 
PROFIL = -pg

all:Wave2D.exe

Wave2D.exe:Wave2D.o

	gfortran -o Wave2D.exe *.o $(PGPLOT) $(DEBUG)

Wave2D.o:Map.mod

	gfortran Wave2D.f90 -c $(OPTS)

Map.mod:

	gfortran Map.f90 -c $(OPTS)
	
clean:

	del *.mod
	del *.o

