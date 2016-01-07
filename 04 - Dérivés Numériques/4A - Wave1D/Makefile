OPTS   = -fdollar-ok
LAPACK = -llapack -lblas
PGPLOT = -I%GWMODDIR% -Wl,--subsystem,console -lpgplot -lGrWin -mwindows
OPENMP = -fopenmp
MPI    = -I C:\MPICH2\include -L C:\MPICH2\lib -lmpi -lfmpich2g -lmpicxx
DEBUG  = -fcheck=all -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace 
PROFIL = -pg

all:Wave1D.exe

Wave1D.exe:Wave1D.o

	gfortran -o Wave1D.exe *.o $(PGPLOT)

Wave1D.o:Plot.mod

	gfortran Wave1D.f90 -c $(OPTS)

Plot.mod:

	gfortran Plot.f90 -c $(OPTS)
	
clean:

	del *.mod
	del *.o

