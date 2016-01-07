@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST *.exe DEL *.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
    gfortran -c nrtype.f90
    gfortran -c nr.f90
    gfortran -c EarthRadius.f90
    gfortran -c nrutil.f90
    gfortran -c linmin.f90
    gfortran -c mnbrak.f90
    gfortran -c brent.f90
    gfortran -c powell.f90
    gfortran -o RAYON *.o
    IF EXIST RAYON.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST RAYON.exe (
	RAYON.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS