@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    DEL *.mod
    IF EXIST KIRCHHOFF.exe DEL KIRCHHOFF.exe
    DEL *.o
    DEL *.f90~
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
rem    gfortran -c kinds.f90
    gfortran -c nrtype.f90
    gfortran -c decomp.f90
    gfortran -c forsythe.f90
    gfortran -c solve.f90
    gfortran -c kirchhoff.f90
    gfortran -o KIRCHHOFF *.o
    IF EXIST KIRCHHOFF.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST KIRCHHOFF.exe (
	KIRCHHOFF.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS