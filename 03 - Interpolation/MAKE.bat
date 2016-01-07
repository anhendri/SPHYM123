@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST INTERPOLATION.exe DEL INTERPOLATION.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    IF EXIST sortie.dat DEL sortie.dat
    IF EXIST sortie.ps DEL sortie.ps
    IF EXIST sortie2.dat DEL sortie2.dat
    IF EXIST sortie2.ps DEL sortie2.ps
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
rem    gfortran -c kinds.f90
rem    gfortran -c donnes_communes.f90
    gfortran -c nrtype.f90
    gfortran -c nr.f90
rem    gfortran -c func.f90
    gfortran -c golden.f90
    gfortran -c locate.f90
    gfortran -c nrutil.f90
    gfortran -c spline.f90
    gfortran -c splint.f90
    gfortran -c tridag.f90
rem    gfortran -c interfaces.f90
    gfortran -c interpolation.f90
    gfortran -o INTERPOLATION *.o
    IF EXIST INTERPOLATION.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST INTERPOLATION.exe (
	INTERPOLATION.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS