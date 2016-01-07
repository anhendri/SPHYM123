@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST JULIA.exe DEL JULIA.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    IF EXIST sortie.dat DEL sortie.dat
    IF EXIST sortie.ps DEL sortie.ps
    IF EXIST sortie2.dat DEL sortie2.dat
    IF EXIST sortie2.ps DEL sortie2.ps
    IF EXIST sortie3.dat DEL sortie3.dat
    IF EXIST sortie3.ps DEL sortie3.ps
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
    gfortran -c -cpp JULIA.f90
    gfortran -o JULIA *.o
    IF EXIST JULIA.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST JULIA.exe (
	JULIA.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS
