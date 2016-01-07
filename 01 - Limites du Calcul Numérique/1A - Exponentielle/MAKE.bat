@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST EXPONENTIELLE.exe DEL EXPONENTIELLE.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    IF EXIST sortie.dat DEL sortie.dat
    IF EXIST graphe.dat DEL graphe.dat
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
    gfortran -c kinds.f90
    gfortran -c EXPONENTIELLE.f90
    gfortran -o EXPONENTIELLE *.o
    IF EXIST EXPONENTIELLE.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST EXPONENTIELLE.exe (
	EXPONENTIELLE.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS