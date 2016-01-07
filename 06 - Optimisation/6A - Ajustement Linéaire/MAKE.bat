@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST SVD.exe DEL SVD.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    IF EXIST Sortie.dat DEL Sortie.dat
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
    gfortran SVD.f90 -o SVD liblapack.a libblas.a
    IF EXIST SVD.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST SVD.exe (
	SVD.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS