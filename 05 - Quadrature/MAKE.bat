@ECHO off
CLS

:CLEAN
    IF NOT "%1" == "clean" IF NOT "%1" == "all" GOTO MAKE
    IF EXIST *.mod DEL *.mod
    IF EXIST Integration.exe DEL Integration.exe
    IF EXIST *.o DEL *.o
    IF EXIST *.f90~ DEL *.f90~
    ECHO Netoyage termine
    IF "%1" == "clean" goto END

:MAKE
    IF NOT "%1" == "" IF NOT "%1" == "all" GOTO RUN
rem    gfortran -c kinds.f90
    gfortran -c nrtype.f90
    gfortran -c forsythe.f90
    gfortran -c IntegrationPack.f90 -fdollar-ok
rem    gfortran -c func.f90
rem    gfortran -c func2.f90
rem    gfortran -c func3.f90
rem    gfortran -c interfaces.f90
    gfortran -c quanc8.f90
    gfortran -c integration.f90
    gfortran -o INTEGRATION *.o
    IF EXIST INTEGRATION.exe ECHO Compilation Terminee
    IF("%1"=="") GOTO END

:RUN
    IF NOT "%1"=="run" IF NOT "%1"=="all" GOTO END
    IF EXIST INTEGRATION.exe (
	INTEGRATION.exe
	ECHO Execution Terminee
	GOTO END
    )
    ECHO Application Introuvable

:END
PAUSE
CLS