!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none
  integer, parameter :: R = 8
end module KINDS

module DEFINE
  use kinds
  real(kind=R),allocatable :: DATA(:)
end module


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE INTERFACES
module INTERFACES
    interface
        function FCT1(X)
          use KINDS
          real(kind = R)             :: FCT1
          real(kind = R), intent(IN) :: X
        end function FCT1
        function FCT2(X)
          use KINDS
          real(kind = R)             :: FCT2
          real(kind = R), intent(IN) :: X
        end function FCT2
        function FCT3(X)
          use KINDS
          real(kind = R)             :: FCT3
          real(kind = R), intent(IN) :: X
        end function FCT3
    end interface
end module INTERFACES


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program INTEGRATIN
  use KINDS
  use forsythe, only : quanc8
  use INTERFACES
  use nrtype
  use DEFINE
  use IntegrationPack, only: Integration, Gauss
  implicit none

  real(kind = R) :: XMIN = 0._r, XMAX = 1._r, EABS = 0._r, EREL = 0._r, RSLT
  real(kind = R) :: EEST, FLAG
  integer        :: NEVL, INDX, IVAL

    call quanc8(FCT1, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)

    write(*,*) 'f(x) = 1/((x-0.3)**2+0.01) + 1/((x-0.9)**2+0.04) - 6 '
    write(*,'(A37,1X,E11.3)') 'Resultat                            :' , RSLT
    write(*,'(A37,1X,E11.3)') 'Erreur absolue                      :' , EEST
    write(*,'(A37,1X,I5)')    'Nombre d''evaluations de la fonction :', NEVL

    write(*,*) 'Evaluation en fonction de l''erreur relative'
    do INDX = 1,16
        EREL = 10._r**(-INDX)
        call quanc8(FCT1, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)
        write(*,*) INDX, RSLT, EEST, NEVL
    enddo

    write(*,*) 'Evaluation en fonction de l''erreur absolue'
    EREL = 0._r
    do INDX = 1,16
        EABS = 10._r**(-INDX)
        call quanc8(FCT1, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)
        write(*,*) INDX, RSLT, EEST, NEVL
    enddo

    EABS = 0._r
    do INDX = 1,3
        EREL = 10._r**(1-5*INDX)
        allocate(DATA(0))
        call quanc8(FCT1, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)
        open(100, file = "OUTPUT"//achar(48+INDX)//".dat")
        do IVAL = 1,NEVL
            write(100,*) DATA(2*IVAL-1:2*IVAL)
        enddo
        close(100)
        deallocate(DATA)
    enddo

    write(*,*) 'sin(x)/x'

    EABS = 0._r
    XMAX = 2._r
    do INDX = 1,16
        EREL = 10._r**(-INDX)
        call quanc8(FCT2, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)
        write(*,*) INDX, RSLT, EEST, NEVL, FLAG
        write(*,*) INDX, NEVL, RSLT, Integration(FCT2,XMIN,XMAX,NEVL), Gauss(FCT2,XMIN,XMAX,NEVL)
    enddo

    write(*,*) 'tan(x)/x'

    do INDX = 1,16
        EREL = 10._r**(-INDX)
        call quanc8(FCT3, XMIN, XMAX, EABS, EREL, RSLT, EEST, NEVL, FLAG)
        write(*,*) INDX, RSLT, EEST, NEVL, FLAG
        write(*,*) INDX, NEVL, RSLT, Integration(FCT3,XMIN,XMAX,NEVL), Gauss(FCT3,XMIN,XMAX,NEVL)
    enddo

end program INTEGRATIN


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION FCT1
function FCT1(X) 
  use KINDS
  use DEFINE
  implicit none

  real(kind = R)             :: FCT1
  real(kind = R), intent(IN) :: X

    FCT1 = 1/((X-0.3)**2+0.01) + 1/((X-0.9)**2+0.04) - 6
    if(allocated(DATA)) DATA = [DATA,X,FCT1]
end function FCT1


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION FCT2
function FCT2(X)
  use KINDS
  implicit none

  real(kind = R)             :: FCT2
  real(kind = R), intent(IN) :: X

    if(X == 0._r) then
        FCT2 = 1._r
    else
        FCT2 = sin(X)/X
    endif
end function FCT2


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION FCT3
function FCT3(X)
  use KINDS
  implicit none

  real(kind = R) :: FCT3
  real(kind = R), intent(IN) :: X

    if(X == 0._r) then
        FCT3 = 1._r
    else
        FCT3 = tan(X)/X
    end if
end function FCT3
