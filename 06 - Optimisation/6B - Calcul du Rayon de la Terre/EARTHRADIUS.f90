!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE KINDS
module KINDS
  implicit none 
  integer, parameter :: R = 4
end module KINDS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULE COMMON_VALUES
module COMMON_VALUES                  
  use KINDS
  implicit none
  integer, save :: NMBR
  real(kind = R), allocatable, save :: CRDN(:,:)
end module COMMON_VALUES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! MODULES LIST_INTERFACES
module LIST_INTERFACES
    interface
        function FUNC(INPT)
          use nrtype
          use common_values
          use KINDS
          real(kind = R), intent(in) :: INPT(:)
          real(kind = R)             :: FUNC, ANGL(2*NMBR), ALPH(NMBR,NMBR)
          integer                    :: INDX, NEVL = 0
        end function FUNC
    end interface	
end module LIST_INTERFACES

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program EARTHRADIUS
  use KINDS
  use nr, only : powell
  use COMMON_VALUES
  use LIST_INTERFACES
  implicit none

  integer                     :: INDX
  real(kind = R)              :: FTOL = 1.E-4, FRET
  real(kind = R), allocatable :: DATA(:), IDTT(:,:)    

    open(200, file='INPUT/generalisation.dat')	
        read(200, *) NMBR
    close(200)

    allocate(IDTT(2*NMBR-2,2*NMBR-2), CRDN(NMBR,NMBR))

    IDTT = 0._r
    forall(INDX = 1:2*NMBR-2) IDTT(INDX,INDX) = 1._r

    open(100, file='INPUT/entrees.dat')
        read(100,*)
        read(100,*) CRDN
    close(100)

    DATA = [6000._r,(/(1._r, INDX=2,2*NMBR-2)/)]
    call powell(DATA, IDTT, FTOL, INDX, FRET)
    write(*,*) DATA(1)

end program EARTHRADIUS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! FONCTION FUNC
function FUNC(INPT)
  use nrtype
  use COMMON_VALUES
  use KINDS
  
  real(kind = R), intent(IN) :: INPT(:)
  real(kind = R)             :: FUNC, ANGL(2*NMBR), ALPH(NMBR,NMBR)
  integer                    :: INDX

    ANGL = [0._R, 0._R, INPT(2), 0._R, INPT(3:2*NMBR-2)]
    do INDX = 1,NMBR
        ALPH(INDX,1:INDX) = acos(sin(ANGL(1:2*INDX-1:2))*sin(ANGL(2*INDX-1))&
                           *cos(ANGL(2:2*INDX:2)-ANGL(2*INDX))&
                           +cos(ANGL(1:2*INDX-1:2))*cos(ANGL(2*INDX-1)))
        ALPH(1:INDX,INDX) = ALPH(INDX,1:INDX)
    enddo
    where(isnan(ALPH)) ALPH = 0._R
    FUNC = sum((CRDN - INPT(1)*ALPH)**2)

end function FUNC
