!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! PROGRAMME PRINCIPAL
program CHAOS
#define mean(x) sum(x)/size(x)
  implicit none

  real(kind=8)              :: CSTE, XI8(2), A, B
  real(kind=8), allocatable :: DIFF(:,:), IVAL(:), DIF2(:)
  real(kind=4)              :: XI4(2)
  integer                   :: LIMT, INDX

    open(120, file = "INPUT.dat")
        read(120,*) CSTE, LIMT
    close(120)

    allocate(DIFF(2,0:LIMT))

    open(120, file = "OUTPUT.dat")
    open(121, file = "LAMBDA.dat")
    

    XI4 = [(1._4)/(3._4), (2._4)**(-3._4)]
    XI8 = [(1._8)/(3._8), (2._8)**(-3._8)]
    DIFF(:,0) = abs(XI8-XI4)
    write(120,"(1I2, 6E11.3)") 0, XI4, XI8, DIFF(:,0)

    do INDX = 1,LIMT
       XI4 = CSTE*XI4*((1._4)-XI4)
       XI8 = CSTE*XI8*((1._8)-XI8)
       DIFF(:,INDX) = abs(XI8-XI4)
       write(120,"(1I2, 6E12.4)") INDX, XI4, XI8, DIFF(:,INDX)
    end do

    DIF2 = DIFF(1,0:29)
    IVAL = real((/(INDX, INDX = 0,29)/),8)

    A = sum((IVAL-mean(IVAL))*(log(DIF2)-mean(log(DIF2))))/sum((IVAL-mean(IVAL))**2)
    B = mean(log(DIF2)) - A*mean(IVAL)

    write(121,"(1I2, 2E12.4)") (INDX-1, exp(A*IVAL(INDX)+B), DIF2(INDX), INDX = 1,30)
end program CHAOS
