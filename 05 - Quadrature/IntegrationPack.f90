! ----------------------------------------------------------------------------------------------
!
!   INTEGRATIONPACK contains a series of routines for the integration of a function f(x)
!                   between two points a and b
! 
!   To use any routine of this module, insert
!
!       use IntegrationPack
!
!   in your calling program.
!
!   Author: Alexandre Mayer
!           Facultés Universitaires Notre-Dame de la Paix, Namur, Belgium
!           http://perso.fundp.ac.be/~amayer
!           
! ----------------------------------------------------------------------------------------------
      Module IntegrationPack

      integer,parameter,private :: r = 8

      CONTAINS

! ----------------------------------------------------------------------------------------------
! ------- Function Trapeze ---------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   TRAPEZE integrates a function f(x) from point x=a to point x=b using the method of trapezes
!           with a step h=(b-a)/n
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Trapeze (f,a,b,n)

      implicit none

      real(kind=r) :: Trapeze
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S, h
      integer :: i

      h = (b-a)/n

      S = 0._r
      do i=1,n-1
       S = S + f(a+i*h)
      end do
      Trapeze = h * (f(a)/2._r+S+f(b)/2._r)

      end Function Trapeze

! ----------------------------------------------------------------------------------------------
! ------- Function Simpson ---------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   SIMPSON integrates a function f(x) from point x=a to point x=b using the method of Simpson
!           with a step h=(b-a)/n
!
!   Warning :
!
!           n must be a multiple of 2 to use this method (otherwise Trapeze will be used instead)
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Simpson (f,a,b,n)

      implicit none

      real(kind=r) :: Simpson
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S2, S4, h
      integer :: i

      if ((n/2)*2/=n) then
       Simpson = Trapeze(f,a,b,n)
       return
      endif

      h = (b-a)/n

      S4 = 0._r
      do i=1,n-1,2
       S4 = S4 + f(a+i*h)
      end do
      S2 = 0._r
      do i=2,n-2,2
       S2 = S2 + f(a+i*h)
      end do
      Simpson = (h/3._r)*(f(a)+4._r*S4+2._r*S2+f(b))

      end Function Simpson

! ----------------------------------------------------------------------------------------------
! ------- Function Bode ------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   BODE integrates a function f(x) from point x=a to point x=b using the method of Bode
!        with a step h=(b-a)/n
!
!   Warning :
!
!           n must be a multiple of 4 to use this method (otherwise Simpson will be used instead)
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Bode (f,a,b,n)

      implicit none

      real(kind=r) :: Bode
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S32, S12, S14, h
      integer :: i

      if ((n/4)*4/=n) then
       Bode = Simpson(f,a,b,n)
       return
      endif

      h = (b-a)/n

      S32 = 0._r
      do i=1,n-1,2
       S32 = S32 + f(a+i*h)
      end do
      S12 = 0._r
      do i=2,n-2,4
       S12 = S12 + f(a+i*h)
      end do
      S14 = 0._r
      do i=4,n-4,4
       S14 = S14 + f(a+i*h)
      end do

      Bode = (2._r*h/45._r)*(7._r*f(a)+32._r*S32+12._r*S12+14._r*S14+7._r*f(b))

      end Function Bode

! ----------------------------------------------------------------------------------------------
! ------- Function Bart ------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   BART integrates a function f(x) from point x=a to point x=b using the method of Bart
!        with a step h=(b-a)/n
!
!   Warning :
!
!           n must be a multiple of 8 to use this method (otherwise Bode will be used instead)
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Bart (f,a,b,n)

      implicit none

      real(kind=r) :: Bart
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S1024, S352, S436, S434, h
      integer :: i

      if ((n/8)*8/=n) then
       Bart = Bode(f,a,b,n)
       return
      endif

      h = (b-a)/n

      S1024 = 0._r
      do i=1,n-1,2
       S1024 = S1024 + f(a+i*h)
      end do
      S352 = 0._r
      do i=2,n-2,4
       S352 = S352 + f(a+i*h)
      end do
      S436 = 0._r
      do i=4,n-4,8
       S436 = S436 + f(a+i*h)
      end do
      S434 = 0._r
      do i=8,n-8,8
       S434 = S434 + f(a+i*h)
      end do

      Bart = (4._r*h/2835._r) &
           * (217._r*f(a)+1024._r*S1024+352._r*S352+436._r*S436+434._r*S434+217._r*f(b))

      end Function Bart

! ----------------------------------------------------------------------------------------------
! ------- Function Lisa ------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   LISA integrates a function f(x) from point x=a to point x=b using the method of Lisa
!        with a step h=(b-a)/n
!
!   Warning :
!
!           n must be a multiple of 16 to use this method (otherwise Bart will be used instead)
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Lisa (f,a,b,n)

      implicit none

      real(kind=r) :: Lisa
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S131072, S44032, S55456, S55116, S55118, h
      integer :: i

      if ((n/16)*16/=n) then
       Lisa = Bart(f,a,b,n)
       return
      endif

      h = (b-a)/n

      S131072 = 0._r
      do i=1,n-1,2
       S131072 = S131072 + f(a+i*h)
      end do
      S44032 = 0._r
      do i=2,n-2,4
       S44032 = S44032 + f(a+i*h)
      end do
      S55456 = 0._r
      do i=4,n-4,8
       S55456 = S55456 + f(a+i*h)
      end do
      S55116 = 0._r
      do i=8,n-8,16
       S55116 = S55116 + f(a+i*h)
      end do
      S55118 = 0._r
      do i=16,n-16,16
       S55118 = S55118 + f(a+i*h)
      end do

      Lisa = (8._r*h/722925._r) &
           * (27559._r*f(a)+131072._r*S131072+44032._r*S44032+55456._r*S55456 &
             +55116._r*S55116+55118._r*S55118+27559._r*f(b))

      end Function Lisa

! ----------------------------------------------------------------------------------------------
! ------- Function Maggie ----------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   MAGGIE integrates a function f(x) from point x=a to point x=b using the method of Lisa
!          with a step h=(b-a)/n
!
!   Warning :
!
!           n must be a multiple of 32 to use this method (otherwise Lisa will be used instead)
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Maggie (f,a,b,n)

      implicit none

      real(kind=r) :: Maggie
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: S67108864, S22413312, S28349440, S28163936, S28165300, S28165298, h
      integer :: i

      if ((n/32)*32/=n) then
       Maggie = Lisa(f,a,b,n)
       return
      endif

      h = (b-a)/n

      S67108864 = 0._r
      do i=1,n-1,2
       S67108864 = S67108864 + f(a+i*h)
      end do
      S22413312 = 0._r
      do i=2,n-2,4
       S22413312 = S22413312 + f(a+i*h)
      end do
      S28349440 = 0._r
      do i=4,n-4,8
       S28349440 = S28349440 + f(a+i*h)
      end do
      S28163936 = 0._r
      do i=8,n-8,16
       S28163936 = S28163936 + f(a+i*h)
      end do
      S28165300 = 0._r
      do i=16,n-16,32
       S28165300 = S28165300 + f(a+i*h)
      end do
      S28165298 = 0._r
      do i=32,n-32,32
       S28165298 = S28165298 + f(a+i*h)
      end do

      Maggie = (16._r*h/739552275._r) &
             * (14082649._r*f(a)      &
               +67108864._r*S67108864 &
               +22413312._r*S22413312 &
               +28349440._r*S28349440 &
               +28163936._r*S28163936 &
               +28165300._r*S28165300 &
               +28165298._r*S28165298 &
               +14082649._r*f(b))

      end Function Maggie

! ----------------------------------------------------------------------------------------------
! ------- Function Integration -----------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   INTEGRATION integrates a function f(x) from point x=a to point x=b using the best combination
!               of the methods of trapezes, Simpson, Bode, Bart, Lisa and Maggie
!               with a step h=(b-a)/n
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!                    n determines the integration step h=(b-a)/n
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Integration (f,a,b,n)

      implicit none

      real(kind=r) :: Integration
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r) :: h, a$, b$
      integer :: n$, nb

      a$ = a ; n$ = n ; h = (b-a)/n

      Integration = 0._r

      nb = n$/32
      if (nb>0) then
       b$ = min(a$+nb*32*(b-a)/n,b)
       Integration = Integration + Maggie (f,a$,b$,nb*32)
       a$ = b$ ; n$ = n$ - nb * 32
      endif
      
      nb = n$/16
      if (nb>0) then
       b$ = min(a$+nb*16*(b-a)/n,b)
       Integration = Integration + Lisa (f,a$,b$,nb*16)
       a$ = b$ ; n$ = n$ - nb * 16
      endif

      nb = n$/8
      if (nb>0) then
       b$ = min(a$+nb*8*(b-a)/n,b)
       Integration = Integration + Bart (f,a$,b$,nb*8)
       a$ = b$ ; n$ = n$ - nb * 8
      endif

      nb = n$/4
      if (nb>0) then
       b$ = min(a$+nb*4*(b-a)/n,b)
       Integration = Integration + Bode (f,a$,b$,nb*4)
       a$ = b$ ; n$ = n$ - nb * 4
      endif

      nb = n$/2
      if (nb>0) then
       b$ = min(a$+nb*2*(b-a)/n,b)
       Integration = Integration + Simpson (f,a$,b$,nb*2)
       a$ = b$ ; n$ = n$ - nb * 2
      endif

      if (n$>0) then
       Integration = Integration + Trapeze (f,a$,b,n$)
      endif

      end Function Integration

! ----------------------------------------------------------------------------------------------
! ------- Function GAUSS -----------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   GAUSS integrates a function f(x) from point x=a to point x=b using the method of Gauss
!         with n points
!
!   Inputs :
!        - f         (real double precision) function of a (real double precision) argument
!                    the function f(x) to be integrated
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!        - n         (integer)
!                    the number of points used for the application of the method
!
!   Function return value :  (real double precision)
!                            the value of the integral
!
! ----------------------------------------------------------------------------------------------
      Function Gauss (f,a,b,n)

      implicit none

      real(kind=r) :: Gauss
      integer,intent(in) :: n
      real(kind=r),intent(in) :: a, b
      interface
       Function f(x)
        real(kind=8) :: f
        real(kind=8),intent(in) :: x
       end Function f
      end interface

      real(kind=r),dimension(:),allocatable :: x, w
      integer :: i
      
      allocate(x(1:n),w(1:n))

      call GaussFactors (a,b,x,w)

      Gauss = 0._r
      do i = 1, n
       Gauss = Gauss + w(i) * f(x(i))
      enddo

      deallocate(x,w)

      end Function Gauss

! ----------------------------------------------------------------------------------------------
! ------- Subroutine GaussFactors --------------------------------------------------------------
! ----------------------------------------------------------------------------------------------
!
!   GAUSSFACTORS computes the abscisses x(i) and weights w(i) for the integration
!   of a function f(x) on the interval [a,b] using the method of Gauss.
!
!   The result of this integration is then given by sum_{i=1}^{n} w(i) f(x(i)),
!   where n is the number of points used for the integration (its value
!   is detected from the dimension of the arrays x and w).
!
!   Default usage: call GaussFactors ( a, b, x, w )
!
!   Inputs :
!        - a         (real double precision)
!                    lower bound of the interval of integration
!        - b         (real double precision)
!                    upper bound of the interval of integration
!
!   Outputs :
!        - x(:)      (real double precision)
!                    array that contains the abscisses x(i)
!        - w(:)      (real double precision)
!                    array that contains the weights w(i)
!
!   This procedure relies on the routine gauleg of the Numerical Recipes
!
! ----------------------------------------------------------------------------------------------
      Subroutine GaussFactors (a,b,x,w)

      real(kind=8),intent(in) :: a, b
	real(kind=8),dimension(:),intent(out) :: x,w

      call gauleg (a,b,x,w)

      end Subroutine GaussFactors

      Subroutine gauleg (x1,x2,x,w)

      IMPLICIT NONE
      integer,parameter :: DP=8, I4B=4
      REAL(DP), PARAMETER :: PI_D=3.141592653589793238462643383279502884197_dp
      REAL(DP), INTENT(IN) :: x1,x2
      REAL(DP), DIMENSION(:), INTENT(OUT) :: x,w
      REAL(DP), PARAMETER :: EPS=3.0e-14_dp
      INTEGER(I4B) :: its,j,m,n
      INTEGER(I4B), PARAMETER :: MAXIT=10
      REAL(DP) :: xl,xm
      REAL(DP), DIMENSION((size(x)+1)/2) :: p1,p2,p3,pp,z,z1
      LOGICAL, DIMENSION((size(x)+1)/2) :: unfinished
      n=size(x)
      m=(n+1)/2
      xm=0.5_dp*(x2+x1)
      xl=0.5_dp*(x2-x1)
      z=cos(PI_D*(arth(1,1,m)-0.25_dp)/(n+0.5_dp))
      unfinished=.true.
      do its=1,MAXIT
       where (unfinished)
        p1=1.0_dp
        p2=0.0_dp
       end where
       do j=1,n
        where (unfinished)
         p3=p2
         p2=p1
         p1=((2.0_dp*j-1.0_dp)*z*p2-(j-1.0_dp)*p3)/j
        end where
       end do
       where (unfinished)
        pp=n*(z*p1-p2)/(z*z-1.0_dp)
        z1=z
        z=z1-p1/pp
        unfinished=(abs(z-z1) > EPS)
       end where
       if (.not. any(unfinished)) exit
      end do
      if (its == MAXIT+1) print *, "Warning : too many iterations in gauleg !"
      x(1:m)=xm-xl*z
      x(n:n-m+1:-1)=xm+xl*z
      w(1:m)=2.0_dp*xl/((1.0_dp-z**2)*pp**2)
      w(n:n-m+1:-1)=w(1:m)

      end Subroutine gauleg

      FUNCTION arth(first,increment,n)
      integer,parameter :: I4B=4
      INTEGER(I4B), PARAMETER :: NPAR_ARTH=16,NPAR2_ARTH=8
      INTEGER(I4B), INTENT(IN) :: first,increment,n
      INTEGER(I4B), DIMENSION(n) :: arth
      INTEGER(I4B) :: k,k2,temp
      if (n > 0) arth(1)=first
      if (n <= NPAR_ARTH) then
       do k=2,n
        arth(k)=arth(k-1)+increment
       end do
      else
       do k=2,NPAR2_ARTH
        arth(k)=arth(k-1)+increment
       end do
       temp=increment*NPAR2_ARTH
       k=NPAR2_ARTH
       do
        if (k >= n) exit
        k2=k+k
        arth(k+1:min(k2,n))=temp+arth(1:min(k,n-k))
        temp=temp+temp
        k=k2
       end do
      end if
      END FUNCTION arth

      end Module IntegrationPack
